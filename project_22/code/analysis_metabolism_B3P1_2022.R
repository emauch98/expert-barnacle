################################################################################
# This is a metabolism analysis of pond B3P1
# Treatment B3P1: Fish (Sticklebacks)
# Observation period used to estimate metabolism of B3P1:
# 2022-06-24 00:00:00 to 2022-07-02 00:00:00
# Model used: Bayesian
################################################################################

# Step 0: set up R-script ------------------------------------------------------
rm(list= ls())

# Packages ad functions from Moritz Luehrig's paper
source("../../Literature/Moritz_Luehrig_paper_stuff/methods_packages.R")

# Estimating lake metabolism with free-water oxygen
# See "report_analysis_metabolism_B3P3_2022" for further details
require(LakeMetabolizer)

# For plotting
require(GGally)

# Imputation of NAs for time-series (our NAs are due to outlier processing)
require(imputeTS)

# For Bayesian lake metabolism modeling
require(R2jags) # Also, installing JAGS is necessary under
# http://www.sourceforge.net/projects/mcmc-jags/files

require(ggpubr)
require(grid) # Arranging multiple plots
require(grDevices) # Saving png files

# This is our complete dataset of the sondes measurements 2022
# Measurement Interval: 2 minutes
all <- fread("../data/processed_data_sondes/ponds_sonde_data_all_uncut.txt")

# Importing the metadata
# Wind and irradiation
# Measurement Interval: 1 hour, hourly averages
# Source: QAS_L automated weather station near Narsarsuaq
# https://dataverse.geus.dk/dataset.xhtml?persistentId=doi:10.22008/FK2/IW73UU
# wind sensor height: ca. 2.87m
meta <- fread("../data/metadata_metabolism_analysis/metadata_2022.txt")

# Step 1: data formatting ------------------------------------------------------

# Select columns to keep from our time series
all <- all[, c("Pond", "Date_time", "ODO_mgL", "Temp_C")]

# Select Pond B1P1 (Treatment: Fish)
all <- subset(all, Pond == "B3P1")

# Rename
all <- setnames(all,c("Pond", "datetime", "do.obs", "wtr"))

# Isolate the desired period (described in the introduction)
all <- all[c(463:6222),]

# choose desired period for the metadata
meta <- meta[c(25:217),]

# rename
meta <- setnames(meta,c("datetime", "dsr_cor", "wsp"))

meta <- meta %>%
  mutate(datetime = as.POSIXct(format(datetime, format = "%Y-%m-%d %H:01:00", tz = "UTC"), tz = "UTC"))

# Create a new inflated dataset with measurements every 2 minutes
# The hourly mean will be assigned to every observation (2min-interval) within that hour
meta_ext <- meta %>%
  complete(datetime = seq(from = min(datetime), to = max(datetime), by = "2 min"))

# Fill the missing wind speed and irradiation values with the corresponding hourly values using 'fill'
meta_ext <- meta_ext %>%
  fill(wsp, .direction = "up") %>%
  fill(dsr_cor, .direction = "up")

# Shift wind speed and irradiation one row up
meta_ext <- meta_ext %>%
  mutate(wsp = lead(wsp, default = last(wsp))) %>%
  mutate(dsr_cor = lead(dsr_cor, default = last(dsr_cor)))

# Ommit last row (not included in our timeseries)
meta_ext <- meta_ext[-nrow(meta_ext), ]

# Reset row names for the final result
rownames(meta_ext) <- NULL

# Step 2: merge the 2 datasets -------------------------------------------------
all <- merge(meta_ext, all, by = "datetime")

# Step 3: Missing data imputation ----------------------------------------------
# We need a complete ODO_mgL timeseries. Therefore we will impute the missing values
# here, with the package "imputeTS", which is specifically designed to impute time-series
# observations.

colSums(is.na(all))

# Make a timeseries object
ts <- ts(all$do.obs, frequency = 720)

# Impute with imputeTS
imp_all <- na_ma(ts) # use this function!
imp_all <- data.table(imp_all)
imp_all$datetime <- all$datetime

# Check for missing values in the imputed dataset
sum(is.na(imp_all)) # No missing values anymore

# Check imputation by plotting the original and the imputed dataset
plot(x = all$datetime, y = all$do.obs, type = "l", col = "blue", lwd = 2)
lines(x = imp_all$datetime, y = imp_all$imp_all, type = "l",
      col = rgb(1, 0, 0, alpha = 0.3), lwd = 2)
# The imputation worked very well!

# Replace original column with imputed column
all$do.obs <- imp_all$imp_all
sum(is.na(all))

# Step 4: Model choice ---------------------------------------------------------
# Gas transfer coefficient model: kvachon()
# - needs wind speed, sensor height, and lake area

# Lake metabolism model: metab.bayesian()
# Most flexible model

# Step 5: Calculate k600 -------------------------------------------------------

# This function needs specific column names
all$wnd <- all$wsp
all$wsp <- NULL

# Normalize wind to 10m sensor height
wind.scale <- wind.scale(all, wnd.z = 2.87)

all <- merge(all, wind.scale, by = "datetime")

all$wnd <- all$wnd_10
all$wnd_10 <- NULL

# Also only the 2 columns of interest, otherwise it gets confused
all.600 <- all[,c("datetime","wnd")]
k600.vachon <- k.vachon(all.600, lake.area = 4000)

# Merge
all <- merge(all, k600.vachon, by = "datetime")

# Step 6: Calculate gas-specific k600 ------------------------------------------

kgas <- k600.2.kGAS(all)

# Merge
all <- merge(all, kgas, by = "datetime")

# only keep k.gas
all$k600 <- NULL

# Step 7: Calculate dissolved oxygen saturated (do.sat) ------------------------

# I know that the sonde has do.sat measurements, but I have more faith
# in the authors of this paper to calculate a more accurate do.sat then the
# sonde-intern calculations.

o2.sat <- o2.at.sat(all[,c('datetime','wtr')])

# Merge
all <- merge(all, o2.sat, by = "datetime")

# Step 8: Calculate Actively mixed layer depth (z.mix) -------------------------

################################################################################
# The max depth of pond B3P1 is about 2m.
# According to the profiling plots, B3P1 had an actively mixed layer depth of
# about 1.75 on June 20, 20222
# Since mixed layer depth is highly dependent on weather, I'll assume an
# average mixed layer depth of 1.5m
################################################################################

# Set Actively mixed layer depth to 2.5m
all$z.mix <- 1.5

# Step 9: Derive photosynthetically active radiation (par) from shortwave irradiation (sw) ----
# Here, I take the downwelling shortwave irradiation (tilt corrected) from the
# QAS_L automated weather station a couple of kilometers away from Narsarsuaq
# It's the closest weather station measuring this parameter, that I could find.

# Derive par from sw
par <- sw.to.par(all, sw.col = "dsr_cor")
rownames(par) <- NULL

# Merge
all <- merge(all, par[,c("datetime", "par")], by = "datetime")

# Step 10: Fit model -----------------------------------------------------------

# Choose columns to keep
all <- all[,c("datetime", "do.obs", "do.sat", "k.gas", "z.mix", "par", "wtr")]

# Bayesian
bayesian.res <- metab(all, method="bayesian", wtr.name='wtr', do.obs.name='do.obs',
                      irr.name='par', z.mix = "z.mix", k.gas.name = "k.gas",
                      do.sat.name = "do.sat")

bayesian.sd <- attr(bayesian.res, "metab.sd")

# Create a date variable
bayesian.res$Date <- as.Date(paste(bayesian.res$year, "-", bayesian.res$doy, sep = ""), format = "%Y-%j")

# Select columns to keep
bayesian.res <- bayesian.res[,c("Date", "GPP", "R", "NEP")]

# Create a date variable
bayesian.sd$Date <- as.Date(paste(bayesian.sd$year, "-", bayesian.sd$doy, sep = ""), format = "%Y-%j")

# Select columns to keep
bayesian.sd <- bayesian.sd[,c("Date", "GPPsd", "Rsd", "NEPsd")]

bayesian.merge <- merge(bayesian.res, bayesian.sd, by = "Date")

fwrite(bayesian.merge, "../results_metabolism/B3P1_bayesian_2022.txt", sep="\t")

# Step 11: Visualize the results -----------------------------------------------

# Plots of ecosystem parameters-------------------------------------------------

p4 = ggplot((all)) + theme_light() +
  geom_line(aes(y=do.obs, x=datetime), color = "gray17") +
  rremove("xlab") +
  ylab(expression(paste("DO (mg L",NULL^-1,")")))

p5 = ggplot((all)) + theme_light() +
  geom_line(aes(y=do.sat, x=datetime), color = "gray17") +
  rremove("xlab") +
  ylab(expression(paste("Equilibrium\nDO concentration (mg L",NULL^-1,")")))

p6 = ggplot((all)) + theme_light() +
  geom_line(aes(y=wtr, x=datetime), color = "gray17") +
  rremove("xlab") +
  ylab("Temperature in °C")

p7 = ggplot((all)) + theme_light() +
  geom_line(aes(y=par, x=datetime), color = "gray17") +
  rremove("xlab") +
  ylab(expression(paste("PAR (",mu,"mol m",NULL^-2," s",NULL^-1,")")))

p8 = ggplot((all)) + theme_light() +
  geom_line(aes(y=k.gas, x=datetime), color = "gray17") +
  rremove("xlab") +
  ylab(expression(paste("Gas and temperature specific\ngas transfer coefficient (m",NULL^-1,")")))

fig2 <- ggarrange(p4, p5, p6, p7, p8,
                  nrow=5)

fig3 <- annotate_figure(fig2, top = text_grob("Parameters used to estimate metabolism of B3P1 in 2022"))

plot(fig3)

# Save the plot as a .png
png("../results_metabolism/parameters_B3P1_2022.png", width = 1200, height = 900)
print(fig3)
dev.off()

# Plots of metabolism-----------------------------------------------------------

# Plot
# GPP
p1 = ggplot(data = bayesian.merge) + theme_light() +
  geom_line(aes(y=GPP, x=Date)) +
  ylab(expression(paste("GPP (", O[2], " in mg L", NULL^-1, " day", NULL^-1, ")")))

# R
p2 = ggplot(data = bayesian.merge) + theme_light() +
  geom_line(aes(y=R, x=Date)) +
  ylab(expression(paste("R (", O[2], " in mg L", NULL^-1, " day", NULL^-1, ")")))

# NEP
p3 = ggplot(data = bayesian.merge) + theme_light() +
  geom_line(aes(y=NEP, x=Date)) +
  ylab(expression(paste("NEP (", O[2], " in mg L", NULL^-1, " day", NULL^-1, ")")))

fig1 <- ggarrange(p1 + rremove("xlab"), p2 + rremove("xlab"), p3 + rremove("xlab"),
                  nrow=3)

fig4 <- annotate_figure(fig1, top = text_grob("Estimated metabolism of pond B3P1 in 2022"))
plot(fig4)

# Save the plot as a .png
png("../results_metabolism/metabolism_B3P1_2022.png", width = 1200, height = 900)
print(fig4)
dev.off()
