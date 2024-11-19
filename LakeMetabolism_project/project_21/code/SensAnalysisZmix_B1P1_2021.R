################################################################################
# This script is meant to perform a simple sensitivity analysis of the
# Bayesian lake metabolism model with regards to mixed layer depth.
# Mixed layer depth is important for estimation of lake metabolism, since it
# defines the amount of water column
# getting into regular contact with the atmosphere and impacts the extend of air-water
# gas transfer of oxygen. Mixed layer depth is highly dependent on weather conditions,
# and hence regular profiles of at least temperature are normally necessary to
# estimate it. However, we do not have regular profiles for our pond experiment.
# Therefore, having a general idea of how strongly mixed layer depth impacts
# metabolism estimates is crucial.
# We use the example of B1P1 in 2021 for this sensitivity analysis.
# We will fit a model with mixed layer depth of 0.5, 1, 2, and 3 meters for this pond.
# The ponds maximum depth is 4m.
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

# This is our complete dataset of the sondes measurements 2021
# Measurement Interval: 2 minutes
all <- fread("../data/processed_data_sondes/ponds_sonde_data_all.txt")

# Importing the metadata
# Wind and irradiation
# Measurement Interval: 1 hour, hourly averages
# Source: QAS_L automated weather station near Narsarsuaq
# https://dataverse.geus.dk/dataset.xhtml?persistentId=doi:10.22008/FK2/IW73UU
# wind sensor height: 2.65 + 0.4 = ca 3.05m
meta <- fread("../data/metadata_metabolism_analysis/metadata_2021.txt")

# Step 1: data formatting ------------------------------------------------------

# Select columns to keep from our time series
all <- all[, c("Pond", "Date_time", "ODO_mgL", "Temp_C")]

# Select Pond B1P1 (Treatment: Fish)
all <- subset(all, Pond == "B1P1")

# Rename
all <- setnames(all,c("Pond", "datetime", "do.obs", "wtr"))

# Isolate the desired period (described in the introduction)
# Unfortunately, B1P1 had a huge data-gap so we can only estimate
# metabolism before (we have 4 complete days)
all <- all[c(121:3000),]

# choose desired period for the metadata
meta <- meta[c(1:97),]

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

# 113 NAs (due to outlier processing)
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

# Step 5: Calculate k600 using the Cole method ---------------------------------

# This function needs specific column names
all$wnd <- all$wsp
all$wsp <- NULL

# Normalize wind to 10m sensor height
wind.scale <- wind.scale(all, wnd.z = 3.05)

all <- merge(all, wind.scale, by = "datetime")

all$wnd <- all$wnd_10
all$wnd_10 <- NULL

# Also only the 2 columns of interest, otherwise it gets confused
all.600 <- all[,c("datetime","wnd")]
k600.vachon <- k.vachon(all.600, lake.area = 2100)

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

# Step 8: Derive photosynthetically active radiation (par) from shortwave irradiation (sw) ----
# Here, I take the downwelling shortwave irradiation (tilt corrected) from the
# QAS_L automated weather station a couple of kilometers away from Narsarsuaq
# It's the closest weather station measuring this parameter, that I could find.

# Derive par from sw
par <- sw.to.par(all, sw.col = "dsr_cor")
rownames(par) <- NULL

# Merge
all <- merge(all, par[,c("datetime", "par")], by = "datetime")

# Step 9: Calculate Actively mixed layer depth (z.mix) -------------------------

################################################################################
# The max depth of pond B1P1 is about 4m.
# According to the profiling plots, B1P1 had an actively mixed layer depth of
# about 2m on Sept 10 2021.
# We will fit a model with mixed layer depth of 0.5, 1, 2, and 3 meters for this pond.
# The ponds maximum depth is 4m.
################################################################################

# Set different mixed layer depths of sensitivity analysis
# For some reason, metab.bayesian does not recognize z.mix columns
# if they are not named "z.mix", hence I have to create a dataset for each z.mix
all2 <- all
all2$z.mix <- 0.5

all3 <-all
all3$z.mix <- 1

all4 <- all
all4$z.mix <- 2

all5 <- all
all5$z.mix <- 3

# Step 10: Fit model -----------------------------------------------------------

# Choose columns to keep
all2 <- all2[,c("datetime", "do.obs", "do.sat", "k.gas", "z.mix", "par", "wtr")]
all3 <- all3[,c("datetime", "do.obs", "do.sat", "k.gas", "z.mix", "par", "wtr")]
all4 <- all4[,c("datetime", "do.obs", "do.sat", "k.gas", "z.mix", "par", "wtr")]
all5 <- all5[,c("datetime", "do.obs", "do.sat", "k.gas", "z.mix", "par", "wtr")]

# Mixed layer depth 0.5m over the whole observation period
bayesian.res_0.5 <- metab(all2, method="bayesian", wtr.name='wtr', do.obs.name='do.obs',
                      irr.name='par', z.mix = "z.mix", k.gas.name = "k.gas",
                      do.sat.name = "do.sat")

# Mixed layer depth 1m over the whole observation period
bayesian.res_1 <- metab(all3, method="bayesian", wtr.name='wtr', do.obs.name='do.obs',
                        irr.name='par', z.mix = "z.mix", k.gas.name = "k.gas",
                        do.sat.name = "do.sat")

# Mixed layer depth 2m over the whole observation period
bayesian.res_2 <- metab(all4, method="bayesian", wtr.name='wtr', do.obs.name='do.obs',
                        irr.name='par', z.mix = "z.mix", k.gas.name = "k.gas",
                        do.sat.name = "do.sat")

# Mixed layer depth 3m over the whole observation period
bayesian.res_3 <- metab(all5, method="bayesian", wtr.name='wtr', do.obs.name='do.obs',
                        irr.name='par', z.mix = "z.mix", k.gas.name = "k.gas",
                        do.sat.name = "do.sat")

# Get standard deviations of estimates
bayesian.sd_0.5 <- attr(bayesian.res_0.5, "metab.sd")
bayesian.sd_1 <- attr(bayesian.res_1, "metab.sd")
bayesian.sd_2 <- attr(bayesian.res_2, "metab.sd")
bayesian.sd_3 <- attr(bayesian.res_3, "metab.sd")

# Create a date variable
bayesian.res_0.5$Date <- as.Date(paste(bayesian.res_0.5$year, "-", bayesian.res_0.5$doy, sep = ""), format = "%Y-%j")
bayesian.res_1$Date <- as.Date(paste(bayesian.res_1$year, "-", bayesian.res_1$doy, sep = ""), format = "%Y-%j")
bayesian.res_2$Date <- as.Date(paste(bayesian.res_2$year, "-", bayesian.res_2$doy, sep = ""), format = "%Y-%j")
bayesian.res_3$Date <- as.Date(paste(bayesian.res_3$year, "-", bayesian.res_3$doy, sep = ""), format = "%Y-%j")

# Select columns to keep
bayesian.res_0.5 <- bayesian.res_0.5[,c("Date", "GPP", "R", "NEP")]
bayesian.res_1 <- bayesian.res_1[,c("Date", "GPP", "R", "NEP")]
bayesian.res_2 <- bayesian.res_2[,c("Date", "GPP", "R", "NEP")]
bayesian.res_3 <- bayesian.res_3[,c("Date", "GPP", "R", "NEP")]

# Create a date variable
bayesian.sd_0.5$Date <- as.Date(paste(bayesian.sd_0.5$year, "-", bayesian.sd_0.5$doy, sep = ""), format = "%Y-%j")
bayesian.sd_1$Date <- as.Date(paste(bayesian.sd_1$year, "-", bayesian.sd_1$doy, sep = ""), format = "%Y-%j")
bayesian.sd_2$Date <- as.Date(paste(bayesian.sd_2$year, "-", bayesian.sd_2$doy, sep = ""), format = "%Y-%j")
bayesian.sd_3$Date <- as.Date(paste(bayesian.sd_3$year, "-", bayesian.sd_3$doy, sep = ""), format = "%Y-%j")

# Select columns to keep
bayesian.sd_0.5 <- bayesian.sd_0.5[,c("Date", "GPPsd", "Rsd", "NEPsd")]
bayesian.sd_1 <- bayesian.sd_1[,c("Date", "GPPsd", "Rsd", "NEPsd")]
bayesian.sd_2 <- bayesian.sd_2[,c("Date", "GPPsd", "Rsd", "NEPsd")]
bayesian.sd_3 <- bayesian.sd_3[,c("Date", "GPPsd", "Rsd", "NEPsd")]

bayesian.merge_0.5 <- merge(bayesian.res_0.5, bayesian.sd_0.5, by = "Date")
bayesian.merge_1 <- merge(bayesian.res_1, bayesian.sd_1, by = "Date")
bayesian.merge_2 <- merge(bayesian.res_2, bayesian.sd_2, by = "Date")
bayesian.merge_3 <- merge(bayesian.res_3, bayesian.sd_3, by = "Date")

# create Z.mix variable identifier
bayesian.merge_0.5$z.mix <- "0.5m"
bayesian.merge_1$z.mix <- "1m"
bayesian.merge_2$z.mix <- "2m"
bayesian.merge_3$z.mix <- "3m"

# Merge
bayesian.merge.all <- rbind(bayesian.merge_0.5, bayesian.merge_1,
                            bayesian.merge_2, bayesian.merge_3)

# Step 11: Visualize the results -----------------------------------------------

# GPP
p1 = ggplot(data = bayesian.merge.all, aes(x=Date, y=GPP, color=z.mix)) + theme_light() +
  geom_line(data = subset(bayesian.merge.all, z.mix == "0.5m")) +
  geom_errorbar(aes(ymin = GPP-GPPsd, ymax = GPP+GPPsd), width = 0.12) +
  geom_line(data = subset(bayesian.merge.all, z.mix == "1m")) +
  geom_line(data = subset(bayesian.merge.all, z.mix == "2m")) +
  geom_line(data = subset(bayesian.merge.all, z.mix == "3m")) +
  ylab(expression(paste("GPP (", O[2], " in mg L", NULL^-1, " day", NULL^-1, ")")))

# R
p2 = ggplot(data = bayesian.merge.all, aes(x=Date, y=R, color=z.mix)) + theme_light() +
  geom_line(data = subset(bayesian.merge.all, z.mix == "0.5m")) +
  geom_errorbar(aes(ymin = R-Rsd, ymax = R+Rsd), width = 0.12) +
  geom_line(data = subset(bayesian.merge.all, z.mix == "1m")) +
  geom_line(data = subset(bayesian.merge.all, z.mix == "2m")) +
  geom_line(data = subset(bayesian.merge.all, z.mix == "3m")) +
  ylab(expression(paste("R (", O[2], " in mg L", NULL^-1, " day", NULL^-1, ")")))

# NEP
p3 = ggplot(data = bayesian.merge.all, aes(x=Date, y=NEP, color=z.mix)) + theme_light() +
  geom_line(data = subset(bayesian.merge.all, z.mix == "0.5m")) +
  geom_errorbar(aes(ymin = NEP-NEPsd, ymax = NEP+NEPsd), width = 0.12) +
  geom_line(data = subset(bayesian.merge.all, z.mix == "1m")) +
  geom_line(data = subset(bayesian.merge.all, z.mix == "2m")) +
  geom_line(data = subset(bayesian.merge.all, z.mix == "3m")) +
  ylab(expression(paste("NEP (", O[2], " in mg L", NULL^-1, " day", NULL^-1, ")")))

# Plots of ecosystem parameters
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

fig2 <- ggarrange(p1 + rremove("xlab"), p2 + rremove("xlab"), p3 + rremove("xlab"),
                  p4 + rremove("xlab"), p5 + rremove("xlab"), p6 + rremove("xlab"),
                  p7 + rremove("xlab"), p8 + rremove("xlab"), nrow=4, ncol = 2,
                  common.legend = TRUE, legend = "right")

annotate_figure(fig2, top = text_grob("Sensitivity analysis for z.mix in pond B1P1, 2021"))

# Save the plot as a .png
png("../results_SensAnalysis/SensAnalysisZmix_B1P1_2021.png", width = 2000,
    height = 1500, res = 150)
annotate_figure(fig2, top = text_grob("Sensitivity analysis for z.mix in pond B1P1, 2021"))
dev.off()
