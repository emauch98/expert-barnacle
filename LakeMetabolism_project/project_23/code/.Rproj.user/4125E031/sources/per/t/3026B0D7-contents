
# Step 0: set up R-script ------------------------------------------------------
rm(list= ls())

source("../../Literature/Moritz_Luehrig_paper_stuff/methods_packages.R")

# For plotting
require(GGally)
require(ggpubr)
require(grid)

# For images
require(grDevices)

# Import the metabolism result datasets

# Treatment: Fish
B3P3 <- fread("../results_metabolism/B3P3_bayesian_2023.txt")
B1P1 <- fread("../results_metabolism/B1P1_bayesian_2023.txt")
B2P2 <- fread("../results_metabolism/B2P2_bayesian_2023.txt")
B2P3 <- fread("../results_metabolism/B2P3_bayesian_2023.txt")
B3P1 <- fread("../results_metabolism/B3P1_bayesian_2023.txt")
B3P2 <- fread("../results_metabolism/B3P2_bayesian_2023.txt")

# Treatment: no Fish
B2P4 <- fread("../results_metabolism/B2P4_bayesian_2023.txt")
B1P4 <- fread("../results_metabolism/B1P4_bayesian_2023.txt")
ERL85 <- fread("../results_metabolism/ERL85_bayesian_2023.txt")
# From eyeballing the results, it appears that we had trouble
# estimating metabolism in pond ERL85 (e.g. positive Respiration on quite a
# lot of days)
B3P0 <- fread("../results_metabolism/B3P0_bayesian_2023.txt")
ERL122 <- fread("../results_metabolism/ERL122_bayesian_2023.txt")
# We did not havetemperature measurements for ERL152, hence the analysis
# was not possible for this pond

# Create a merged results dataset
# Fish
B3P3$Treatment <- "Fish"
B3P3$Pond <- "B3P3"

B1P1$Treatment <- "Fish"
B1P1$Pond <- "B1P1"

B2P2$Treatment <- "Fish"
B2P2$Pond <- "B2P2"

B2P3$Treatment <- "Fish"
B2P3$Pond <- "B2P3"

B3P1$Treatment <- "Fish"
B3P1$Pond <- "B3P1"

B3P2$Treatment <- "Fish"
B3P2$Pond <- "B3P2"

# No Fish
B2P4$Treatment <- "no Fish"
B2P4$Pond <- "B2P4"

B1P4$Treatment <- "no Fish"
B1P4$Pond <- "B1P4"

ERL85$Treatment <- "no Fish"
ERL85$Pond <- "ERL85"

B3P0$Treatment <- "no Fish"
B3P0$Pond <- "B3P0"

ERL122$Treatment <- "no Fish"
ERL122$Pond <- "ERL122"

# Merge
all <- rbind(B1P1, B2P2, B3P3, B2P3, B3P1, B3P2, B2P4, B1P4, ERL85, B3P0, ERL122)

all$Treatment <- factor(all$Treatment)

# Save
fwrite(all, "../results_metabolism/merged_results_complete_2023.txt", sep = "\t")

# ------------------------------------------------------------------------------
# Plot: R

# Define the color palette for the ponds with fish
fish_colors <- c("B1P1" = "#1f78b4",
                 "B2P2" = "#a6d8e7",
                 "B3P3" = "#85c0e0",
                 "B2P3" = "#54b0d9",
                 "B3P1" = "#0088cc",
                 "B3P2" = "#005f8a")

# Define the color palette for the ponds without fish
no_fish_colors <- c("B2P4" = "#d9d9d9",
                    "B1P4" = "#b0b0b0",
                    "ERL85" = "#808080",
                    "B3P0" = "#595959",
                    "ERL122" = "#404040")

# Combine both color sets
custom_colors <- c(fish_colors, no_fish_colors)

p1 = ggplot() +
  # Ponds with Fish
  geom_line(data = subset(all, Pond %in% names(fish_colors)), aes(y=GPP, x=Date, color = Pond), lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond %in% names(fish_colors)), 
                aes(ymin = GPP - GPPsd, ymax = GPP + GPPsd, x = Date, color = Pond), 
                width = 0.2) +
  
  # Ponds without Fish
  geom_line(data = subset(all, Pond %in% names(no_fish_colors)), aes(y=GPP, x=Date, color = Pond), lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond %in% names(no_fish_colors)), 
                aes(ymin = GPP - GPPsd, ymax = GPP + GPPsd, x = Date, color = Pond), 
                width = 0.2) +
  
  labs(y = expression(paste("GPP (", O[2], " in mg L", NULL^-1, " day", NULL^-1, ")")),
       x = "Date") +
  theme_minimal() +
  scale_color_manual(values = custom_colors) +  # Apply custom color scale
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 18),
    legend.title = element_text(size = 20),  # Remove legend title
    legend.text = element_text(size = 18)
  )

# Display the plot
plot(p1)

# Save the plot as a .png
png("../results_metabolism/GPP_complete_2023.png", width = 1600, height = 1000)
print(p1)
dev.off()

# ------------------------------------------------------------------------------
# R
# Fish
p2 = ggplot() +
  # Ponds with Fish
  geom_line(data = subset(all, Pond %in% names(fish_colors)), aes(y=R, x=Date, color = Pond), lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond %in% names(fish_colors)), 
                aes(ymin = R - Rsd, ymax = R + Rsd, x = Date, color = Pond), 
                width = 0.2) +
  
  # Ponds without Fish
  geom_line(data = subset(all, Pond %in% names(no_fish_colors)), aes(y=R, x=Date, color = Pond), lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond %in% names(no_fish_colors)), 
                aes(ymin = R - Rsd, ymax = R + Rsd, x = Date, color = Pond), 
                width = 0.2) +
  
  labs(y = expression(paste("R (", O[2], " in mg L", NULL^-1, " day", NULL^-1, ")")),
       x = "Date") +
  theme_minimal() +
  scale_color_manual(values = custom_colors) +  # Apply custom color scale
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 18),
    legend.title = element_text(size = 20),  # Remove legend title
    legend.text = element_text(size = 18),
  )

plot(p2)

# Save the plot as a .png
png("../results_metabolism/R_complete_2023.png", width = 1600, height = 1000)
print(p2)
dev.off()

# ------------------------------------------------------------------------------
# NEP
# Fish
p3 = ggplot() +
  # Ponds with Fish
  geom_line(data = subset(all, Pond %in% names(fish_colors)), aes(y=NEP, x=Date, color = Pond), lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond %in% names(fish_colors)), 
                aes(ymin = NEP - NEPsd, ymax = NEP + NEPsd, x = Date, color = Pond), 
                width = 0.2) +
  
  # Ponds without Fish
  geom_line(data = subset(all, Pond %in% names(no_fish_colors)), aes(y=NEP, x=Date, color = Pond), lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond %in% names(no_fish_colors)), 
                aes(ymin = NEP - NEPsd, ymax = NEP + NEPsd, x = Date, color = Pond), 
                width = 0.2) +
  
  labs(y = expression(paste("NEP (", O[2], " in mg L", NULL^-1, " day", NULL^-1, ")")),
       x = "Date") +
  theme_minimal() +
  scale_color_manual(values = custom_colors) +  # Apply custom color scale
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 18),
    legend.title = element_text(size = 20),  # Remove legend title
    legend.text = element_text(size = 18),
  )

plot(p3)

# Save the plot as a .png
png("../results_metabolism/NEP_complete_2023.png", width = 1600, height = 1000)
print(p3)
dev.off()

# ------------------------------------------------------------------------------
# Plot daily average of the 3 metabolism measures for Fish vs. no Fish ---------

# Import the results dataset
all<- fread("../results_metabolism/merged_results_complete_2023.txt", sep = "\t")

# Calculate average GPP per day and treatment
mean_gpp <- all[, .(mean_GPP = mean(GPP)), by = .(Date, Treatment)]

# Calculate average R per day and treatment
mean_r <- all[, .(mean_R = mean(R)), by = .(Date, Treatment)]

# Calculate average NEP per day and treatment
mean_nep <- all[, .(mean_NEP = mean(NEP)), by = .(Date, Treatment)]

# merge
daily_means <- merge(mean_gpp, mean_r, by = c("Date", "Treatment"))
daily_means <- merge(daily_means, mean_nep, by = c("Date", "Treatment"))

# save
fwrite(daily_means, "../results_metabolism/merged_results_daily_mean_complete_2023.txt", sep = "\t")


# Plot -------------------------------------------------------------------------

# Plot mean daily GPP Fish vs. no Fish
p4 = ggplot(data = daily_means, aes(x = Date, y = mean_GPP, color = Treatment)) +
  theme_minimal() +
  geom_line(lwd = 1.1, alpha = 0.5) +
  geom_point(alpha = 0.5, size = 3) +
  ylab(expression(paste("mean GPP (", O[2], " in mg L", NULL^-1, " day", NULL^-1, ")"))) +
  scale_color_manual(values = c("Fish" = "#1f78b4", "no Fish" = "#808080")) +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
)

plot(p4)

# Save the plot as a .png
png("../results_metabolism/GPP_daily_means_complete_2023.png", width = 2000, height = 1000)
print(p4)
dev.off()

# ------------------------------------------------------------------------------
# Plot mean daily R Fish vs. no Fish
p5 = ggplot(data = daily_means, aes(x = Date, y = mean_R, color = Treatment)) +
  geom_line(lwd = 1.1, alpha = 0.5) +
  geom_point(alpha = 0.5, size = 3) +
  theme_minimal() +
  ylab(expression(paste("mean R (", O[2], " in mg L", NULL^-1, " day", NULL^-1, ")"))) +
  scale_color_manual(values = c("Fish" = "#1f78b4", "no Fish" = "#808080")) +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
  )
plot(p5)

# Save the plot as a .png
png("../results_metabolism/R_daily_means_complete_2023.png", width = 2000, height = 1000)
print(p5)
dev.off()

# ------------------------------------------------------------------------------
# Plot mean daily NEP Fish vs. no Fish
p6 = ggplot(data = daily_means, aes(x = Date, y = mean_NEP, color = Treatment)) +
  geom_line(lwd = 1.1, alpha = 0.5) +
  geom_point(alpha = 0.5, size = 3) +
  theme_minimal() +
  ylab(expression(paste("mean NEP (", O[2], " in mg L", NULL^-1, " day", NULL^-1, ")"))) +
  scale_color_manual(values = c("Fish" = "#1f78b4", "no Fish" = "#808080")) +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
  )

plot(p6)

# Save the plot as a .png
png("../results_metabolism/NEP_daily_means_complete_2023.png", width = 2000, height = 1000)
print(p6)
dev.off()
