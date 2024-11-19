
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
B3P3 <- fread("../results_metabolism/B3P3_bayesian_2022.txt")
B1P1 <- fread("../results_metabolism/B1P1_bayesian_2022.txt")
B2P2 <- fread("../results_metabolism/B2P2_bayesian_2022.txt")
B2P3 <- fread("../results_metabolism/B2P3_bayesian_2022.txt")
B3P1 <- fread("../results_metabolism/B3P1_bayesian_2022.txt")
B3P2 <- fread("../results_metabolism/B3P2_bayesian_2022.txt")

# Treatment: no Fish
B2P4 <- fread("../results_metabolism/B2P4_bayesian_2022.txt")
ERL122 <- fread("../results_metabolism/ERL122_bayesian_2022.txt")
ERL85 <- fread("../results_metabolism/ERL85_bayesian_2022.txt")

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

ERL122$Treatment <- "no Fish"
ERL122$Pond <- "ERL122"

ERL85$Treatment <- "no Fish"
ERL85$Pond <- "ERL85"

# Merge
all <- rbind(B1P1, B2P2, B3P3, B2P3, B3P1, B3P2, B2P4, ERL122, ERL85)

# Save
fwrite(all, "../results_metabolism/merged_results_complete_2022.txt", sep = "\t")

# Plot: GPP
# Fish
p1 = ggplot(subset(all, Pond=="B1P1"), aes(y=GPP, x=Date, color = Treatment)) + theme_light() +
  geom_line(lwd = 1.1, alpha = 0.5) +
  geom_errorbar(aes(ymin = GPP-GPPsd, ymax = GPP+GPPsd), width = 0.15) +
  
  geom_line(data = subset(all, Pond=="B2P2"), aes(y=GPP, x=Date, color = Treatment),lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond=="B2P2"), aes(ymin = GPP-GPPsd, ymax = GPP+GPPsd), width = 0.15)+
    
  geom_line(data = subset(all, Pond=="B3P3"), aes(y=GPP, x=Date, color = Treatment),lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond=="B3P3"), aes(ymin = GPP-GPPsd, ymax = GPP+GPPsd), width = 0.15) +
    
  geom_line(data = subset(all, Pond=="B2P3"), aes(y=GPP, x=Date, color = Treatment),lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond=="B2P3"), aes(ymin = GPP-GPPsd, ymax = GPP+GPPsd), width = 0.15) +
    
  geom_line(data = subset(all, Pond=="B3P1"), aes(y=GPP, x=Date, color = Treatment),lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond=="B3P1"), aes(ymin = GPP-GPPsd, ymax = GPP+GPPsd), width = 0.15) +
    
  geom_line(data = subset(all, Pond=="B3P2"), aes(y=GPP, x=Date, color = Treatment),lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond=="B3P2"), aes(ymin = GPP-GPPsd, ymax = GPP+GPPsd), width = 0.15) +
    
  # No Fish
  geom_line(data = subset(all, Pond=="B2P4"), aes(y=GPP, x=Date, color = Treatment),lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond=="B2P4"), aes(ymin = GPP-GPPsd, ymax = GPP+GPPsd), width = 0.15) +
    
  geom_line(data = subset(all, Pond=="ERL122"), aes(y=GPP, x=Date, color = Treatment),lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond=="ERL122"), aes(ymin = GPP-GPPsd, ymax = GPP+GPPsd), width = 0.15) +
    
  geom_line(data = subset(all, Pond=="ERL85"), aes(y=GPP, x=Date, color = Treatment),lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond=="ERL85"), aes(ymin = GPP-GPPsd, ymax = GPP+GPPsd), width = 0.15) +
    
  ylab(expression(paste("GPP (", O[2], " in mg L", NULL^-1, " day", NULL^-1, ")"))) +
  ggtitle("Modeled GPP of all Ponds in 2022")

plot(p1)

# Save the plot as a .png
png("../results_metabolism/GPP_complete_2022.png", width = 1600, height = 1000)
print(p1)
dev.off()

# R
# Fish
p2 = ggplot(subset(all, Pond=="B1P1"), aes(y=R, x=Date, color = Treatment)) + theme_light() +
  geom_line(lwd = 1.1, alpha = 0.5) +
  geom_errorbar(aes(ymin = R-Rsd, ymax = R+Rsd), width = 0.15) +
  
  geom_line(data = subset(all, Pond=="B2P2"), aes(y=R, x=Date, color = Treatment),lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond=="B2P2"), aes(ymin = R-Rsd, ymax = R+Rsd), width = 0.15)+
  
  geom_line(data = subset(all, Pond=="B3P3"), aes(y=R, x=Date, color = Treatment),lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond=="B3P3"), aes(ymin = R-Rsd, ymax = R+Rsd), width = 0.15) +
  
  geom_line(data = subset(all, Pond=="B2P3"), aes(y=R, x=Date, color = Treatment),lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond=="B2P3"), aes(ymin = R-Rsd, ymax = R+Rsd), width = 0.15) +
  
  geom_line(data = subset(all, Pond=="B3P1"), aes(y=R, x=Date, color = Treatment),lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond=="B3P1"), aes(ymin = R-Rsd, ymax = R+Rsd), width = 0.15) +
  
  geom_line(data = subset(all, Pond=="B3P2"), aes(y=R, x=Date, color = Treatment),lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond=="B3P2"), aes(ymin = R-Rsd, ymax = R+Rsd), width = 0.15) +
  
  # No Fish
  geom_line(data = subset(all, Pond=="B2P4"), aes(y=R, x=Date, color = Treatment),lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond=="B2P4"), aes(ymin = R-Rsd, ymax = R+Rsd), width = 0.15) +
  
  geom_line(data = subset(all, Pond=="ERL122"), aes(y=R, x=Date, color = Treatment),lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond=="ERL122"), aes(ymin = R-Rsd, ymax = R+Rsd), width = 0.15) +
  
  geom_line(data = subset(all, Pond=="ERL85"), aes(y=R, x=Date, color = Treatment),lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond=="ERL85"), aes(ymin = R-Rsd, ymax = R+Rsd), width = 0.15) +
  ylab(expression(paste("R (", O[2], " in mg L", NULL^-1, " day", NULL^-1, ")"))) +
  ggtitle("Modeled R of all Ponds in 2022")

plot(p2)

# Save the plot as a .png
png("../results_metabolism/R_complete_2022.png", width = 1600, height = 1000)
print(p2)
dev.off()

# NEP
# Fish
p3 = ggplot(subset(all, Pond=="B1P1"), aes(y=NEP, x=Date, color = Treatment)) + theme_light() +
  geom_line(lwd = 1.1, alpha = 0.5) +
  geom_errorbar(aes(ymin = NEP-NEPsd, ymax = NEP+NEPsd), width = 0.15) +
  
  geom_line(data = subset(all, Pond=="B2P2"), aes(y=NEP, x=Date, color = Treatment),lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond=="B2P2"), aes(ymin = NEP-NEPsd, ymax = NEP+NEPsd), width = 0.15)+
  
  geom_line(data = subset(all, Pond=="B3P3"), aes(y=NEP, x=Date, color = Treatment),lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond=="B3P3"), aes(ymin = NEP-NEPsd, ymax = NEP+NEPsd), width = 0.15) +
  
  geom_line(data = subset(all, Pond=="B2P3"), aes(y=NEP, x=Date, color = Treatment),lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond=="B2P3"), aes(ymin = NEP-NEPsd, ymax = NEP+NEPsd), width = 0.15) +
  
  geom_line(data = subset(all, Pond=="B3P1"), aes(y=NEP, x=Date, color = Treatment),lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond=="B3P1"), aes(ymin = NEP-NEPsd, ymax = NEP+NEPsd), width = 0.15) +
  
  geom_line(data = subset(all, Pond=="B3P2"), aes(y=NEP, x=Date, color = Treatment),lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond=="B3P2"), aes(ymin = NEP-NEPsd, ymax = NEP+NEPsd), width = 0.15) +
  
  # No Fish
  geom_line(data = subset(all, Pond=="B2P4"), aes(y=NEP, x=Date, color = Treatment),lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond=="B2P4"), aes(ymin = NEP-NEPsd, ymax = NEP+NEPsd), width = 0.15) +
  
  geom_line(data = subset(all, Pond=="ERL122"), aes(y=NEP, x=Date, color = Treatment),lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond=="ERL122"), aes(ymin = NEP-NEPsd, ymax = NEP+NEPsd), width = 0.15) +
  
  geom_line(data = subset(all, Pond=="ERL85"), aes(y=NEP, x=Date, color = Treatment),lwd = 1.1, alpha = 0.5) +
  geom_errorbar(data = subset(all, Pond=="ERL85"), aes(ymin = NEP-NEPsd, ymax = NEP+NEPsd), width = 0.15) +
  ylab(expression(paste("NEP (", O[2], " in mg L", NULL^-1, " day", NULL^-1, ")"))) +
  ggtitle("Modeled NEP of all Ponds in 2022")

plot(p3)

# Save the plot as a .png
png("../results_metabolism/NEP_complete_2022.png", width = 1600, height = 1000)
print(p3)
dev.off()

# Plot daily average of the 3 metabolism measures for Fish vs. no Fish ---------

# Import the results dataset
all<- fread("../results_metabolism/merged_results_complete_2022.txt", sep = "\t")

# Calculate average GPP per day and treatment
mean_gpp <- all[, .(mean_GPP = mean(GPP)), by = .(Date, Treatment)]

# Calculate average R per day and treatment
mean_r <- all[, .(mean_R = mean(R)), by = .(Date, Treatment)]

# Calculate average NEP per day and treatment
mean_nep <- all[, .(mean_NEP = mean(NEP)), by = .(Date, Treatment)]

# merge
daily_means <- merge(mean_gpp, mean_nep, by = c("Date", "Treatment"))
daily_means <- merge(daily_means, mean_r, by = c("Date", "Treatment"))

# save
fwrite(daily_means, "../results_metabolism/merged_results_daily_mean_complete_2022.txt", sep = "\t")


# Plot -------------------------------------------------------------------------

# Plot mean daily GPP Fish vs. no Fish
p4 = ggplot(data = daily_means, aes(x = Date, y = mean_GPP, color = Treatment)) +
  geom_line(lwd = 1.1, alpha = 0.5) +
  geom_point(alpha = 0.5, size = 3) +
  theme_light() +
  ylab(expression(paste("mean GPP (", O[2], " in mg L", NULL^-1, " day", NULL^-1, ")"))) +
  ggtitle("Mean GPP across all ponds and treatments in 2022")

plot(p4)

# Save the plot as a .png
png("../results_metabolism/GPP_daily_means_complete_2022.png", width = 1600, height = 1000)
print(p4)
dev.off()

# Plot mean daily R Fish vs. no Fish
p5 = ggplot(data = daily_means, aes(x = Date, y = mean_R, color = Treatment)) +
  geom_line(lwd = 1.1, alpha = 0.5) +
  geom_point(alpha = 0.5, size = 3) +
  theme_light() +
  ylab(expression(paste("mean R (", O[2], " in mg L", NULL^-1, " day", NULL^-1, ")"))) +
  ggtitle("Mean R across all ponds and treatments in 2022")

plot(p5)

# Save the plot as a .png
png("../results_metabolism/R_daily_means_complete_2022.png", width = 1600, height = 1000)
print(p5)
dev.off()

# Plot mean daily NEP Fish vs. no Fish
p6 = ggplot(data = daily_means, aes(x = Date, y = mean_NEP, color = Treatment)) +
  geom_line(lwd = 1.1, alpha = 0.5) +
  geom_point(alpha = 0.5, size = 3) +
  theme_light() +
  ylab(expression(paste("mean NEP (", O[2], " in mg L", NULL^-1, " day", NULL^-1, ")"))) +
  ggtitle("Mean NEP across all ponds and treatments in 2022")

plot(p6)

# Save the plot as a .png
png("../results_metabolism/NEP_daily_means_complete_2022.png", width = 1600, height = 1000)
print(p6)
dev.off()