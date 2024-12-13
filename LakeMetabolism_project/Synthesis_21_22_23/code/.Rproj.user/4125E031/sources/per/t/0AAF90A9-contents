
# Step 0: set up R-script ------------------------------------------------------
rm(list= ls())

source("../../Literature/Moritz_Luehrig_paper_stuff/methods_packages.R")

# For plotting
require(GGally)
require(ggpubr)
require(grid)
require(gridExtra)

# For images
require(grDevices)

# Step 1: Import metabolism results datasets -----------------------------------
# 2021
all_21 <- fread("../../project_21/results_metabolism/merged_results_complete_2021.txt")

# 2022
all_22 <- fread("../../project_22/results_metabolism/merged_results_complete_2022.txt")

# 2023
all_23 <- fread("../../project_23/results_metabolism/merged_results_complete_2023.txt")
# In total, we have 425 days of estimated metabolism across all 3 years

# Step 2: Create datasets for pond-wise means and standard errors of means by year
means_21 <- all_21 %>%
  group_by(Treatment,Pond) %>%
  summarise(
    mean_GPP = mean(GPP),
    mean_R = mean(R),
    mean_NEP = mean(NEP),
    se_mean_GPP = sd(GPP)/sqrt(length(GPP)),
    se_mean_R = sd(R)/sqrt(length(R)),
    se_mean_NEP = sd(NEP)/sqrt(length(NEP)),
    .groups = "drop"
  )

means_22 <- all_22 %>%
  group_by(Treatment,Pond) %>%
  summarise(
    mean_GPP = mean(GPP),
    mean_R = mean(R),
    mean_NEP = mean(NEP),
    se_mean_GPP = sd(GPP)/sqrt(length(GPP)),
    se_mean_R = sd(R)/sqrt(length(R)),
    se_mean_NEP = sd(NEP)/sqrt(length(NEP)),
    .groups = "drop"
  )

means_23 <- all_23 %>%
  group_by(Treatment,Pond) %>%
  summarise(
    mean_GPP = mean(GPP),
    mean_R = mean(R),
    mean_NEP = mean(NEP),
    se_mean_GPP = sd(GPP)/sqrt(length(GPP)),
    se_mean_R = sd(R)/sqrt(length(R)),
    se_mean_NEP = sd(NEP)/sqrt(length(NEP)),
    .groups = "drop"
  )

# Step 3: Plot -----------------------------------------------------------------
# Overall GPP plots ------------------------------------------------------------
# 2021
p1 <- ggplot() + theme_minimal() +
  geom_jitter(data = all_21, aes(x = Treatment, y = GPP, color = Treatment),
              width = 0.1, size = 2, alpha = 0.1) +
  
  geom_point(data = means_21, aes(x = Treatment, y = mean_GPP, col = Treatment,
                                  group = Pond),
              size = 4, position = position_dodge(width = 0.1)) +
  
  geom_errorbar(data = means_21, aes(x = Treatment, ymin = mean_GPP-se_mean_GPP,
                                     ymax = mean_GPP+se_mean_GPP, col = Treatment,
                                     group = Pond), width = 0.3) +
  
  labs(title = "2021",
       y = expression(paste("GPP (", O[2], " in mg L", NULL^-1, " day", NULL^-1, ")"))) +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20))

# 2022
p2 <- ggplot() + theme_minimal() +
  geom_jitter(data = all_22, aes(x = Treatment, y = GPP, color = Treatment),
              width = 0.1, size = 2, alpha = 0.1) +
  
  geom_point(data = means_22, aes(x = Treatment, y = mean_GPP, col = Treatment,
                                  group = Pond),
             size = 4, position = position_dodge(width = 0.1)) +
  
  geom_errorbar(data = means_22, aes(x = Treatment, ymin = mean_GPP-se_mean_GPP,
                                     ymax = mean_GPP+se_mean_GPP, col = Treatment,
                                     group = Pond), width = 0.3) +
  
  labs(title = "2022",
       y = expression(paste("GPP (", O[2], " in mg L", NULL^-1, " day", NULL^-1, ")"))) +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20))

# 2023
p3 <- ggplot() + theme_minimal() +
  geom_jitter(data = all_23, aes(x = Treatment, y = GPP, color = Treatment),
              width = 0.1, size = 2, alpha = 0.1) +
  
  geom_point(data = means_23, aes(x = Treatment, y = mean_GPP, col = Treatment,
                                  group = Pond),
             size = 4, position = position_dodge(width = 0.1)) +
  
  geom_errorbar(data = means_23, aes(x = Treatment, ymin = mean_GPP-se_mean_GPP,
                                     ymax = mean_GPP+se_mean_GPP, col = Treatment,
                                     group = Pond), width = 0.3) +
  
  labs(title = "2023",
       y = expression(paste("GPP (", O[2], " in mg L", NULL^-1, " day", NULL^-1, ")"))) +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20))

# Overall R plots --------------------------------------------------------------
# 2021
p4 <- ggplot() + theme_minimal() +
  geom_jitter(data = all_21, aes(x = Treatment, y = R, color = Treatment),
              width = 0.1, size = 2, alpha = 0.1) +
  
  geom_point(data = means_21, aes(x = Treatment, y = mean_R, col = Treatment,
                                  group = Pond),
             size = 4, position = position_dodge(width = 0.1)) +
  
  geom_errorbar(data = means_21, aes(x = Treatment, ymin = mean_R-se_mean_R,
                                     ymax = mean_R+se_mean_R, col = Treatment,
                                     group = Pond), width = 0.3) +
  labs(y = expression(paste("R (", O[2], " in mg L", NULL^-1, " day", NULL^-1, ")"))) +
  theme(axis.title.x = element_blank())

# 2022
p5 <- ggplot() + theme_minimal() +
  geom_jitter(data = all_22, aes(x = Treatment, y = R, color = Treatment),
              width = 0.1, size = 2, alpha = 0.1) +
  
  geom_point(data = means_22, aes(x = Treatment, y = mean_R, col = Treatment,
                                  group = Pond),
             size = 4, position = position_dodge(width = 0.2)) +
  
  geom_errorbar(data = means_22, aes(x = Treatment, ymin = mean_R-se_mean_R,
                                     ymax = mean_R+se_mean_R, col = Treatment,
                                     group = Pond), width = 0.3) +
  labs(y = expression(paste("R (", O[2], " in mg L", NULL^-1, " day", NULL^-1, ")"))) +
  theme(axis.title.x = element_blank())

# 2023
p6 <- ggplot() + theme_minimal() +
  geom_jitter(data = all_23, aes(x = Treatment, y = R, color = Treatment),
              width = 0.1, size = 2, alpha = 0.1) +
  
  geom_point(data = means_23, aes(x = Treatment, y = mean_R, col = Treatment,
                                  group = Pond),
             size = 4, position = position_dodge(width = 0.2)) +
  
  geom_errorbar(data = means_23, aes(x = Treatment, ymin = mean_R-se_mean_R,
                                     ymax = mean_R+se_mean_R, col = Treatment,
                                     group = Pond), width = 0.3) +
  labs(y = expression(paste("R (", O[2], " in mg L", NULL^-1, " day", NULL^-1, ")"))) +
  theme(axis.title.x = element_blank())

# Overall NEP plots --------------------------------------------------------------
# 2021
p7 <- ggplot() + theme_minimal() +
  geom_jitter(data = all_21, aes(x = Treatment, y = NEP, color = Treatment),
              width = 0.1, size = 2, alpha = 0.1) +
  
  geom_point(data = means_21, aes(x = Treatment, y = mean_NEP, col = Treatment,
                                  group = Pond),
             size = 4, position = position_dodge(width = 0.1)) +
  
  geom_errorbar(data = means_21, aes(x = Treatment, ymin = mean_NEP-se_mean_NEP,
                                     ymax = mean_NEP+se_mean_NEP, col = Treatment,
                                     group = Pond), width = 0.3) +
  
  labs(y = expression(paste("NEP (", O[2], " in mg L", NULL^-1, " day", NULL^-1, ")"))) +
  theme(axis.title.x = element_blank())

# 2022
p8 <- ggplot() + theme_minimal() +
  geom_jitter(data = all_22, aes(x = Treatment, y = NEP, color = Treatment),
              width = 0.1, size = 2, alpha = 0.1) +
  
  geom_point(data = means_22, aes(x = Treatment, y = mean_NEP, col = Treatment,
                                  group = Pond),
             size = 4, position = position_dodge(width = 0.1)) +
  
  geom_errorbar(data = means_22, aes(x = Treatment, ymin = mean_NEP-se_mean_NEP,
                                     ymax = mean_NEP+se_mean_NEP, col = Treatment,
                                     group = Pond), width = 0.3) +
  
  labs(y = expression(paste("NEP (", O[2], " in mg L", NULL^-1, " day", NULL^-1, ")"))) +
  theme(axis.title.x = element_blank())

# 2023
p9 <- ggplot() + theme_minimal() +
  geom_jitter(data = all_23, aes(x = Treatment, y = NEP, color = Treatment),
              width = 0.1, size = 2, alpha = 0.1) +
  
  geom_point(data = means_23, aes(x = Treatment, y = mean_NEP, col = Treatment,
                                  group = Pond),
             size = 4, position = position_dodge(width = 0.1)) +
  
  geom_errorbar(data = means_23, aes(x = Treatment, ymin = mean_NEP-se_mean_NEP,
                                     ymax = mean_NEP+se_mean_NEP, col = Treatment,
                                     group = Pond), width = 0.3) +
  
  labs(y = expression(paste("NEP (", O[2], " in mg L", NULL^-1, " day", NULL^-1, ")"))) +
  theme(axis.title.x = element_blank())

# ------------------------------------------------------------------------------
# Aggregated results plot
p10 <- ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, nrow=3, ncol = 3,
                 common.legend = TRUE, legend = "bottom")

p11 <- ggarrange(p1, p2, p3, ncol = 3,
                 common.legend = TRUE, legend = "bottom")

# Save the plot as a .png
png("../results_metabolism_synthesis/overall_metabolism.png",
    width = 1000, height = 800)
plot(p10)
dev.off()

# Save the plot as a .png
png("../results_metabolism_synthesis/overall_GPP_pond_wise.png",
    width = 700, height = 400, res = 90)
plot(p11)
dev.off()
