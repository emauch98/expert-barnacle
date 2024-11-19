# Step 0: set up R-script ------------------------------------------------------
rm(list= ls())

setwd("~/ZIVI_EAWAG/project_21")

source("~/ZIVI_EAWAG/project_21/Moritz_Luehrig_paper_stuff/methods_packages.R")

# Mixed model packages
library(lme4)
library(nlme)

# Assisting packages
library(car)
library(pastecs)
library(ggpubr)
library(GGally)
library(effects)
library(arm)
library(MuMIn)

sonde_key = fread( "~/ZIVI_EAWAG/project_22/ponds_sonde_key.txt", header=T)
# Treatments:
# S: fish from single population (L26)
# D: fish from 2 populations (double, L26+ERL33)
# NF: no fish

# For t-test:
# overall pond-wise means
dat = fread("~/ZIVI_EAWAG/project_22/data/ponds_sonde_data_overall_avg.txt")

# For mixed model:
# daily pond-wise means (want to account for daily fluctuations by using the daily
# means of the individual ponds, simultaneously having enough datapoints to fit the model
# -> need more datapoints than levels of the random effect Sonde later)
dat2 = fread("~/ZIVI_EAWAG/project_22/data/ponds_sonde_data_daily_avg.txt")

# Step 1: format dataset -------------------------------------------------------

dat = dat[, c("Pond", "Treatment", "Sonde", "Chlorophyll_RFU")]
dat2 = dat2[, c("Pond", "Treatment", "Sonde", "Chlorophyll_RFU")]

# Summarize S (fish from single population) and D (fish from two populations) into F (Fish)
dat$Fish <- recode(dat$Treatment, "c('S', 'D') = 'yes'; c('NF') = 'no'")
dat2$Fish <- recode(dat2$Treatment, "c('S', 'D') = 'yes'; c('NF') = 'no'")

# Convert characters to factors
dat$Fish <- factor(dat$Fish)
dat$Sonde <- factor(dat$Sonde)
dat2$Fish <- factor(dat2$Fish)
dat2$Sonde <- factor(dat2$Sonde)

# Step 2: EDA ------------------------------------------------------------------

# Step 2.1: Visualize research question
# Graphically
ggplot(dat, aes(Fish, Chlorophyll_RFU)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(col = Sonde), width = 0.25, alpha=1, size = 3) +
  labs(y = "Chlorophyll RFU", title = "Pond-wise means Chloropyll RFU no Fish vs. Fish") +
  theme_light()

ggplot(dat2, aes(Fish, Chlorophyll_RFU)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(col = Sonde), width = 0.25, alpha=1, size = 3) +
  labs(y = "Chlorophyll RFU", title = "Pond-wise daily means Chloropyll RFU no Fish vs. Fish") +
  theme_light()

ggplot(dat2, aes(Treatment, Chlorophyll_RFU)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(col = Sonde), width = 0.25, alpha=1, size = 3) +
  labs(y = "Chlorophyll RFU", title = "Pond-wise daily means Chloropyll RFU no Fish vs. Fish") +
  theme_light()
# Different sondes seem to have different intercepts, even though the treatment
# is the same

# Step 2.2: Independence of Chlorophyll_RFU-------------------------------------
# Although Chlorophyll_RFU is strongly time-dependent, we sort of accounted
# for this dependence by taking the overall or daily means of the individual ponds

# Step 2.4. homogeneity of Chlorophyll_RFU--------------------------------------
ggplot(dat, aes(Fish, Chlorophyll_RFU)) +
  geom_boxplot() +
  theme_light()
# Variance in ponds with fish much higher

# Step 3: model fitting --------------------------------------------------------

# Approach:

# 1) T-test
# Since the assumption
# of homogeneity of variance is violated (ponds with Fish seem to vary much more in their
# diurnal Chlorophyll pattern, which is also part of the research question, 
# but can be addressed at a later stage), we can just do a two-sample t-test
# allowing for unequal variances.
# A concern is not accounting for the fact that the sondes
# have an impact on the measurements as well (as well as the solar radiation).
# I addition, I'm not sure what consequences follow if one uses aggregated data
# (comparing pond-wise means) to do a t-test.
# What I am sure is that one looses information about the distribution of chloro
# in the different ponds, as well power to detect a relationship.

# 2) Mixed model
# I was also thinking about a mixed model with fish as fixed effects and
# sondes as a random effect with random intercepts.
# For that, I will be using the daily means, to sort of adjust for the daily,
# time-dependent fluctuations in Chlorophyll RFU

# Hypotheses:

# H0: no associaton between the presence/absence of fish and chlorophyll RFU
# H1: association between presence/absence of fish and chlorophyll RFU

# Step 3.1: T-test--------------------------------------------------------------

# Welch two-sample t-test
(t1 <- t.test(formula = dat$Chlorophyll_RFU ~ dat$Fish, var.equal = FALSE, conf.level = 0.95))
# est. mean for ponds no Fish: -0.22
# est. mean for ponds Fish: 1.00
# Esimated difference in means no Fish - Fish: -1.22
# 95% Wald-CI: from -2.07 to -0.38
# p = 0.012
# There is evidence for an association between the presence/absence of fish in the
# ponds and the mean Chlorophyll RFU measured.

# Step 3.2: Mixed effects model-------------------------------------------------
# Step 3.2.1: Distribution of Chlorophyll RFU-----------------------------------

# ECDF no Fish vs. Fish daily means
ggplot(dat2, aes(Chlorophyll_RFU, col = Fish)) +
  stat_ecdf() +
  labs(y = expression("P(x"<="X | Fish)"), title = "Empirical Cumulative Distribution
        Function (ECDF) of Chloropyll RFU no Fish vs. Fish") +
  theme_light()

ggarrange(
  ggplot(subset(dat2, Fish=="no")) +
    geom_histogram(aes(x=Chlorophyll_RFU)) +
    theme_light() +
    labs(title = "No Fish"),
  ggplot(subset(dat2, Fish=="yes")) +
    geom_histogram(aes(x=Chlorophyll_RFU)) +
    theme_light() +
    labs(title = "Fish")
)
# Odd distribution of chloro for ponds without fish
# Overdispesion in ponds with fish ("long tail")
# In gerneral, Chlorphyll_RFU does not seem to be normally distributed

# Step 3.2.2: Fit Model --------------------------------------------------------

# Identifying the random structure
m1 <- lmer(Chlorophyll_RFU ~ Fish + (1|Sonde), REML = TRUE, data = dat2)
# I had this simple model in mind

m2 <- lmer(Chlorophyll_RFU ~ Fish + (1|Sonde) +(1|Pond), REML = TRUE, data = dat2)
# Maybe there is also variability caused by the different ponds, independent
# of the treatment (different baseline Chlorophyll RFU for whatever reason)?
# Although pond and sonde will be highly correlated

anova(m1, m2)
# Random intercept for pond doesn't lead to improvement
# take m1

# Rename
m <- m1

# Step 3.2.3: Model diagnostics ------------------------------------------------

plot(m)
# residual structure looks odd
# They seem to increase, as the fitted values increase (heteroscedasticity)

# Diagnostics dataframe
df <- data.frame(z.resid= scale(residuals(m)),
                 Fish= m@frame$Fish,
                 Fitted= fitted.values(m))

# Distribution of Chlorophyll_RFU
ggarrange(
  ggplot(df, aes(z.resid)) +
    geom_histogram(aes(y= ..density..), fill= "white", col= "black") +
    stat_function(fun= dnorm,
                  args= list(mean(df$z.resid),
                             sd(df$z.resid)),
                  n= 1e2,
                  col= "blue") +
    labs(y= "Density", x= "Standardized Residuals") +
    theme_bw(),
  ggplot(df, aes(sample= z.resid)) +
    stat_qq() +
    geom_abline(intercept= 0, slope= 1, col= "red") +
    labs(x= "Theoretical quantiles", y= "Sample Quantiles") +
    theme_bw(),
  ncol= 2)
# deviations from the assumption of normally distributed residuals
# over/underdispersion
# This is probably largely driven through Sonde14, which had very high values
# of chlorophyll RFU, lead to this deviation  from the normality assumption

# Residual plots
ggarrange(
  ggplot(df, aes(Fish, z.resid)) +
    geom_hline(yintercept= 0, lty= 2, col= "grey") +
    geom_boxplot() +
    theme_bw(),
  ggplot(df, aes(Fitted, z.resid)) +
    geom_hline(yintercept= 0, lty= 2, col= "grey") +
    geom_point() +
    geom_smooth(col= "magenta", se= F) +
    theme_bw(),
  ncol= 2)
# Heteroscedasticity in Fish

# Account for heteroscedasticity in Fish by applying a variance-function
# for the residuals:
# Constant variance for each level of Fish, but allowing for different variances
# in the 2 levels
vf1 <- varIdent(form= ~ 1|Fish)

vf2 <- varPower(form= ~ 1|Fish)

# Refit our new model with package nlme
m.vf <- lme(Chlorophyll_RFU ~ Fish,
            random = (~1|Sonde),
            weights = vf1,
            na.action=na.exclude,
            data = dat2)

# Refit previous model with package nlme
m.or <- lme(Chlorophyll_RFU ~ Fish,
            random = (~1|Sonde),
            na.action=na.exclude,
            data = dat2)

# Compare to model without specified variance function
anova(m.or, m.vf)
# Model with specified variance function seems to be a better fit

# Rename
m <- m.vf

# Step 3.2.4: Model interpretation and predctions ------------------------------

Anova(m)
# Fish contributes significantly to explain the variability
# observed in Chlorophyll_RFU
S(m)
# Fixed effects
# - The expected daily mean RFU level for ponds without Fish is -0.21
# - Compared to ponds without fish, ponds with fish are expected to have daily
# Chlorophyll RFU levels increased by 1.28 units (p = 0.008)

# Random effects
# The additional standard deviation in Chlorophyll RFU caused by the different Sondes
# is estimated to be 0.61

# Variance function
# Compared to ponds with fish, ponds without fish are expected to have a standard
# deviation reduced by a factor of 0.17 of Chlorophyll RFU-levels

# Pseudo-R-squared for mixed models
r.squaredGLMM(m)
# Proportion of variance explained by fixed effects: 0.47
# Proportion of variance explained by entire model: 0.89

# Prediction plot

pred.m<- data.frame(Effect("Fish", m))
pred.m$Fish<- factor(pred.m$Fish)

ggplot(pred.m, aes(Fish, fit, col= Fish)) +
  geom_errorbar(aes(ymin= lower, ymax= upper), width= .2, position= "dodge") +
  geom_point(size= 4, position= position_dodge(width= .2)) +
  geom_point(data= m$data, aes(y= Chlorophyll_RFU), alpha= .3,
             position= position_jitterdodge(dodge.width= .2,
                                            jitter.width= .1,
                                            jitter.height= 0)) +
  labs(y= "Chlorophyll RFU", title = "Predicted daily means of Chlorophyll RFU in ponds without
       fish vs. ponds with fish") +
  theme_light()