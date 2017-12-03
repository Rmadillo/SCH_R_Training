# Statistical Literacy for Biologists
# Week 4, Part 1: Effect Sizes (and CIs/HDIs)


## ------------------------------------------------------------------------
data(sbp, package = "sbpdata")

## ------------------------------------------------------------------------
# Load libraries
library(psych)
library(BEST)
library(simpleboot)
library(bootES)
library(epitools)
library(gridExtra)
library(tidyverse)


## ------------------------------------------------------------------------
# Density plot, means by group
ggplot(sbp, aes(sbp, fill = Treatment_Group)) +
    geom_density(alpha = 0.4) +
    geom_vline(data = filter(sbp, Treatment_Group == "Treatment"), 
               aes(xintercept = mean(sbp)), color = "blue") +
    geom_vline(data = filter(sbp, Treatment_Group == "Control"), 
               aes(xintercept = mean(sbp)), color = "red") +
    scale_y_continuous(NULL, breaks = NULL, name = "Relative Frequency") +
    theme_bw()

## ------------------------------------------------------------------------
# Look at means and normal distribution CIs for weight by Treatment_Group
Rmisc::group.CI(sbp ~ Treatment_Group, sbp)

## ------------------------------------------------------------------------
# Separate data into control and treatment groups
Treatment = filter(sbp, Treatment_Group == "Treatment")
Controls = filter(sbp, Treatment_Group == "Control")

# Absolute difference between means
abs(mean(Controls$sbp) - mean(Treatment$sbp))

# Difference between means CI
t.test(Controls$sbp, Treatment$sbp)$conf.int

## ------------------------------------------------------------------------
# Difference between means, bootstrapped version using bootES
# Your results will vary unless you set a seed 
# (run it a few times to see what happens)
bootES(sbp, data.col = "sbp", group.col = "Treatment_Group", contrast =
    c("Treatment", "Control"), effect.type = "unstandardized", R = 1000)

## ------------------------------------------------------------------------
# Means and CIs plot
# Note the use of mean_cl_boot to tell ggplot to calculate bootstrap CIs
p1 = ggplot(sbp, aes(Treatment_Group, sbp, color = Treatment_Group)) + 
    stat_summary(geom = "point", fun.y = mean) + 
    stat_summary(geom = "errorbar", fun.data = mean_cl_boot, 
                 width = 0) +
    labs(y = expression(paste("Mean Systolic BP")), 
         x = "", color = "Group") +
    theme(legend.position = "none")

# Difference between mean sbps into a two.boot object
diff_means = two.boot(Controls$sbp, Treatment$sbp, mean, R = 1000)

# Calculate the 95% CI using BCa bootstrapping (defaults to 10k reps)
diff_means_ci = boot.ci(diff_means, conf = 0.95, type = "bca")

# Make 1-row data frame for difference between means
sbp_mean_df_diff = data.frame(Difference = "Difference",
    Mean = mean(Controls$sbp) - mean(Treatment$sbp),
    Lower_CI = diff_means_ci$bca[4], Upper_CI = diff_means_ci$bca[5])

# Difference between means plot
p2 = ggplot(sbp_mean_df_diff, aes(x = Difference, y = Mean)) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0) +
  geom_point() +
  xlab("") +
  ylab("Difference Between Means")

# Plot side-by-side inside a single graph
grid.arrange(p1, p2, widths = c(.75, .25), nrow = 1)




## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# Ex 1
Female = filter(sbp, Gender == "Female")
Male = filter(sbp, Gender == "Male")

abs(mean(Female$height) - mean(Male$height))
t.test(Female$height, Male$height)$conf.int

bootES(sbp, data.col = "height", group.col = "Gender", contrast =
    c("Male", "Female"), effect.type = "unstandardized")




## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# Cohen's d
bootES(sbp, data.col = "sbp", group.col = "Treatment_Group", contrast =
    c("Treatment", "Control"), effect.type = "cohens.d")

# Hedge's g, with control SD used instead of pooled SD
bootES(sbp, data.col = "sbp", group.col = "Treatment_Group", contrast =
    c("Treatment", "Control"), effect.type = "cohens.d", 
    glass.control = "Control")

# Robust Cohen's d when you have "weird" tails or outliers
bootES(sbp, data.col = "sbp", group.col = "Treatment_Group", contrast =
    c("Treatment", "Control"), effect.type = "akp.robust.d")




## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# Ex 2
# http://rpsychologist.com/d3/cohend/




## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# Run the MCMC analysis, can take a few mins
BEST_CvT_sbp = BESTmcmc(Controls$sbp, Treatment$sbp)

# Plot all, look at pieces (not included in handout)
# plotAll(BEST_CvT_sbp)

# Plot difference in means results
plot(BEST_CvT_sbp)

## ------------------------------------------------------------------------
# Estimate of true mean difference
mean(BEST_CvT_sbp$mu1 - BEST_CvT_sbp$mu2)

# HDI on the difference between means
hdi(BEST_CvT_sbp$mu1 - BEST_CvT_sbp$mu2)




## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# Ex 3
BEST_FvM_height = BESTmcmc(Female$height, Male$height)

plot(BEST_FvM_height)

mean(BEST_CvT_sbp$mu1 - BEST_CvT_sbp$mu2)

hdi(BEST_CvT_sbp$mu1 - BEST_CvT_sbp$mu2)




## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# Look at the distributions and plot medians from the groups' data frames
ggplot(sbp, aes(sbp, fill = Treatment_Group)) +
    geom_density(alpha = 0.4) +
    geom_vline(xintercept = median(Controls$sbp), color = "red") +
    geom_vline(xintercept = median(Treatment$sbp), color = "blue") +
    theme_bw()

## ------------------------------------------------------------------------
# Look at medians and CIs for sbp by Control (c_*) and Treatment (t_*) groups
c_median = median(Controls$sbp)
c_median_boot = one.boot(Controls$sbp, median, R = 1000)
c_median_ci = boot.ci(c_median_boot, conf = 0.95, type = "bca")

t_median = median(Treatment$sbp)
t_median_boot = one.boot(Treatment$sbp, median, R = 1000)
t_median_ci = boot.ci(t_median_boot, conf = 0.95, type = "bca")

sbp_median_df = data.frame(Group = c("Control", "Treatment"), Median = 
    c(c_median, t_median), Lower_CI = c(c_median_ci$bca[4], 
    t_median_ci$bca[4]), Upper_CI = c(c_median_ci$bca[5], 
    t_median_ci$bca[5]))

## ------------------------------------------------------------------------
sbp_median_df

## ------------------------------------------------------------------------
# Difference between medians
median(Treatment$sbp) - median(Controls$sbp)

# Difference between median sbps into a two.boot object
# R is number of bootstrap replications
diff_medians = two.boot(Treatment$sbp, Controls$sbp, median, R = 1000)

# Calculate the 95% CI using BCa bootstrapping (defaults to 10k reps)
diff_medians_ci = boot.ci(diff_medians, conf = 0.95, type = "bca")
diff_medians_ci

## ------------------------------------------------------------------------
# Medians and CIs
p1 = ggplot(sbp_median_df, aes(x = Group, y = Median, color = Group)) +
    geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0) +
    geom_point() +
    xlab("") +
    ylab("Median Systolic BP") +
    theme(legend.position = "left")

# Make 1-row data frame for difference between medians
sbp_median_df_diff = data.frame(Difference = "Difference",
    Median = median(Treatment$sbp) - median(Controls$sbp),
    Lower_CI = diff_medians_ci$bca[4], Upper_CI = diff_medians_ci$bca[5])

# Difference between medians
p2 = ggplot(sbp_median_df_diff, aes(x = Difference, y = Median)) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0) +
  geom_point() +
  xlab("") +
  ylab("Difference Between Medians")

# Plot side-by-side inside a single graph
grid.arrange(p1, p2, widths = c(.75, .25), nrow = 1)

## ------------------------------------------------------------------------
# Difference between 75th percentiles
quantile(Treatment$sbp, probs = c(0.75)) - quantile(Controls$sbp, probs = c(0.75))

# Put into a two.boot object
diff_75 = two.boot(Treatment$sbp, Controls$sbp, quantile, probs = 0.75, R = 1000)

# 95% CI of the difference
diff_75_ci = boot.ci(diff_75, conf = 0.95, type = "bca")
diff_75_ci

## ------------------------------------------------------------------------
# Look at Q75s and CIs for sbp by Control (c_*) and Treatment (t_*) groups
c_75 = quantile(Controls$sbp, probs = c(0.75))
c_75_boot = one.boot(Controls$sbp, quantile, probs = 0.75, R = 1000)
c_75_ci = boot.ci(c_75_boot, conf = 0.95, type = "bca")

t_75 = quantile(Treatment$sbp, probs = c(0.75))
t_75_boot = one.boot(Treatment$sbp, quantile, probs = c(0.75), R = 1000)
t_75_ci = boot.ci(t_75_boot, conf = 0.95, type = "bca")

sbp_75_df = data.frame(Group = c("Control", "Treatment"), Q75 = 
    c(c_75, t_75), Lower_CI = c(c_75_ci$bca[4], 
    t_75_ci$bca[4]), Upper_CI = c(c_75_ci$bca[5], 
    t_75_ci$bca[5]))

# Q75s and CIs
p1 = ggplot(sbp_75_df, aes(x = Group, y = Q75, color = Group)) +
    geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0) +
    geom_point() +
    xlab("") +
    ylab("75th Percentile of SBP") +
    theme(legend.position = "left")

# Make 1-row data frame for difference between medians
sbp_75_df_diff = data.frame(Difference = "Difference",
    Q75 = quantile(Treatment$sbp, probs = c(0.75)) - quantile(Controls$sbp, probs = c(0.75)),
    Lower_CI = diff_75_ci$bca[4], Upper_CI = diff_75_ci$bca[5])

# Difference between Q75s
p2 = ggplot(sbp_75_df_diff, aes(x = Difference, y = Q75)) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0) +
  geom_point() +
  xlab("") +
  ylab("Difference Between 75th Percentiles")

# Plot side-by-side inside a single graph
grid.arrange(p1, p2, widths = c(.75, .25), nrow = 1)

## ------------------------------------------------------------------------
# Median difference
# H_0: there is a 50% probability that a value chosen at random 
# from one group exceeds a value chosen at random from the other group 
median_diff = wilcox.test(sbp ~ Treatment_Group, data = sbp, conf.int = TRUE)

# Show median difference
median_diff$estimate

# 95% CI
median_diff$conf.int




## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# Ex 4
# http://rpsychologist.com/d3/CI/




## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# Data from Muller, F. H., Tobakmissbrauch und Lungencarcinom,
# Zeit. f. Krebsforsch. 49, 57-85, 1939. One of the early papers
# investigating the relation between smoking and lung cancer.

# Number ("no_*") of heavy smokers in one group of 86 lung cancer patients
# and one group of 86 healthy individuals.

no_heavy_smokers <- c(56, 31)
no_cases <- c(86, 86)

prop.test(x = c(no_heavy_smokers), 
          n = c(no_cases))

binom.exact(x = c(no_heavy_smokers), 
            n = c(no_cases))

## ------------------------------------------------------------------------
lung_ci = binom.exact(x = c(no_heavy_smokers), n = c(no_cases))

lung_df = data.frame(Group = c("Lung Cancer", "No Lung Cancer"),
                         Proportion = lung_ci$proportion,
                         Lower_CI = lung_ci$lower,
                         Upper_CI = lung_ci$upper)

# Plot the two proportions and their CIs
ggplot(lung_df, aes(x = Group, y = Proportion, color = Group)) +
    geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.1) +
    geom_point() +
    ylab("Proportion of Heavy Smokers")

## ------------------------------------------------------------------------
# Obtain CI for the difference
prop_diff_ci = prop.test(x = c(no_heavy_smokers), n = c(no_cases))

# Make 1-row data frame
prop_lung_diff = data.frame(Difference = "Difference",
    Proportion = prop_diff_ci$estimate[1] - prop_diff_ci$estimate[2],
    Lower_CI = prop_diff_ci$conf.int[1],
    Upper_CI = prop_diff_ci$conf.int[2])

# Create main plot with the groups (as above)
prop_p1 = ggplot(lung_df, aes(x = Group, y = Proportion, color = Group)) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.1) +
  geom_point() +
  labs(x= "", y = "Proportion of Heavy Smokers") +
  theme(legend.position = "left")

# Create the secondary plot of the difference
prop_p2 = ggplot(prop_lung_diff, aes(x = Difference, y = Proportion)) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.1) +
  geom_point() +
  labs(x= "", y = "Difference in Proportion of Heavy Smokers")

# Plot side-by-side inside a single graph
grid.arrange(prop_p1, prop_p2, widths = c(.75, .25), nrow = 1)

## ------------------------------------------------------------------------
# devtools::install_github("rasmusab/bayesian_first_aid")
library(BayesianFirstAid)

bayes.prop.test(no_heavy_smokers, no_cases)

# Save the return value in order to inspect the model result further.
fit <- bayes.prop.test(no_heavy_smokers, no_cases)
plot(fit)
summary(fit)




## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# Create fake tumor count data (Treatment same as used before)
set.seed(54)
Treatment = rpois(30, 1)
set.seed(24)
Control = rpois(20, 2)

# Group rates (tumor count per organism) and 90% CIs 
pois.exact(x = c(sum(Treatment), sum(Control)), 
           pt = c(30, 20), conf.level = 0.90)

## ------------------------------------------------------------------------
tumor_ci = pois.exact(x = c(sum(Treatment), sum(Control)), 
    pt = c(30, 20), conf.level = 0.90)

tumor_new = data.frame(
    Group = c("Treatment", "Control"),
    Rate = tumor_ci$rate,
    Lower_CI = tumor_ci$lower,
    Upper_CI = tumor_ci$upper)

# Plot the two rates and their CIs
ggplot(tumor_new, aes(x = Group, y = Rate, color = Group)) +
    geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.1) +
    geom_point()

## ------------------------------------------------------------------------
poisson.test(x = c(no_heavy_smokers), 
             T = c(no_cases))

## ------------------------------------------------------------------------
riskratio(matrix(c(55, 31, 30, 56), 2, 2), method = "boot")

## ------------------------------------------------------------------------
# Obtain the odds ratio and CI 
oddsratio(matrix(c(55, 31, 30, 56), 2, 2))




## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
ggplot(Controls, aes(sbp, weight)) +
    geom_point()

cor(Controls$weight, Controls$sbp)
cor.test(Controls$weight, Controls$sbp)$conf.int

## ------------------------------------------------------------------------
bootES(c(Controls$weight, Controls$sbp), effect.type = "r")

## ------------------------------------------------------------------------
# iterations cut down from 5k to save processing time in class
bayes_cor = BayesianFirstAid::bayes.cor.test(sbp$sbp, sbp$weight, n.iter = 1000) 
plot(bayes_cor)
bayes_cor

## ------------------------------------------------------------------------
cor.ci(sbp[ , c(5, 3)], method = "spearman", plot = FALSE)

## ------------------------------------------------------------------------
cor.ci(sbp[ , c(5, 3)], method = "kendall", plot = FALSE)

## ------------------------------------------------------------------------
# Look at the data and the model
ggplot(Controls, aes(weight, sbp)) +
    geom_smooth(method = "lm") +
    geom_point() +
    facet_wrap(~Treatment_Group)

## ------------------------------------------------------------------------
# Linear model 
sbp_weight_control_lm = lm(sbp ~ weight, data = Controls)

# This is equivalent:
# sbp_weight_control_lm = glm(bmi ~ weight, data = Controls, family = gaussian)

# review model coefficients
sbp_weight_control_lm

## ------------------------------------------------------------------------
dotwhisker::dwplot(sbp_weight_control_lm)

## ------------------------------------------------------------------------
# Histogram and density of residuals, plus normal curve
multi.hist(sbp_weight_control_lm$residuals)

## ------------------------------------------------------------------------
ggplot(Controls, aes(weight, sbp)) +
  stat_quantile(quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9)) +
  geom_point()

## ------------------------------------------------------------------------
# Just 3 quantiles shown for brevity
weight_quantile_lm = rq(sbp ~ weight, tau = c(0.25, 0.50, 0.75), data = Controls)
summary(weight_quantile_lm)

## ------------------------------------------------------------------------
# run the linear model
sbp_lm = lm(sbp ~ Treatment_Group, data = sbp)

# review model coefficients
sbp_lm

# if you want to confirm this is the same as a t-test:
# Intercept (reference category mean) = 
# mean(Controls$sbp)
# Coefficient ("effect" of being in Treatment on sbp) = 
# mean(Treatment$sbp) - mean(Controls$sbp)

# quick and dirty plot
ggplot(sbp, aes(Treatment_Group, sbp)) +
  geom_boxplot() +
  coord_flip()

## ------------------------------------------------------------------------
# Get the data and obtain FSIQ delta
neuro = read.csv("https://raw.githubusercontent.com/Rmadillo/SCH_R_Training/master/Diff_Inf/Shurtleffetal2015_episurg_data.csv", header=T)
neuro_wide = reshape2::dcast(neuro, ID + Side + Duration ~ Phase, value.var = "FSIQ")
neuro_wide$FSIQ_D = neuro_wide$Post - neuro_wide$Pre
neuro_wide = na.omit(neuro_wide)

# Get the residuals from the null model and add them back to the data
FSIQ_lm_h0 = lm(FSIQ_D ~ 1, data = neuro_wide)
neuro_wide$FSIQ_resid = FSIQ_lm_h0$resid

# Plot residuals against the groups
ggplot(neuro_wide, aes(Duration, FSIQ_resid, color = Duration)) +
  geom_hline(yintercept = 0, color = "darkgreen") +
  geom_point() +
  theme_bw() +
  theme(legend.position = "none")




## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
library(survival)
library(survminer)

# Get data and add names of cancer type
data(tongue, package = "KMsurv")

tongue$tumor = ifelse(tongue$type == 1, "Aneuploid",  "Diploid")

# Create survival model
tongue_survival = survfit(Surv(time = time, event = delta) ~ tumor, 
  data = tongue)

# Plot the curves and table
ggsurvplot(tongue_survival, risk.table = TRUE, conf.int = TRUE)





## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
aq = reshape(anscombe, varying = TRUE, sep="", direction = "long", timevar = "seriesname")

## ------------------------------------------------------------------------
ggplot(aq, aes(x, y)) +
  geom_smooth(method = "lm") +
  geom_point() +
  facet_wrap(~ seriesname)

## ------------------------------------------------------------------------
aq_list = split(aq, aq$seriesname)
aq1 = lm(x ~ y , data = aq_list[[1]])
aq2 = lm(x ~ y , data = aq_list[[2]])
aq3 = lm(x ~ y , data = aq_list[[3]])
aq4 = lm(x ~ y , data = aq_list[[4]])
summary(aq1)$coefficients
summary(aq2)$coefficients
summary(aq3)$coefficients
summary(aq4)$coefficients
summary(aq1)$r.square
summary(aq2)$r.square
summary(aq3)$r.square
summary(aq4)$r.square

## ------------------------------------------------------------------------
# End of file