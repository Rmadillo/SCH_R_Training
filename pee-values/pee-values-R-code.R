############################################################################## #
#
# Analysis to accompany "The Only Good p-values Come from Catheters: Qualitative
# Philosophies and Quantitative Results in Pediatric Brain Surgery Outcomes"
# presentation by Dwight Barry at Advancing Analytics for Pediatric Hospitals
# June 5, 2019, Lurie Children's Hospital, Chicago, IL, USA
# 
# Data: Shurtleff et al. 2015, do not use without citing
# Code: Dwight Barry, CC0, use any way you want!
# Github: https://github.com/Rmadillo/SCH_R_Training/tree/master/pee-values
#
############################################################################## #




#### Abstract ------------------------------------------------------------- ####

# For over a century, the idea of statistical significance has been the gold 
# standard of the assessment of clinical data. While mathematical, statistical, 
# scientific, and logical criticisms of this approach have existed just as long,
# it has only been with the advances in statistical theory and computing power
# in the past few decades that better methods became widely available. Using a 
# small-n dataset of cognitive outcomes from neurosurgery patients, I will show 
# a few reasons why the concept of statistical significance is troublesome, and 
# apply Bayesian and Information-Theoretic methods to the same dataset to show 
# the impact of different statistical philosophies on interpretation of outcomes.




#### Overview ------------------------------------------------------------- ####


# Shurtleff et al. 2015. Impact of epilepsy surgery on development of preschool
# children: identification of a cohort likely to benefit from early intervention.
# Journal of Neurosurgery: Pediatrics 16(4): 383-392. http://dx.doi.org/10.3171/2015.3.PEDS14359

# Data is taken directly from Table 2 of Shurtleff et al. 2015
# so cite that paper when using this data

# Cognitive outcome after focal surgical resection was retrospectively reviewed
# for 15 cognitively intact children operated on between 2 and 6 years of age for
# lesion-related, early-onset epilepsy. Wechsler intelligence tests were conducted
# prior to and after surgery to explore differences in intelligence between short
# (<6 months) versus long (>18 months) duration of seizures prior to surgical
# resection.  

# The practical question: Does time from Dx to Tx matter for cognitive outcomes?  

# Only 11 patients had both pre and post surgery cognitive test scores.  




#### Setup ---------------------------------------------------------------- ####


# Load packages
library(reshape2)        # cast FSIQ data to pre/post
library(psych)           # summary stats
library(orddom)          # effect sizes with bootstrapped CIs
library(coin)            # frequentist permutation tests
library(simpleboot)      # simple bootstrapping CIs
library(dabestr)         # estimation plots
library(lme4)            # mixed models
library(BEST)            # Bayesian analysis
library(AICcmodavg)      # Information-Theoretic analysis
library(tidyverse)       # all the tidy goodness
library(ggfortify)       # plot model diagnostics


# Acquire data
DataLong = read_csv("https://raw.githubusercontent.com/Rmadillo/SCH_R_Training/master/Diff_Inf/Shurtleffetal2015_episurg_data.csv")


# Cast main data to wide format and focus on FSIQ only
DataWideD = dcast(DataLong, ID + Side + Duration ~ Phase, value.var = "FSIQ") %>%
    # Calculate change in score from Pre to Post
    mutate(FSIQD = Post - Pre,
           FSIQPerChng = (Post - Pre) / Pre) %>%
    # Remove NAs for final Differences results, order to show Short effect
    filter(complete.cases(.))


# Subset into short and long data frames for plotting and Bayesian t-test
short = filter(DataWideD, Duration == "Short")
long = filter(DataWideD, Duration == "Long") 


# Subset long form data for repeated measures ANOVA
DataLong2 = DataLong %>% filter(ID %in% DataWideD$ID)




#### EDA ------------------------------------------------------------------ ####


# Descriptive stats
describeBy(DataWideD[ , 4:6], group = DataWideD$Duration, skew = F, 
                  quant = c(0.25, 0.5, 0.75), digits = 0)


# |D| effect size
DataWideD %>% group_by(Duration) %>% summarize(mean(FSIQD))


# Plot density histograms of FSIQD by Duration
ggplot(DataWideD, aes(FSIQD, fill = Duration)) + 
    geom_vline(xintercept = mean(short$FSIQD), color = "#00BFC4") +  
    geom_vline(xintercept = mean(long$FSIQD), color = "#F8766D") + 
    geom_density(alpha = 0.70) + 
    # spacing for slides:
    geom_point(aes(y = -0.002), alpha = 0.0, pch = 21, size = 2, show.legend = F,
         position = position_jitter(height = 0.001)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_x_continuous(limit = c(-30, 30), breaks = seq(-30, 30, by = 5)) +
    labs(x = expression(paste(Delta, " FSIQ")),
         fill = "Duration:\nTx to Dx",
         title = "Distribution of Change in FSIQ after Treatment") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) 


# Plot density histograms of FSIQD by Duration, with data points
set.seed(25)
ggplot(DataWideD, aes(FSIQD, fill = Duration)) + 
    geom_vline(xintercept = mean(short$FSIQD), color = "#00BFC4") +  
    geom_vline(xintercept = mean(long$FSIQD), color = "#F8766D") + 
    geom_density(alpha = 0.70) + 
    geom_point(aes(y = -0.002), alpha = 0.8, pch = 21, size = 6, show.legend = F,
         position = position_jitter(height = 0.001)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_x_continuous(limit = c(-30, 30), breaks = seq(-30, 30, by = 5)) +
    labs(x = expression(paste(Delta, " FSIQ")),
         fill = "Duration:\nTx to Dx",
         title = "Distribution of Change in FSIQ after Treatment") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) 


# Plot Post Tx FSIQ given Pre Tx score by Duration 
ggplot(DataWideD, aes(Pre, Post, fill = Duration)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    geom_point(pch = 21, size = 4, show.legend = F) +
    facet_wrap( ~ Duration) +
    coord_equal(ylim = c(75, 145), xlim = c(75, 145)) +
    labs(x = "Pre-treatment FSIQ", y = "Post-treatment FSIQ") +
    theme_bw()


# Plot change in FSIQ by Duration given Pre Tx score
ggplot(DataWideD, aes(Pre, FSIQD, fill = Duration)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(pch = 21, size = 4, show.legend = F) +
    facet_wrap(~ Duration)+
    labs(x = "Pre-treatment FSIQ", y = expression(paste(Delta, " FSIQ"))) +
    theme_bw()


# Plot percent change in FSIQ by Duration given Pre Tx score
ggplot(DataWideD, aes(Pre, FSIQPerChng, fill = Duration)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(pch = 21, size = 4, show.legend = F) +
    facet_wrap(~ Duration)+
    labs(x = "Pre-treatment FSIQ", y = "Percent change in FSIQ") +
    scale_y_continuous(labels = scales::percent) +
    theme_bw()


# Gardner-Altman estimation plot
FSIQ_short_vs_long_GA_plot = DataWideD %>%
    # dabest still being developed, ignore warning
    dabest(Duration, FSIQD, idx = c("Long", "Short"), paired = FALSE, reps = 10000)

plot(FSIQ_short_vs_long_GA_plot, color.column = Duration, rawplot.groupwidth = 0, 
     rawplot.markersize = 4, rawplot.ylim = c(-12, 30))




#### Inference ------------------------------------------------------------ ####


#### Frequentist Analyses ####


# "Significance" tests <gag>
t.test(DataWideD$FSIQD ~ DataWideD$Duration, conf.level = 0.9)
t.test(DataWideD$FSIQD ~ DataWideD$Duration, var.equal = T, 
        conf.level = 0.9)
wilcox_test(DataWideD$FSIQD ~ factor(DataWideD$Duration), conf.int = T, 
        conf.level = 0.9)
wilcox_test(DataWideD$FSIQD ~ factor(DataWideD$Duration), distribution = "exact", 
        conf.int = T, conf.level = 0.9)
oneway_test(FSIQD ~ factor(Duration), data = DataWideD, distribution = "exact")


# Boostrapped compatibility interval on |D| (aka confidence interval)
FSIQ_D_CI = two.boot(short$FSIQD, long$FSIQD, mean, R = 10000)
quantile(FSIQ_D_CI$t, probs = c(0.05, 0.95))


# Regression
FSIQ_lm1 = lm(FSIQD ~ Duration, DataWideD)
summary(FSIQ_lm1)
confint(FSIQ_lm1)
autoplot(FSIQ_lm1, which = 1:6, ncol = 3, label.size = 3)


# Repeated measures ANOVA 
# Interaction term makes no physical sense but let's just go with it
FSIQ_RMA = aov(FSIQ ~ Phase * Duration + Error(ID), DataLong2, contrasts = )
summary.lm(FSIQ_RMA$Within)


# Mixed model
# Ditto on caveat as with ANOVA above
FSIQ_lmer = lmer(FSIQ ~ Duration * Phase + (1 | ID), DataLong2)
anova(FSIQ_lmer)
summary(FSIQ_lmer)
confint(FSIQ_lmer)


# ANCOVA
# Compare diffs but adjust for pre (proper way to analyze simple pre/post)
FSIQ_lm2 = lm(Post ~ Pre + Duration, DataWideD)
summary(FSIQ_lm2)
confint(FSIQ_lm2)
autoplot(FSIQ_lm2, which = 1:6, ncol = 3, label.size = 3)


# Multiple comparisons (using the lowest t-test p-value) for the three contrasts
# use family-wise Holm (conservative) and false discovery rate FDR (liberal)
p = c(0.046, 0.444, 0.05)
p.adjust(p, method = c("holm"))
p.adjust(p, method = c("fdr"))


# Vargha-Delany's A effect size (same as AUC) with bootstrapped CI
FESSVL = dmes.boot(long$FSIQD, short$FSIQD, theta.es = "Ab", ci.meth = "BCA",
        B = 1000, alpha = 0.1)
FESSVL




#### Bayesian Analysis ####


# based on Kruschke, J. K. (2013). Bayesian estimation supersedes the t test. 
# Journal of Experimental Psychology: General 142(2): 573-603
# http://dx.doi.org/10.1037/a0029146
# Default priors:  http://www.indiana.edu/~kruschke/BEST/BESThist.jpg

# requires rjags and jags:
# https://sourceforge.net/projects/mcmc-jags/


# Run the MCMC analysis, can take 30-60 seconds
BEST_SvL_FSIQ = BESTmcmc(short$FSIQD, long$FSIQD, numSavedSteps = 1e+06, 
        burnInSteps = 2000, verbose = FALSE)


# Check convergence and fit
BEST_SvL_FSIQ
plotPostPred(BEST_SvL_FSIQ, nCurvesToPlot = 50)


# Evaluate nu for form of t distribution parameter
hist(log10(BEST_SvL_FSIQ$nu))
mean(BEST_SvL_FSIQ$nu)


# Table of results: summary stats and |D| (muDiff)
FSIQ.Bayes = summary(BEST_SvL_FSIQ, credMass = 0.9, ROPEm = c(-5, 5), 
        ROPEsd = c(0, 15))
FSIQ.Bayes


# Posterior distribution plot w/ effect size, HDI, and ROPE
plot(BEST_SvL_FSIQ, xlim = c(-50, 60), credMass = 0.9, ROPE = c(-5, 5))
mtext("FSIQ, Short vs. Long", side = 3, font = 4)


# Probability of clinically important increase in FSIQ by being in short tx group
FSIQ_Bayes_ROPE = summary(BEST_SvL_FSIQ, credMass = 0.9, ROPEm = c(-5, 5), 
        ROPEsd = c(0, 15), compValm = 5)
FSIQ_Bayes_ROPE




#### Information-Theoretic Analysis ####


# based on Anderson, D. (2007). Alternatives to a p-value in simple "t-tests". 
# https://sites.warnercnr.colostate.edu/anderson/wp-content/uploads/sites/26/2016/11/PDF-of-t_test_ANOVA_-with-I-T_final.pdf


# FSIQ alt and null models
# lm1 was run above; included here just for reminder that it's part of model set
# FSIQ_lm1 = lm(FSIQD ~ Duration, DataWideD)
FSIQ_lm0 = lm(FSIQD ~ 1, DataWideD) 
summary(FSIQ_lm0)
autoplot(FSIQ_lm0, which = 1:6, ncol = 3, label.size = 3)

         
# Names for "alternative" and null models
mnames = c("H1", "H0")


# make AIC table and set up candidate models
FSIQ_candmodels = list(FSIQ_lm1, FSIQ_lm0)
FSIQ_IT_table = aictab(cand.set = FSIQ_candmodels, modnames = mnames)
FSIQ_IT_table


# Evidence ratio
FSIQ_IT_table[1, 6] / FSIQ_IT_table[2, 6]


# Model averaging

# Intercept
FSIQ_lm_intercept_avg = modavg(parm = "(Intercept)", cand.set = FSIQ_candmodels, 
        modnames = mnames, conf.level = 0.9)
FSIQ_lm_intercept_avg


# Duration coefficient
FSIQ_lm_shorteffect_avg = modavg(parm = "DurationShort", 
        cand.set = FSIQ_candmodels, modnames = mnames, conf.level = 0.9)
FSIQ_lm_shorteffect_avg


# Model averaged |D| effect size
FSIQ_modavg_effect = modavgEffect(cand.set = FSIQ_candmodels, modnames = mnames, 
        newdata = data.frame(Duration = c("Short", "Long")), 
        conf.level = 0.9)
FSIQ_modavg_effect




#### ~End of File~ ####
