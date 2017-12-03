# Stats Lit Week 3 
# Philosophies of Inference


## ------------------------------------------------------------------------
library(htmlTable)  # txtRound function
library(reshape2)   # manipulate data frames
library(knitr)      # formatted tables
library(psych)      # summary statistics
library(boot)       # bootstrapping
library(coin)       # permutation tests
library(BEST)       # Bayesian analysis
library(AICcmodavg) # Information-Theoretic analysis
library(tidyverse)  # ggplot2, dplyr, etc.
library(forcats)    # category helper for ggplot

## ------------------------------------------------------------------------
DataLong = read.csv("https://raw.githubusercontent.com/Rmadillo/SCH_R_Training/master/Diff_Inf/Shurtleffetal2015_episurg_data.csv", header = T)

## ------------------------------------------------------------------------
# Order the Phase factor so Pre comes before Post
DataLong$Phase = ordered(DataLong$Phase, levels = c("Pre", "Post"))

# Reshape main data to wide format and calculate change in score from Pre to Post
DataWideFSIQ = dcast(DataLong, ID + Side + Duration ~ Phase, value.var = "FSIQ")
DataWideFSIQ$FSIQD = DataWideFSIQ$Post - DataWideFSIQ$Pre

# Remove the Pre and Post columns from each
#DataWideFSIQ = DataWideFSIQ[,c(1:3,6)]

# Remove NAs to get data for patients w/ both pre/post scores only
DataWideD = na.omit(DataWideFSIQ)



## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# Density plot
ggplot(DataLong, aes(x = FSIQ, fill = Duration)) +
    geom_density(alpha = 0.5) +
    facet_wrap(~ Phase) +
    # shortcut to get rid of y-axis
    scale_y_continuous(NULL, breaks = NULL) +
    theme_bw()

## ------------------------------------------------------------------------
# Dot histogram
ggplot(DataLong, aes(x = FSIQ, fill = Duration)) +
    geom_dotplot(method = "histodot", stackgroups = TRUE) +
    facet_wrap(~ Phase) +
    scale_y_continuous(NULL, breaks = NULL) +
    theme_bw()

## ------------------------------------------------------------------------
# Table of summary stats for FSIQ scores (pre/post surgery)  
sum_stat_table = describeBy(list(DataWideD$Pre, DataWideD$Post), DataWideD$Duration, skew = FALSE, quant = c(0.25, 0.50, 0.75), IQR = TRUE, mat = TRUE, digits = 1)
kable(sum_stat_table, row.names = FALSE)

## ------------------------------------------------------------------------
# Density and dot histogram plot together
p1 = ggplot(DataWideD, aes(x = FSIQD, fill = Duration)) +
    geom_density(alpha = 0.5) +
    # Expand x-axis for inference
    xlim(-40, 40) +
    xlab("") +
    scale_y_continuous(NULL, breaks = NULL) +
    theme_bw()

p2 = ggplot(DataWideD, aes(x = FSIQD, fill = Duration)) +
    xlim(-40, 40) +
    geom_dotplot(method = "histodot", stackgroups = TRUE) +
    scale_y_continuous(NULL, breaks = NULL) +
    xlab(expression(paste(Delta, ' FSIQ'))) +
    theme_bw()

gridExtra::grid.arrange(p1, p2, ncol = 1, heights = c(0.7, 0.3))

## ------------------------------------------------------------------------
# Table of summary stats for *change* in FSIQ scores (pre/post surgery) 
sum_stat_delta_table = describeBy(DataWideD$FSIQD, DataWideD$Duration, skew = FALSE, quant = c(0.25, 0.50, 0.75), IQR = TRUE, mat = TRUE, digits = 1)
kable(sum_stat_delta_table, row.names = FALSE)



## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# Ex 1
# Use this code to get the nonverbal scores into wide format
DataWideNVIQ = dcast(DataLong, ID + Side + Duration ~ Phase, value.var = "Nonverbal")
DataWideNVIQ$NVIQD = DataWideNVIQ$Post - DataWideNVIQ$Pre
DataWideNVIQ = na.omit(DataWideNVIQ)



## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# Ex 1
ex1_p1 = ggplot(DataWideNVIQ, aes(x = NVIQD, fill = Duration)) +
    geom_density(alpha = 0.5) +
    xlim(-40, 40) +
    xlab("") +
    scale_y_continuous(NULL, breaks = NULL) +
    theme_bw()

ex1_p2 = ggplot(DataWideNVIQ, aes(x = NVIQD, fill = Duration)) +
    xlim(-40, 40) +
    geom_dotplot(method = "histodot", stackgroups = TRUE) +
    scale_y_continuous(NULL, breaks = NULL) +
    xlab(expression(paste(Delta, ' FSIQ'))) +
    theme_bw()

gridExtra::grid.arrange(ex1_p1, ex1_p2, ncol = 1, heights = c(0.75, 0.25))

describeBy(DataWideNVIQ$NVIQD, DataWideNVIQ$Duration, skew = FALSE, quant = c(0.25, 0.50, 0.75), IQR = TRUE, mat = TRUE, digits = 1)



## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# Dotplot
ggplot(DataWideD, aes(ID, FSIQD, color = Duration, fill = Duration)) +
    geom_bar(stat = "identity", width = 0.02) +
    geom_point(size = 4) +
    coord_flip()

## ------------------------------------------------------------------------
# Ordered dotplot
ggplot(DataWideD, aes(fct_reorder(as.factor(ID), FSIQD), FSIQD, color = Duration, fill = Duration)) +
    labs(y = expression(paste(Delta, " FSIQ")), x = "Subject (ordered by change in FSIQ)", 
         color = "Time to\ntreatment", fill = "Time to\ntreatment") +
    geom_hline(yintercept = 0, color = "gray50") +
    geom_bar(stat = "identity", width = 0.02) +
    geom_point(size = 4) +
    coord_flip()

## ------------------------------------------------------------------------
# Dumbbell plot
ggplot(DataWideFSIQ, aes(fct_reorder(as.factor(ID), Post), Post)) +
    # Add in a reference block for "normal IQ"
    geom_rect(xmin = -Inf, xmax = Inf, ymin = 85, ymax = 115, fill = "gray90", alpha = 0.05) +
    # The dumbbell's "bar"
    geom_segment(aes(x = fct_reorder(as.factor(ID), Post),
                     xend = fct_reorder(as.factor(ID), Post), 
                     y = Pre, yend = Post, color = Duration), 
                     size = 1.5, alpha = 0.4) +
    # The dumbbell's "weights"
    geom_point(aes(y = Pre, color = Duration, fill = Duration), size = 2.5, alpha = 0.75) +
    geom_point(aes(y = Post, color = Duration, fill = Duration), size = 4) +
    # Plot tidying
    labs(y = "FSIQ", x = "Subject ID\n(ordered by post-surgery FSIQ)", 
         color = "Time to\ntreatment", fill = "Time to\ntreatment") +
    coord_flip() +
    theme_bw()

## ------------------------------------------------------------------------
# Parallel coordinates plot
ggplot(DataLong, aes(Phase, FSIQ, group = ID, color = Duration)) +
    geom_line(alpha = 0.6) +
    geom_point(aes(shape = Duration), alpha = 0.8) +
    scale_x_discrete(expand = c(0.1, 0.1)) +
    theme_bw()



## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# Ex 2
ggplot(DataWideD, aes(Pre, Post, color = Duration)) +
    geom_point(aes(size = FSIQD))



## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# The basic t-test 
t.test(FSIQD ~ Duration, data = DataWideD)

## ------------------------------------------------------------------------
ggplot(DataWideD, aes(x = FSIQD)) +
    xlim(-40, 40) +
    geom_vline(data = filter(DataWideD, Duration == "Long"), 
        aes(xintercept = mean(FSIQD)), color = "red", linetype = "dashed") +
    stat_function(data = filter(DataWideD, Duration == "Long"),
        fun = dnorm, 
        color = "red",
        args = list(mean = mean(filter(DataWideD, Duration == "Long")$FSIQD), 
                    sd = sd(filter(DataWideD, Duration == "Long")$FSIQD))) +
    geom_vline(data = filter(DataWideD, Duration == "Short"), 
        aes(xintercept = mean(FSIQD)), color = "darkblue", linetype = "dashed") +
    stat_function(data = filter(DataWideD, Duration == "Short"),
        fun = dnorm, 
        color = "darkblue",
        args = list(mean = mean(filter(DataWideD, Duration == "Short")$FSIQD), 
                    sd = sd(filter(DataWideD, Duration == "Short")$FSIQD))) +
    scale_y_continuous(NULL, breaks = NULL) +
    xlab(expression(paste(Delta, ' FSIQ'))) 

## ------------------------------------------------------------------------
ggplot(DataWideD, aes(x = FSIQD)) +
        geom_density(aes(color = Duration, fill = Duration), alpha = 0.5) +
    xlim(-40, 40) +
    geom_vline(data = filter(DataWideD, Duration == "Long"), 
        aes(xintercept = mean(FSIQD)), color = "red", linetype = "dashed") +
    stat_function(data = filter(DataWideD, Duration == "Long"),
        fun = dnorm, 
        color = "red",
        args = list(mean = mean(filter(DataWideD, Duration == "Long")$FSIQD), 
                    sd = sd(filter(DataWideD, Duration == "Long")$FSIQD))) +
    geom_vline(data = filter(DataWideD, Duration == "Short"), 
        aes(xintercept = mean(FSIQD)), color = "darkblue", linetype = "dashed") +
    stat_function(data = filter(DataWideD, Duration == "Short"),
        fun = dnorm, 
        color = "darkblue",
        args = list(mean = mean(filter(DataWideD, Duration == "Short")$FSIQD), 
                    sd = sd(filter(DataWideD, Duration == "Short")$FSIQD))) +
    scale_y_continuous(NULL, breaks = NULL) +
    xlab(expression(paste(Delta, ' FSIQ'))) +
    theme(legend.position = "top")

## ------------------------------------------------------------------------
# Are the variances equal? (DON'T DO THIS IN REAL LIFE!)
var.test(FSIQD ~ Duration, data = DataWideD)

# t-test, and assume that the variances are equal
t.test(FSIQD ~ Duration, data = DataWideD, var.equal = TRUE)



## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# Ex 3
t.test(NVIQD ~ Duration, data = DataWideNVIQ)
t.test(NVIQD ~ Duration, data = DataWideNVIQ, var.equal = TRUE)



## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# A variety of frequentist tests
FSIQ.t1 = t.test(FSIQD ~ Duration, data = DataWideD)
FSIQ.t2 = t.test(FSIQD ~ Duration, data = DataWideD, var.equal = T)
FSIQ.t3 = wilcox_test(FSIQD ~ Duration, data = DataWideD)
FSIQ.t4 = wilcox_test(FSIQD ~ Duration, data = DataWideD, distribution = "exact")
FSIQ.perm = oneway_test(FSIQD ~ Duration, data = DataWideD, distribution = "exact")
FSIQ.lm1 = lm(FSIQD ~ Duration, data = DataWideD)

## ------------------------------------------------------------------------
# p-value adjustment
# using p-values for other comparisons in this experiment
p = c(0.046, 0.444, 0.05)
pbon = p.adjust(p, method = c("bonferroni"))
pholm = p.adjust(p, method = c("holm"))
pfdr = p.adjust(p, method = c("fdr"))

## ------------------------------------------------------------------------
# Difference in means (mean = "expected value") between two groups 
sum_stat_delta_table$mean[1] - sum_stat_delta_table$mean[2]

## ------------------------------------------------------------------------
# Means and their CIs plot
mean_p1 = ggplot(DataWideD, aes(Duration, FSIQD, color = Duration)) + 
    stat_summary(geom = "point", fun.y = mean) + 
    stat_summary(geom = "errorbar", fun.data = mean_cl_normal, 
                 width = 0.1) +
    labs(y = expression(paste("Average ", Delta, " FSIQ")), x = "", 
         color = "Time to\ntreatment") +
    ylim(-30, 30) +
    theme(legend.position = "left")

# Difference in means with CI plot; first, create data frame w/ t-test output for CIs
FSIQ_ttest = t.test(FSIQD ~ Duration, data = DataWideD)

mean_diff_df = data.frame(Difference = "Difference",
    Mean = FSIQ_ttest$estimate[1] - FSIQ_ttest$estimate[2],
    Lower_CI = FSIQ_ttest$conf.int[1], Upper_CI = FSIQ_ttest$conf.int[2])

mean_p2 = ggplot(mean_diff_df, aes(Difference, Mean)) +
    geom_point() +
    ylim(-30, 30) +
    geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.1) +
    labs(y = expression(paste("Difference between average ", Delta, " FSIQ")), x = "") 

gridExtra::grid.arrange(mean_p1, mean_p2, ncol = 2, widths = c(0.75, 0.25))



## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# Ex 5
nv_mean_p1 = ggplot(DataWideNVIQ, aes(Duration, NVIQD, color = Duration)) + 
    stat_summary(geom = "point", fun.y = mean) + 
    stat_summary(geom = "errorbar", fun.data = mean_cl_normal, 
                 fun.args = list(conf.int = 0.90), width = 0.1) +
    labs(y = expression(paste("Average ", Delta, " Nonverbal IQ")), x = "", 
         color = "Time to\ntreatment") +
    ylim(-30, 30) +
    theme(legend.position = "left")

NVIQ_ttest = t.test(NVIQD ~ Duration, data = DataWideNVIQ, conf.level = 0.90)
nv_mean_diff_df = data.frame(Difference = "Difference",
    Mean = NVIQ_ttest$estimate[1] - NVIQ_ttest$estimate[2],
    Lower_CI = NVIQ_ttest$conf.int[1], Upper_CI = NVIQ_ttest$conf.int[2])

nv_mean_p2 = ggplot(nv_mean_diff_df, aes(Difference, Mean)) +
    geom_point() +
    ylim(-30, 30) +
    geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.1) +
    labs(y = expression(paste("Difference between average ", Delta, " FSIQ")), x = "") 

gridExtra::grid.arrange(nv_mean_p1, nv_mean_p2, ncol = 2, widths = c(0.75, 0.25))



## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# Subset into short and long data frames as BESTmcmc needs vectors
short = filter(DataWideD, Duration == "Short")
long = filter(DataWideD, Duration == "Long") 

# Run the MCMC analysis, can take a few mins
BEST_SvL_FSIQ = BESTmcmc(short$FSIQD, long$FSIQD)

## ------------------------------------------------------------------------
# Plot all results (not shown)
# plotAll(BEST_SvL_FSIQ)

# Plot difference in means result
plot(BEST_SvL_FSIQ, ROPE = c(-5, 5))

## ------------------------------------------------------------------------
# Summary of results
summary(BEST_SvL_FSIQ, ROPEm = c(-5, 5), compValeff = 0.5, ROPEeff = c(-0.25, 0.25))

## ------------------------------------------------------------------------
# BEST output, distribution of differences between means distributions
muDiff_SvL_FSIQ = BEST_SvL_FSIQ$mu1 - BEST_SvL_FSIQ$mu2

# Estimate of true difference
mean(muDiff_SvL_FSIQ)

# Probability of having any improvement being in the short duration group
mean(muDiff_SvL_FSIQ >= 0.01)

# Probability of having improvement of at least 5 being in the short duration group
mean(muDiff_SvL_FSIQ >= 5)

# HDI on the difference between means
hdi(BEST_SvL_FSIQ$mu1 - BEST_SvL_FSIQ$mu2)



## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# Ex 6
short_NVIQ = filter(DataWideNVIQ, Duration == "Short")
long_NVIQ = filter(DataWideNVIQ, Duration == "Long") 

BEST_SvL_NVIQ = BESTmcmc(short_NVIQ$NVIQD, long_NVIQ$NVIQD)

plot(BEST_SvL_NVIQ, credMass = 0.90)



## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# FSIQ alt and null models (same as implied by t-test)
FSIQ_lm1 = lm(FSIQD ~ Duration, data = DataWideD)
FSIQ_lm0 = lm(FSIQD ~ 1, data = DataWideD) 

# Make list of candidate models
FSIQ_candmodels = list(FSIQ_lm1, FSIQ_lm0)

# Names for alt and null models
mnames = c("H1", "H0")

## ------------------------------------------------------------------------
# AIC table
aictab(cand.set = FSIQ_candmodels, modnames = mnames)

## ------------------------------------------------------------------------
# Do model averaging on regression coefficient results
FSIQ_lm_intercept_avg = modavg(parm = "(Intercept)", cand.set = FSIQ_candmodels,
                            modnames = mnames)

FSIQ_lm_shorteffect_avg = modavg(parm = "DurationShort", cand.set =
                            FSIQ_candmodels, modnames = mnames)

FSIQ_lm_intercept_avg
FSIQ_lm_shorteffect_avg

## ------------------------------------------------------------------------
# Model averaged effect size
modavgEffect(cand.set = FSIQ_candmodels, modnames = mnames, newdata = 
             data.frame(Duration = c("Short", "Long")))



## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# Ex 7
NVIQ_lm1 = lm(NVIQD ~ Duration, data = DataWideNVIQ)
NVIQ_lm0 = lm(NVIQD ~ 1, data = DataWideNVIQ) 

# Make list of candidate models
NVIQ_candmodels = list(NVIQ_lm1, NVIQ_lm0)

# Names for alt and null models
mnames = c("H1", "H0")
aictab(cand.set = NVIQ_candmodels, modnames = mnames)

modavgEffect(cand.set = NVIQ_candmodels, modnames = mnames, newdata = 
             data.frame(Duration = c("Short", "Long")))



## ------------------------------------------------------------------------
## End of file
## ------------------------------------------------------------------------