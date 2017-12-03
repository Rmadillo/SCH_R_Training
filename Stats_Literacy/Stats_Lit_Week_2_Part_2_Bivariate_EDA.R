# Statistical Literacy for Biologists
# Week 1, Part 2: Univariate EDA


## ------------------------------------------------------------------------
library(psych)       # summary stats
library(VIM)         # visualize missing data
library(tidyverse)   # gpplot, dplyr, and tidyr packages
library(forcats)     # factor manipulation for tidy data
library(scales)      # additional scales for ggplot axes
library(GGally)      # scatter/bar/box/density plot matrix


## ------------------------------------------------------------------------
data(sbp, package = "sbpdata")


## ------------------------------------------------------------------------
# Create new data frame from actual data
bad_sbp = sbp

# Create about 15% NAs scattered around the new data frame
set.seed(54)
bad_sbp = as.data.frame(lapply(bad_sbp, function(cc) cc[sample(c(TRUE, NA), 
                prob = c(0.85, 0.15), size = length(cc), replace = TRUE) ]))

# Replace NAs with bad values in sbp and age
bad_sbp$sbp[is.na(bad_sbp$sbp)] = sample(8:20, sum(is.na(bad_sbp$sbp)), replace = TRUE)
bad_sbp$age[is.na(bad_sbp$age)] = sample(180:222, sum(is.na(bad_sbp$age)), replace = TRUE)

# Make a bad factor variable
bad_sbp$gender = sample(1:2, size = length(bad_sbp$Gender), replace = T, prob = c(0.30, 0.70))

# Add a duplicate line (repeat row 72 on new row 501)
bad_sbp[501, ] = bad_sbp[72, ]


## ------------------------------------------------------------------------
multi.hist(bad_sbp[ , sapply(bad_sbp, is.numeric)], nrow = 2, main = "")


## ------------------------------------------------------------------------
# Using boxplot as-is also shows factors, and doesn't account for variables' ranges
# Not run: boxplot(bad_sbp)

# Scaled, centered, and only numeric variables
boxplot(scale(bad_sbp[ , sapply(bad_sbp, is.numeric)]))


## ------------------------------------------------------------------------
# Looking for all zeros on one diagonal
table(bad_sbp$Gender, bad_sbp$gender)


## ------------------------------------------------------------------------
sort(unique(bad_sbp$age))


## ------------------------------------------------------------------------
bad_sbp[duplicated(bad_sbp) | duplicated(bad_sbp, fromLast = TRUE), ]


## ------------------------------------------------------------------------
matrixplot(bad_sbp)


## ------------------------------------------------------------------------
apply(bad_sbp, 2, function(x) any(is.na(x)))


## ------------------------------------------------------------------------
tabplot::tableplot(bad_sbp[ , c(1, 3, 5, 8:9)], pals = list("Paired"), sortCol = bmi)


## ------------------------------------------------------------------------
rm(bad_sbp)


## ------------------------------------------------------------------------
## ------------------------------------------------------------------------


## ------------------------------------------------------------------------
# Load the anscombe data and reshape into long form
aq = reshape(anscombe, varying = TRUE, sep="", direction = "long", timevar = "seriesname")

# Subset of the summary stats to the "Table 1" basics
describeBy(aq$x, aq$seriesname, skew = F, mat = T, digits = 2)[ , c(2, 4:6)]
describeBy(aq$y, aq$seriesname, skew = F, mat = T, digits = 2)[ , c(2, 4:6)]

ggplot(aq, aes(x, y)) +
    geom_point() +
    facet_wrap(~ seriesname)


## ------------------------------------------------------------------------
## ------------------------------------------------------------------------


#### Numeric/categorical EDA ####

## ------------------------------------------------------------------------
# Not run (run to see why histograms don't work well for this)
ggplot(sbp, aes(x = weight, fill = Treatment_Group)) +
    geom_histogram(alpha = 0.5) 

# Comparative density plot
ggplot(sbp, aes(x = weight, fill = Treatment_Group)) +
    geom_density(alpha = 0.5) 

# Comparative density plot with 3 groups
ggplot(sbp, aes(x = weight, fill = Exercise)) +
    geom_density(alpha = 0.5) 

# Faceted combination histogram/density plots
ggplot(sbp, aes(x = weight)) +
    geom_histogram(aes(y = ..density..), color = "gray80", binwidth = 10) +
    geom_density(color = "blue", fill = "blue", alpha = 0.2) +
    facet_wrap(~ Exercise, ncol = 1) + 
    ylab("") +
    theme_bw() +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) 

ggplot(sbp, aes(x = weight, fill = Treatment_Group)) +
    geom_density(alpha = 0.4) +
    facet_wrap(~ Exercise, ncol = 1) +
    labs(x = "Weight (lbs)", y = "Exercise Level", fill = "Treatment\nGroup") +
    theme_bw() + 
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) 


## ------------------------------------------------------------------------
# Boxplot
ggplot(sbp, aes(x = Exercise, y = weight)) +
    geom_boxplot() + 
    coord_flip()

# Boxplot with an additional grouping
ggplot(sbp, aes(x = Exercise, y = weight, fill = Treatment_Group)) +
    # The position_dodge width gives a little space between boxes
    geom_boxplot(position = position_dodge(width = 0.85)) + 
    coord_flip() 

library(ggridges)
ggplot(sbp, aes(x = weight, y = Exercise, fill = Treatment_Group)) +
    # These options relate to distance b/w groups and trim length
    geom_density_ridges2(scale = 1, rel_min_height = 0.01, alpha = 0.4) +
    # This helps scale the curves relative to the plot space
    scale_y_discrete(expand = c(0.025, 0.05))

ggplot(sbp, aes(x = Exercise, y = weight, fill = Treatment_Group)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + 
    labs(x = "Weight (lbs)", y = "Exercise Level", fill = "Treatment\nGroup") +
    coord_flip() 


#### Numeric/numeric EDA ####

## ------------------------------------------------------------------------
plot(sbp$weight, sbp$sbp)

# Scatterplot
ggplot(sbp, aes(weight, sbp)) +
    geom_point()

# Scatterplot with grouping
ggplot(sbp, aes(weight, sbp, color = Treatment_Group)) +
    geom_point()

# Scatterplot with grouping, semi-transparent points, and loess trends
ggplot(sbp, aes(weight, sbp, color = Treatment_Group)) +
    geom_point(alpha = 0.75) +
    geom_smooth()

# Scatterplot with grouping, semi-transparent points, loess, and sizing
ggplot(sbp, aes(weight, sbp, color = Treatment_Group)) +
    geom_point(aes(size = bmi), alpha = 0.75) +
    #scale_size_continuous(range = c(0.5, 3)) + # Change point size range
    geom_smooth()

# Interactive 3d scatterplot
rgl::plot3d(sbp$weight, sbp$sbp, sbp$bmi, radius = as.numeric(sbp$Married) * 2,
       col = as.numeric(sbp$Income) + 3, type = "s")


## ------------------------------------------------------------------------
# Base R scatterplot matrix
pairs(sbp[1:4])

# ggplot scatterplot matrix
ggscatmat(sbp, columns = c(1:4), color = "Treatment_Group", alpha = 0.5)

# Facet wrap scatterplot matrix
ggplot(sbp, aes(weight, sbp, color = Treatment_Group)) +
    labs(x = "Weight", y = "Systolic Blood Pressure", color = "Treatment Group") +
    geom_point(alpha = 0.5) +
    facet_wrap( ~ Stress) 

# Facet grid scatterplot matrix
ggplot(sbp, aes(weight, sbp, color = Treatment_Group)) +
    labs(x = "Weight", y = "Systolic Blood Pressure", color = "Treatment Group") +
    geom_point(alpha = 0.5) +
    facet_grid(Gender ~ Stress) 


## ------------------------------------------------------------------------

#### Categorical/categorical EDA ####

# Barplot with facets
ggplot(sbp, aes(Exercise)) +
    geom_bar() +
    facet_wrap(~ Treatment_Group, ncol = 1) + 
    coord_flip()

# Barplot with 2 groups and facets
ggplot(sbp, aes(Exercise, fill = Smoke)) +
    geom_bar() +
    facet_wrap(~ Treatment_Group, ncol = 1) + 
    coord_flip()

# Barplot with 3 groups and facets
ggplot(sbp, aes(Exercise, fill = Salt)) + 
    geom_bar(position = "dodge", width = 0.9) + 
    facet_wrap(~Treatment_Group, nrow = 1)

# Percentage barplot with 3 groups and facets
ggplot(sbp, aes(Income, fill = Salt)) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = percent) +
    facet_wrap(~ Treatment_Group, ncol = 1) +
    coord_flip()


## ------------------------------------------------------------------------
# Two variable mosaic plot
mosaicplot(~ Treatment_Group + Income, data = sbp, main = "")

# Three variable mosaic plot
mosaicplot(~ Treatment_Group + Income + Alcohol, data = sbp, main = "")

# Three variable mosaic plot with log linear model residuals
mosaicplot(~ Treatment_Group + Income + Alcohol, data = sbp, shade = T, 
           main = "", type = "deviance")


## ------------------------------------------------------------------------
library(vcd)

# For ease of plotting, we'll take a subset of the categorical variables
sbp_table = table(sbp[ , 9:12])

# Mosaic plot matrix
pairs(sbp_table, highlighting = 2)


#### Multi-data-type EDA ####

## ------------------------------------------------------------------------
# ggpairs matrix
ggpairs(sbp, columns = c(1:3, 8:9), mapping = aes(color = Treatment_Group, alpha = 0.5)) + 
    theme_bw()

# Interactive scatterplot matrix (factors will be numeric)
pairsD3::pairsD3(sbp[ , c(3, 5, 9, 12)], group = sbp$Treatment_Group, opacity = 0.7,
         tooltip = paste("Treatment Group: ", sbp$Treatment_Group,
                         "<br>Exercise: ", sbp$Exercise,
                         "<br>Age: ", sbp$age))

# Interactive parallel coordinates plot
parcoords::parcoords(sbp[ , c(12, 1:2, 9:11)], brushMode = "1d-axes-multi", reorderable = T,
           rownames = FALSE, color = list(colorBy = "Treatment_Group", colorScale =
                 htmlwidgets::JS("d3.scale.category10()") ))

# Tableplot
tabplot::tableplot(sbp, select = c(1, 3, 6, 10), subset=Treatment_Group, 
                   scales = "lin", nBins = 20)


## ------------------------------------------------------------------------
## ------------------------------------------------------------------------


#### Conditional summary statistics ####

## ------------------------------------------------------------------------
describeBy(sbp$weight, sbp$Treatment_Group, skew = FALSE)

describeBy(sbp$weight, list(sbp$Treatment_Group, sbp$Income), mat = T, 
           digits = 1, skew = FALSE)

describeBy(sbp[ , sapply(sbp, is.numeric)], sbp$Treatment_Group, 
           skew = FALSE)

## ------------------------------------------------------------------------
describeBy(sbp$weight, sbp$sbp >= 140, skew = FALSE)

describeBy(sbp$weight, sbp$sbp >= quantile(sbp$sbp, probs = 0.80), skew = FALSE)


## ------------------------------------------------------------------------
# Count tables
# Not run because it's ugly
table(sbp$Income, sbp$Alcohol, sbp$Treatment_Group)
xtabs(~ Income + Alcohol + Treatment_Group, data = sbp)

cross_tab = xtabs(~ Income + Alcohol + Treatment_Group, data = sbp)
ftable(cross_tab)

# Wrapped in ftable to make it all one line
ftable(addmargins(prop.table(xtabs(~ Income + Alcohol + Treatment_Group, data = sbp))))


## ------------------------------------------------------------------------
## ------------------------------------------------------------------------


#### Homework Answers ####

## ----hw_1_answer, include = FALSE----------------------------------------
# HW 1
ggplot(sbp, aes(bmi, fill = Smoke)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Treatment_Group, ncol = 1)

## ----hw_2_answer, include = FALSE----------------------------------------
# HW 2

# Plot from exercise 2
ggplot(sbp, aes(Treatment_Group, bmi, fill = Smoke)) +
  geom_boxplot(position = position_dodge(width = 0.85))

# Homework
ggplot(sbp, aes(Smoke, bmi, fill = Treatment_Group)) +
  geom_boxplot(position = position_dodge(width = 0.85))

## ----hw_3_answer, include = FALSE----------------------------------------
# HW 3

# Main answer
ggplot(sbp, aes(x = weight, y = bmi, color = Treatment_Group)) +
  geom_point(alpha = 0.7) +
  facet_grid(Income ~ Education, labeller = label_both) +
  theme_bw() 

# Bonus points
# You have to make a new factor, reversed (DON'T OVERWRITE THE OLD ONE!)
sbp$Income_Group = ordered(sbp$Income, levels = c("High", "Medium", "Low"))

# Then plot with new variable
ggplot(sbp, aes(x = weight, y = bmi, color = Treatment_Group)) +
  geom_point(alpha = 0.7) +
  facet_grid(Income_Group ~ Education, labeller = label_both) +
  theme_bw() 

## ----hw_4_answer, include = FALSE----------------------------------------
# HW 4
ggplot(sbp, aes(Education, fill = Married)) +
    geom_bar() +
    facet_grid(Treatment_Group ~ Smoke, scales = "free_y")

## ----hw_5_answer, include = FALSE----------------------------------------
# HW 5
mosaicplot(~ Married + Stress + Education, data = sbp, main = "")
mosaicplot(~ Education + Stress + Married, data = sbp, main = "")

## ----hw_6_answer, include = FALSE----------------------------------------
# HW 6
# There are two ways to do this
describeBy(sbp$age, sbp$sbp <= median(sbp$sbp))
describeBy(sbp$age, sbp$sbp <= quantile(sbp$sbp, probs = 0.50))

## ----hw_7_answer, include = FALSE----------------------------------------
# HW 7
cross_tab = xtabs(~ Married + Stress + Education, data = sbp)
ftable(cross_tab)
cross_tab_df = data.frame(ftable(cross_tab))


## ------------------------------------------------------------------------
## ------------------------------------------------------------------------


#### Exercise Answers ####

## ----ex_1_1_answer, eval = FALSE-----------------------------------------
tabplot::tableplot(sbp, sortCol = Smoke)

## ----ex_2_1_answer, eval = FALSE-----------------------------------------
ggplot(sbp, aes(x = bmi, fill = Smoke)) +
    geom_density(alpha = 0.5)

## ----ex_2_2_answer, eval = FALSE-----------------------------------------
ggplot(sbp, aes(Smoke, bmi, fill = Treatment_Group)) +
    geom_boxplot(position = position_dodge(width = 0.85))

ggplot(sbp, aes(x = bmi, y = Smoke, fill = Treatment_Group)) +
    geom_density_ridges2(scale = 0.9, rel_min_height = 0.01, alpha = 0.4) +
    scale_y_discrete(expand = c(0.025, 0.05))

## ----ex_3_1_answer, eval = FALSE-----------------------------------------
ggplot(sbp, aes(height, bmi, color = Gender)) +
    geom_smooth() +
    geom_point()

## ----ex_3_2_answer, eval = FALSE-----------------------------------------
ggplot(sbp, aes(height, weight, color = Treatment_Group)) +
    geom_point(aes(size = bmi)) +
    facet_grid(Smoke ~ Income, labeller = label_both)

## ----ex_4_1_answer, eval = FALSE-----------------------------------------
ggplot(sbp, aes(Education)) +
    geom_bar() +
    facet_wrap(~ Treatment_Group, ncol = 1)

## ----ex_5_1_answer, eval = FALSE-----------------------------------------
mosaicplot(~ Gender + Married + Alcohol, data = sbp, main = "")

## ----ex_5_2_answer, eval = FALSE-----------------------------------------
mosaicplot(~ Alcohol + Gender + Married, data = sbp, main = "")

## ----ex_6_1_answer, eval = FALSE-----------------------------------------
describeBy(sbp$height, sbp$Gender)

## ----ex_6_2_answer, eval = FALSE-----------------------------------------
describeBy(sbp$sbp, sbp$age <= quantile(sbp$age, probs = 0.75))

## ----ex_6_3_answer, eval = FALSE-----------------------------------------
cross_tab = xtabs(~ Education + Treatment_Group, Stress, data = sbp)
ftable(cross_tab)


## ------------------------------------------------------------------------
## ------------------------------------------------------------------------


#### End of file ####