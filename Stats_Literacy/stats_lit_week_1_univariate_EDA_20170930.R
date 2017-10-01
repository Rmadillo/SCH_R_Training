# Statistical Literacy for Biologists
# Week 1, Part 2: Univariate EDA

library(tidyverse)
library(gridExtra)

## ------------------------------------------------------------------------
# load some data
var1 = anscombe$y1
var2 = anscombe$y2

# calculate mean and sd
mean(var1); sd(var1)
mean(var2); sd(var2)

## ------------------------------------------------------------------------
p1 = ggplot(anscombe, aes(y1)) + geom_histogram(binwidth = 1, color = "gray")
p2 = ggplot(anscombe, aes(y2)) + geom_histogram(binwidth = 1, color = "gray")
grid.arrange(p1, p2, ncol = 2)

## ------------------------------------------------------------------------
# Load the data
data(sbp, package = "sbpdata")

# Let's look at the structure of this data
str(sbp)

## ------------------------------------------------------------------------
library(ggplot2)

## ------------------------------------------------------------------------
hist(sbp$weight)

## ------------------------------------------------------------------------
plot(density(sbp$weight))

## ------------------------------------------------------------------------
hist(sbp$weight, freq = F)
hist(sbp$weight, freq = F, breaks = 5)
hist(sbp$weight, freq = F, breaks = 20)

## ------------------------------------------------------------------------
hist(sbp$weight, freq = F, xlim = c(50, 275), ylim = c(0, 0.012))
lines(density(sbp$weight), col = "red")

hist(sbp$weight, freq = F, breaks = 5, xlim = c(50, 275), ylim = c(0, 0.012))
lines(density(sbp$weight), col = "red")

hist(sbp$weight, freq = F, breaks = 20, xlim = c(50, 275), ylim = c(0, 0.012))
lines(density(sbp$weight), col = "red")

## ------------------------------------------------------------------------
plot(ecdf(sbp$weight))
abline(v = quantile(sbp$weight, probs = c(0.25, 0.5, 0.75)), col = "red")

## ------------------------------------------------------------------------
barplot(table(sbp$Exercise))

## ------------------------------------------------------------------------
# We'll use the GGally package to create a "master EDA overview"
library(GGally)

# ggpairs is slow, so we'll only use a subset of data
ggpairs(data = sbp, columns = c(3:6), aes(color = Treatment_Group, alpha = 0.5))

## ------------------------------------------------------------------------
# Histogram
ggplot(data = sbp, aes(x = weight)) +
    geom_histogram()

## ------------------------------------------------------------------------
# Density
ggplot(sbp, aes(weight)) +
    geom_density()

## ------------------------------------------------------------------------
# Histogram and density, with a custom bin width and some color/fill options
# Note the ..density.. option, which puts the histogram on the density scale
ggplot(sbp, aes(weight)) +
    geom_histogram(aes(y = ..density..), color = "gray80", binwidth = 10) +
    geom_density(color = "blue", fill = "blue", alpha = 0.2)

## ------------------------------------------------------------------------
# Histogram and density, with a few color and fill options, and a plain theme
# and the actual density value is meaningless, so we'll remove it
ggplot(sbp, aes(weight)) +
      geom_histogram(aes(y = ..density..), color = "gray80", binwidth = 10) +
      geom_density(color = "transparent", fill = "blue", alpha = 0.3) +
      theme_bw() + 
      # note: you must use theme options after theme type
      theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) 

## ------------------------------------------------------------------------
ggplot(sbp, aes(x = weight)) +
    stat_ecdf() 

## ------------------------------------------------------------------------
# First, order the data by the variable you want to plot
sbp = arrange(sbp, weight)

# Then create a ggplot object
ecdf_plot_weight = ggplot(sbp, aes(x = weight)) +
    stat_ecdf()

# Finally, pass the ggplot object to another package, e.g., plotly
plotly::ggplotly(ecdf_plot_weight)

## ------------------------------------------------------------------------
# Barplot
ggplot(sbp, aes(x = Exercise)) +
    geom_bar()

## ------------------------------------------------------------------------
# Barplot
ggplot(sbp, aes(x = Exercise)) +
    geom_bar() +
    coord_flip()

## ------------------------------------------------------------------------
# We need to load the forcats library (helps with categorical variables) first
library(forcats)

# Barplot with values ordered by count, from origin
# fct_infreq orders by frequency
ggplot(sbp, aes(x = fct_infreq(Exercise))) +
    geom_bar() +
    xlab("Exercise Category") 

## ------------------------------------------------------------------------
# fct_rev reverses the order so most frequent is on top
ggplot(sbp, aes(x = fct_rev(fct_infreq(Exercise)), y = ..count..)) +
    # Make a very skinny bar for the stem
    geom_bar(stat = "count", width = 0.02) +
    geom_point(stat = "count", size = 4, color = "steelblue") + 
    coord_flip() +
    # We flipped the graph but ggplot uses original aes
    ylim(0, 200) +
    xlab("Exercise Category")

## ------------------------------------------------------------------------
# This will appear in your current working directory; use getwd() if needed
ggsave(filename = "ecdf_plot_weight.png", 
       plot = ecdf_plot_weight, 
       dpi = 600, 
       width = 4, 
       height = 4, 
       units = "in")

## ------------------------------------------------------------------------
summary(sbp)

## ------------------------------------------------------------------------
quantile(sbp$age, probs = c(0.025, 0.20, 0.50, 0.80, 0.975))

## ------------------------------------------------------------------------
what_quantile = ecdf(sbp$weight)
what_quantile(134)

## ------------------------------------------------------------------------
# Load psych package
library(psych)

## ------------------------------------------------------------------------
# Summary stats but wrong results for categorical data 
describe(sbp)

# Summary stats for numeric variables only
describe(sbp[ , sapply(sbp, is.numeric)])

## ------------------------------------------------------------------------
table(sbp$Exercise)
prop.table(table(sbp$Exercise))

## ------------------------------------------------------------------------
addmargins(table(sbp$Exercise))

## ------------------------------------------------------------------------
# Create a describe object
sbp_summary = describe(sbp[ , sapply(sbp, is.numeric)], skew = FALSE, IQR = TRUE)

# Output describe object to a table, rounded to 1 place
# See output results in the pdf
# knitr::kable(sbp_summary, digits = 1)

## ------------------------------------------------------------------------
# Function to calculate mode(s)
Mode <- function(x, na.rm = FALSE) {
    if(na.rm){
      x = x[!is.na(x)]
    }
  x <- sort(x)  
  u <- unique(x)
  y <- lapply(u, function(y) length(x[x==y]))
  u[which(unlist(y) == max(unlist(y)))]
} 

## ------------------------------------------------------------------------
Mode(sbp$bmi)

## ------------------------------------------------------------------------
Mode(sbp$Exercise)

## ------------------------------------------------------------------------
# Calculate density and place into a data frame
weight_dens = data.frame(weight = density(sbp$weight)$x, 
                         density = density(sbp$weight)$y)

# Obtain the weight value at which density is maximized
weight_dens[which.max(weight_dens[, 2]), ]

## ------------------------------------------------------------------------
ggplot(sbp, aes(weight)) +
    geom_histogram(aes(y = ..density..), color = "gray80", binwidth = 10) +
    geom_density(color = "transparent", fill = "blue", alpha = 0.3) +
    geom_vline(aes(xintercept = weight_dens[which.max(weight_dens[, 2]), 1]), 
             color = "orange") + 
    theme_bw() + 
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) 

## ------------------------------------------------------------------------
# Load tableone package
library(tableone)

# Create a variable list for Table 1
table_vars = c("age", "height", "weight", "bmi", "Gender", "Married",
               "Smoke", "Exercise", "Stress")

# Specify the categorical variables
fct_vars = c("Gender", "Married", "Smoke", "Exercise", "Stress")

# Create the table, without using NHST, stratified by treatment group
table_1_lies = CreateTableOne(table_vars, sbp, fct_vars,
                strata = "Treatment_Group", test = FALSE)

# Put the table into an object so it will show up in Rmd files
table_1_lies_object = print(table_1_lies, showAllLevels = TRUE, quote = FALSE,
                noSpaces = TRUE, printToggle = FALSE)

# Print the table
# See table in the pdf file
# knitr::kable(table_1_lies_object, align = 'crr')

## ------------------------------------------------------------------------
# Create some random Poisson count data with mean = 1
set.seed(54)
tumor_count = data.frame(x = rpois(30, 1))

# Calculate mean
mean(tumor_count$x)

# Calculate normal distribution standard deviation
sd(tumor_count$x)

## ------------------------------------------------------------------------
# The implied distribution for Normal(1.1, 1.06)
ggplot(tumor_count, aes(x = x)) +
  xlim(-3, 5) +
  geom_vline(aes(xintercept = mean(tumor_count$x)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = mean(tumor_count$x) - (2 * sd(tumor_count$x))), 
             color = "red", linetype = "dotted") +
  geom_vline(aes(xintercept = mean(tumor_count$x) + (2 * sd(tumor_count$x))), 
             color = "red", linetype = "dotted") +
  stat_function(fun = dnorm, 
                color = "blue",
                args = list(mean = mean(tumor_count$x), 
                       sd = sd(tumor_count$x))) + 
  annotate("text", x = mean(tumor_count$x) - (2 * sd(tumor_count$x)), 
           y = max(density(tumor_count$x)$y), label = "-2sd", fontface = "italic", 
           color = "darkred") +
  annotate("text", x = mean(tumor_count$x) + (2 * sd(tumor_count$x)), 
           y = max(density(tumor_count$x)$y), label = "+2sd", fontface = "italic", 
           color = "darkred") +
  geom_label(x = mean(tumor_count$x), y = 0, label = "bar(x)", parse = TRUE, 
             color = "darkred") + 
  theme_bw() + 
  xlab("Tumor Count") + 
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) 

## ------------------------------------------------------------------------
# Same plot as above, with the actual data added
ggplot(tumor_count, aes(x = x)) +
  xlim(-3, 5) +
  geom_histogram(aes(y = ..density..), color = "gray80", binwidth = 1) +
  geom_vline(aes(xintercept = mean(tumor_count$x)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = mean(tumor_count$x) - (2 * sd(tumor_count$x))),
             color = "red", linetype = "dotted") +
  geom_vline(aes(xintercept = mean(tumor_count$x) + (2 * sd(tumor_count$x))),
             color = "red", linetype = "dotted") +
  stat_function(fun = dnorm, color = "blue",
                args = list(mean = mean(tumor_count$x), 
                       sd = sd(tumor_count$x))) + 
  annotate("text", x = mean(tumor_count$x) - (2 * sd(tumor_count$x)),
           y = max(density(tumor_count$x)$y), label = "-2sd", fontface = "italic",
           color = "darkred") +
  annotate("text", x = mean(tumor_count$x) + (2 * sd(tumor_count$x)),
           y = max(density(tumor_count$x)$y), label = "+2sd", fontface = "italic",
           color = "darkred") +
  geom_label(x = mean(tumor_count$x), y = 0, label = "bar(x)", parse = TRUE,
             color = "darkred") + 
  theme_bw() + 
  xlab("Tumor Count") + 
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) 

## ------------------------------------------------------------------------
mean(sbp$age)
sd(sbp$age)

## ------------------------------------------------------------------------
# Histogram/density with normal curve overplotted
ggplot(sbp, aes(x = age)) +
    geom_histogram(aes(y = ..density..), color = "gray80", binwidth = 2) +
    geom_density(color = "transparent", fill = "blue", alpha = 0.3) +
    stat_function(fun = dnorm, color = "red",
        args = list(mean = mean(sbp$age), sd = sd(sbp$age))) + 
    theme_bw() + 
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) 

# quantiles for 2.5, 33, 67, 95
quantile(sbp$age, probs = c(0.025, 0.33, 0.67, 0.975))

## ------------------------------------------------------------------------
# ecdf with normal cdf overplotted
age_cdf = data.frame(x = seq(15, 75, 1), norm_cdf = pnorm(seq(15, 75, 1), 
                    mean = mean(sbp$age), sd = sd(sbp$age)))

ggplot(sbp, aes(x = age)) +
    geom_line(data = age_cdf, aes(x = x, y = norm_cdf), color = "red") + 
    stat_ecdf(color="blue") +
    theme_bw()

## ------------------------------------------------------------------------
# Load dplyr for its filter command (like SQL's "where")
library(dplyr)

# Create ggplot objects and save to local file
p1 = ggplot(filter(sbp, Treatment_Group == "Control"), aes(x = age)) +
  geom_histogram(fill="darkblue", color = "white", size = 0.1, binwidth = 5) +
  xlim(10, 70) +
  theme_void() 
ggsave("p1.png", p1, width = 0.25, height = 0.15, units = "in")

p2 = ggplot(filter(sbp, Treatment_Group == "Treatment"), aes(x = age)) +
  geom_histogram(fill="darkblue", color = "white", size = 0.1, binwidth = 5) +
  xlim(10, 70) +
  theme_void()
ggsave("p2.png", p2, width = 0.25, height = 0.15, units = "in")

p3 = ggplot(filter(sbp, Treatment_Group == "Control"), aes(x = age)) +
  geom_density(fill="darkblue", color = "white") +
  xlim(10, 70) +
  theme_void() 
ggsave("p3.png", p3, width = 0.25, height = 0.15, units = "in")

p4 = ggplot(filter(sbp, Treatment_Group == "Treatment"), aes(x = age)) +
  geom_density(fill="darkblue", color = "white") +
  xlim(10, 70) +
  theme_void()
ggsave("p4.png", p4, width = 0.25, height = 0.15, units = "in")

p5 = ggplot(filter(sbp, Treatment_Group == "Control"), aes(x = age)) +
  geom_histogram(aes(y = ..density..), color = "white", fill="darkblue", 
                 size = 0.1, binwidth = 5) +
  geom_density(fill="#DEEBF7", color = "black", size = 0.1, alpha = 0.3) +
  xlim(10, 70) +
  theme_void() 
ggsave("p5.png", p5, width = 0.25, height = 0.15, units = "in")

p6 = ggplot(filter(sbp, Treatment_Group == "Treatment"), aes(x = age)) +
  geom_histogram(aes(y = ..density..), color = "white", fill="darkblue",
                 size = 0.1, binwidth = 5) +
  geom_density(fill="#DEEBF7", color = "black", size = 0.1, alpha = 0.2) +
  xlim(10, 70) +
  theme_void()
ggsave("p6.png", p6, width = 0.25, height = 0.15, units = "in")

p7 = ggplot(filter(sbp, Treatment_Group == "Control"), 
            aes(x = factor(1), fill = Exercise)) +
  geom_bar() +
  theme_void() + 
  theme(legend.position = "none") + 
  scale_fill_brewer() # 3 colors hex codes: brewer.pal(3, "Blues")
ggsave("p7.png", p7, width = 0.10, height = 0.25, units = "in")

p8 = ggplot(filter(sbp, Treatment_Group == "Treatment"), 
            aes(x = factor(1), fill = Exercise)) +
  geom_bar() +
  theme_void() + 
  theme(legend.position = "none") + 
  scale_fill_brewer()
ggsave("p8.png", p8, width = 0.10, height = 0.25, units = "in")

p9 = ggplot(filter(sbp, Treatment_Group == "Control"), 
            aes(x = factor(1), fill = Exercise)) +
  geom_bar(width = 1) +
  theme_void() + 
  theme(legend.position = "none") + 
  coord_polar(theta = "y") +
  scale_fill_brewer()
ggsave("p9.png", p9, width = 0.5, height = 0.5, units = "in")

p10 = ggplot(filter(sbp, Treatment_Group == "Treatment"), 
             aes(x = factor(1), fill = Exercise)) +
  geom_bar(width = 1) +
  theme_void() + 
  theme(legend.position = "none") + 
  coord_polar(theta = "y") +
  scale_fill_brewer()
ggsave("p10.png", p10, width = 0.5, height = 0.5, units = "in")

## ------------------------------------------------------------------------
# You can see the summary stats and object structure using summary, but str helps
# Not run: 
str(table_1_lies)
summary(table_1_lies)

## ----eval=FALSE----------------------------------------------------------
## |  | level | Control |    | Treatment |    |
## |------------------------|:------|--------:|:--:|----------:|:--:|
## | *Continuous variable examples*  |  |  |  |  |  |
## | age (mean (sd)) *histogram only* | | `r round(table_1_lies$ContTable$Control[1,4], 1)`  (`r round(table_1_lies$ContTable$Control[1,5], 2)`) | ![](p1.png) | `r round(table_1_lies$ContTable$Treatment[1,4], 1)`  (`r round(table_1_lies$ContTable$Treatment[1,5], 2)`)  | ![](p2.png) |
## | age (mean (sd)) *density only* | | `r round(table_1_lies$ContTable$Control[1,4], 1)`  (`r round(table_1_lies$ContTable$Control[1,5], 2)`) | ![](p3.png) | `r round(table_1_lies$ContTable$Treatment[1,4], 1)`  (`r round(table_1_lies$ContTable$Treatment[1,5], 2)`)  | ![](p4.png) |
## | age (mean (sd)) *both* |  | `r round(table_1_lies$ContTable$Control[1,4], 1)`  (`r round(table_1_lies$ContTable$Control[1,5], 2)`) | ![](p5.png) | `r round(table_1_lies$ContTable$Treatment[1,4], 1)`  (`r round(table_1_lies$ContTable$Treatment[1,5], 2)`)  | ![](p6.png) |
## | *Categorical variable examples*  |  |  |  |  |  |
## | exercise (count (%)) | 1 (Low) <br> 2 (Medium) <br> 3 (High) | `r table_1_lies$CatTable$Control$exercise[1,5]`  (`r round(table_1_lies$CatTable$Control$exercise[1,6], 1)`) <br> `r table_1_lies$CatTable$Control$exercise[2,5]`  (`r round(table_1_lies$CatTable$Control$exercise[2,6], 1)`) <br> `r table_1_lies$CatTable$Control$exercise[3,5]` (`r round(table_1_lies$CatTable$Control$exercise[3,6], 1)`)  <br> | ![](p7.png) | `r table_1_lies$CatTable$Treatment$exercise[1,5]`  (`r round(table_1_lies$CatTable$Treatment$exercise[1,6], 1)`)  <br> `r table_1_lies$CatTable$Treatment$exercise[2,5]`  (`r round(table_1_lies$CatTable$Treatment$exercise[2,6], 1)`) <br> `r table_1_lies$CatTable$Treatment$exercise[3,5]`  (`r round(table_1_lies$CatTable$Treatment$exercise[3,6], 1)`)  <br> | ![](p8.png) |
## | exercise (count (%)) | 1 (Low) <br> 2 (Medium) <br> 3 (High) | `r table_1_lies$CatTable$Control$exercise[1,5]`  (`r round(table_1_lies$CatTable$Control$exercise[1,6], 1)`) <br> `r table_1_lies$CatTable$Control$exercise[2,5]`  (`r round(table_1_lies$CatTable$Control$exercise[2,6], 1)`) <br> `r table_1_lies$CatTable$Control$exercise[3,5]` (`r round(table_1_lies$CatTable$Control$exercise[3,6], 1)`)  <br> | ![](p9.png) | `r table_1_lies$CatTable$Treatment$exercise[1,5]`  (`r round(table_1_lies$CatTable$Treatment$exercise[1,6], 1)`)  <br> `r table_1_lies$CatTable$Treatment$exercise[2,5]`  (`r round(table_1_lies$CatTable$Treatment$exercise[2,6], 1)`) <br> `r table_1_lies$CatTable$Treatment$exercise[3,5]`  (`r round(table_1_lies$CatTable$Treatment$exercise[3,6], 1)`)  <br> | ![](p10.png) |

## ------------------------------------------------------------------------
ggplot(sbp, aes(x = sbp)) +
  geom_histogram(aes(y = ..density..), color = "gray95") +
  geom_density(fill = "#A30134", alpha = 0.2)

## ------------------------------------------------------------------------
ggplot(sbp, aes(x = fct_infreq(Salt))) +
  geom_bar() + 
  coord_flip() 

## ------------------------------------------------------------------------
addmargins(table(sbp$Salt))

## ------------------------------------------------------------------------
describe(sbp$sbp)

## ------------------------------------------------------------------------
sbp_dens = data.frame(sbp = density(sbp$sbp)$x, 
                      density = density(sbp$sbp)$y)

ggplot(sbp, aes(x = sbp)) +
    geom_histogram(aes(y = ..density..), color = "gray95") +
    geom_density(fill = "#A30134", alpha = 0.2) +
    geom_vline(aes(xintercept = mean(sbp)), color = "darkblue") + 
    geom_vline(aes(xintercept = median(sbp)), color = "darkgreen") + 
    geom_vline(aes(xintercept = sbp_dens[which.max(sbp_dens[, 2]), 1]), color = "orange") 

## ------------------------------------------------------------------------
multi.hist(sbp[ , sapply(sbp, is.numeric)])

## ------------------------------------------------------------------------
library(tidyverse)
library(psych)

neuro = read.csv("https://raw.githubusercontent.com/Rmadillo/SCH_R_Training/master/Diff_Inf/Shurtleffetal2015_episurg_data.csv", header = TRUE)

describe(neuro$FSIQ)

addmargins(table(neuro$Side))

ggplot(neuro, aes(x = FSIQ)) +
    geom_histogram(aes(y = ..density..), color = "gray95", binwidth = 5) +
    geom_density(fill = "#0076C0", alpha = 0.5) + 
    theme_bw()

ggplot(neuro, aes(x = Side)) +
    geom_bar() +
    coord_flip() +
    theme_bw()

## ------------------------------------------------------------------------
library(tableone)

table_vars = c("Duration", "Side", "Phase", "FSIQ", "Verbal", "Nonverbal")

fct_vars = c("Duration", "Side", "Phase")

neuro_table_1 = CreateTableOne(table_vars, , neuro, fct_vars, test = FALSE)

neuro_table_2 = print(neuro_table_1, showAllLevels = TRUE, quote = FALSE,
                noSpaces = TRUE, printToggle = FALSE)

# knitr::kable(neuro_table_2, align = 'crr')

## ------------------------------------------------------------------------
# No graphics
library(ggplot2)

table_vars = c("Duration", "Side", "Phase", "FSIQ", "Verbal", "Nonverbal")

fct_vars = c("Duration", "Side", "Phase")

neuro_table_1 = CreateTableOne(table_vars, strata = "Duration", neuro, 
                fct_vars, test = FALSE)

neuro_table_2 = print(neuro_table_1, showAllLevels = TRUE, quote = FALSE,
                noSpaces = TRUE, printToggle = FALSE)

# knitr::kable(neuro_table_2, align = 'crr')

## ------------------------------------------------------------------------
# Using graphics
library(dplyr)

# Create ggplot objects
hwp1 = ggplot(filter(neuro, Duration == "Short"), aes(x = FSIQ)) +
  geom_histogram(fill="darkblue", color = "white", size = 0.1, binwidth = 5) +
  xlim(75, 145) +
  theme_void() 
ggsave("hwp1.png", hwp1, width = 0.25, height = 0.15, units = "in")

hwp2 = ggplot(filter(neuro, Duration == "Long"), aes(x = FSIQ)) +
  geom_density(fill="darkblue", color = "white") +
  xlim(75, 145) +
  theme_void() 
ggsave("hwp2.png", hwp2, width = 0.25, height = 0.15, units = "in")

hwp7 = ggplot(filter(neuro, Duration == "Short"), 
            aes(x = factor(1), fill = Side)) +
  geom_bar() +
  theme_void() + 
  theme(legend.position = "none") + 
  scale_fill_brewer() # 3 colors hex codes: brewer.pal(3, "Blues")
ggsave("hwp7.png", hwp7, width = 0.10, height = 0.25, units = "in")

hwp8 = ggplot(filter(neuro, Duration == "Long"), 
            aes(x = factor(1), fill = Side)) +
  geom_bar() +
  theme_void() + 
  theme(legend.position = "none") + 
  scale_fill_brewer() # 3 colors hex codes: brewer.pal(3, "Blues")
ggsave("hwp8.png", hwp8, width = 0.10, height = 0.25, units = "in")

# etc., etc. 

## ----eval = FALSE, include = FALSE---------------------------------------
## # Cut and paste into an Rmd to create a table w/ graphs
## |  | level | Control |    | Treatment |    |
## |------------------------|:------|--------:|:--:|----------:|:--:|
## | FSIQ (mean (sd)) | | `r round(neuro_table_1$ContTable$Long[1,4], 1)`  (`r round(neuro_table_1$ContTable$Long[1,5], 2)`) | ![](hwp1.png) | `r round(neuro_table_1$ContTable$Short[1,4], 1)`  (`r round(neuro_table_1$ContTable$Short[1,5], 2)`)  | ![](hwp2.png) |
## | Side (count (%)) | Left <br> Right | `r neuro_table_1$CatTable$Long$Side[1,5]`  (`r round(neuro_table_1$CatTable$Long$Side[1,6], 1)`) <br> `r neuro_table_1$CatTable$Long$Side[2,5]`  (`r round(neuro_table_1$CatTable$Long$Side[2,6], 1)`) <br> | ![](hwp7.png) | `r neuro_table_1$CatTable$Short$Side[1,5]`  (`r round(neuro_table_1$CatTable$Short$Side[1,6], 1)`)  <br> `r neuro_table_1$CatTable$Short$Side[2,5]`  (`r round(neuro_table_1$CatTable$Short$Side[2,6], 1)`) <br> | ![](hwp8.png) |

