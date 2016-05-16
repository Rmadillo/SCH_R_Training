#############################################################
# Intro R / EDA Workshop
# Dwight Barry, PhD | Enterprise Analytics
# 16 May 2016 - Version 1.0
#############################################################


# Install packages in bulk
#install.packages(c("GGally", "psych", "vcd", "ggplot2", "scales", "beanplot", "ggExtra", "likert", "tolerance", "gridExtra", "VIM", "VIMGUI", "simpleboot", "segmented", "quantreg", "strucchange", "reshape2", "dplyr", "qcc", "htmlwidgets", "dygraphs", "DT", "hexbin", "forecast", "cdcfluview", "httr", "zoo", "tidyr", "asbio","TSA","devtools"))

# Get working directory
getwd()

# Change working directory
# setwd("C:/Users/DBARR1/Documents/R/R_Code")
# setwd("~/R/R_Code")
# setwd("./R/R_Code")
# setwd("\\\\childrens\\files\\users01\\dbarr1\\R")

# Assignment works with <- and = 
x = 4
y <- 4

# Show objects in the environment
ls()

# Show files in the working directory
dir()


##### Getting and manipulting data ##########################

# Download file from UCI ML library to working directory
# Metadata at https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00275/Bike-Sharing-Dataset.zip", "Bike-Sharing-Dataset.zip")

# Unzip file and look at it 
bike_share_daily = read.table(unz("Bike-Sharing-Dataset.zip", "day.csv"), sep=",", header=TRUE)

# Already unzipped?
# bike_share_daily = read.table("day.csv", sep=",", header=TRUE)

# Get help
?read.table

# Evaluate structure
str(bike_share_daily)

# Get summary stats for data frame
summary(bike_share_daily)

# Get summary stats for column
summary(bike_share_daily$weekday)


##### Cleaning #####

# Converting between data types using 'as.'
# as.Date
# as.POSIXct     # best for date/time
# as.character
# as.numeric
# as.factor
# as.ordered

# Factor to Date
bike_share_daily$dteday = as.Date(bike_share_daily$dteday, format="%Y-%m-%d")

# Help for Date function
?Date

# Details on date/time conversions
?strftime

# Convert Integers to Factors, as a group, using "list" apply
bike_share_daily[,3:7] = lapply(bike_share_daily[,3:7], factor)

# It can sometimes be easier to specify when you load the data
bike_share_daily = read.table(unz("Bike-Sharing-Dataset.zip", "day.csv"), sep=",", header=TRUE, colClasses = c("character", "Date", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "numeric", "numeric", "numeric", "numeric", "integer", "integer", "integer"))

str(bike_share_daily)

summary(bike_share_daily)

# Convert Factors to Ordered Factors		
levels(bike_share_daily$season) = c("Winter", "Spring", "Summer", "Fall")
levels(bike_share_daily$workingday) = c("No", "Yes")
levels(bike_share_daily$holiday) = c("No", "Yes")
bike_share_daily$mnth = ordered(bike_share_daily$mnth, 1:12)
levels(bike_share_daily$mnth) = c(month.abb)
levels(bike_share_daily$yr) = c(2011, 2012)

str(bike_share_daily)

# Write data to csv in working directory
write.csv(bike_share_daily, "bike_share_daily.csv", row.names=FALSE)

# Essential R vocab
# http://adv-r.had.co.nz/Vocabulary.html


##### Exploratory Data Analysis #############################

# INTERLUDE: Why EDA? An example from SCH [Andy Cooper, PhD]

#### Creating summary plots ####

require(ggplot2)
require(GGally)
require(scales)

### Everything at once: `ggpairs`
# WARNING: Can be slow with large data sets
# and/or when slogging through SCH pipes



ggpairs(data=bike_share_daily, columns=c(14:15, 10, 13, 3, 7), title="Daily Bike Sharing Data", axisLabels="show", color="season")

# Error! Troubleshoot by looking at help file
?ggpairs

# Re-run (and stop)
ggpairs(data=bike_share_daily, columns=c(14:15, 10, 13, 3, 7), title="Daily Bike Sharing Data", axisLabels="show", mapping="season")

# Still not showing color
# Googled, find the vignette https://cran.r-project.org/web/packages/GGally/vignettes/ggpairs.html
ggpairs(data=bike_share_daily, columns=c(14:15, 10, 13, 3, 7), title="Daily Bike Sharing Data", axisLabels="show", mapping=aes(color = season, alpha=0.3))

### Create histograms of all numeric variables in one plot

require(psych)

# sapply: "simplified" apply
# select only numeric variables
multi.hist(bike_share_daily[,sapply(bike_share_daily, is.numeric)])

### Pairs plot

pairs(bike_share_daily[,sapply(bike_share_daily, is.numeric)])

# A better "pairs" plot is in the psych package

pairs.panels(bike_share_daily[,sapply(bike_share_daily, is.numeric)], ellipses=T, pch=12, las=2, cex.axis=0.7, method="kendall")

# Mosaic plots: "Scatterplots" for categorical data

mosaicplot(table(bike_share_daily[,c(8,9,3)]))


# INTERLUDE: The Grammar of Graphics

# https://www.rstudio.com/resources/cheatsheets/

## Plotting univariate distributions

### Histograms and density plots

ggplot(bike_share_daily, aes(casual)) +
  geom_histogram()

ggplot(bike_share_daily, aes(casual)) +
  geom_histogram(color="black")

ggplot(bike_share_daily, aes(casual)) +
  geom_histogram(color="gray50", fill="blue", alpha=0.4, binwidth=250) +
  xlab("Casual Use") +
  theme_bw()

# check help to see other built-in themes
?theme_bw

ggplot(bike_share_daily, aes(casual)) +
  geom_density(col="blue", fill="blue", alpha=0.4) +
  xlab("Casual Use") +
  theme_minimal()

ggplot(bike_share_daily, aes(casual)) +
  ylab("density and count") +
  xlab("Casual Use") +
  geom_histogram(aes(y=..density..), col="blue", fill="blue", alpha=0.3, binwidth=250) +
  geom_density(col="blue", fill="blue", alpha=0.2) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

# Got it how you like it? Save image to working directory
# Can use 1) "Export" button in Plots tab, or
# 2) Zoom then right-click, or
# 3) Save to a local file directly, as follows
# Works with all plots
png("densohisto_1.png", width=10, height=7, units="in", res=300)
ggplot(bike_share_daily, aes(casual)) +
  ylab("density and count") +
  xlab("Casual Use") +
  geom_histogram(aes(y=..density..), col="blue", fill="blue", alpha=0.3, binwidth=250) +
  geom_density(col="blue", fill="blue", alpha=0.2) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
dev.off()

# Works with ggplots, saves last ggplot created
ggsave("densohisto_2.png", width=10, height=7, units="in", dpi=300)

# theme provides customization for just about anything
# ?theme

### Bar and dot plots

ggplot(bike_share_daily, aes(weathersit)) +
  geom_bar(col="blue", fill="blue", alpha=0.3) +
  xlab("Weather Pattern") +
  scale_x_discrete(breaks=c(1, 2, 3), labels=c("Clear", "Cloudy/Rainy", "Stormy")) +
  theme_bw()

ggplot(bike_share_daily, aes(x=weathersit, y=..count..)) +
  geom_bar(stat="count", width=0.01) +
  geom_point(stat = "count", size=4, pch=21, fill="darkblue") +
  xlab("Weather Pattern") +
  scale_x_discrete(breaks=c(1, 2, 3), labels=c("Clear", "Cloudy/Rainy", "Stormy")) +
  coord_flip() +
  theme_bw()

# Mosaic plots

library(vcd)
mosaic(table(bike_share_daily[,c(3,9)]), shade=T)
mosaic(table(bike_share_daily[,c(8,9,3)]))

### Plotting multiple univariate distributions with faceting

ggplot(bike_share_daily, aes(casual, fill=season)) +
  geom_histogram(aes(y = ..density..), alpha=0.2, color="gray50") +
  geom_density(alpha=0.5, size=0.5) +
  facet_wrap(~season) +
  theme_light() +
  xlab("Daily Bike Use Count") +
  ylab("") +
  theme(legend.position="none") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title = element_text(size=9, face=2, color="gray30"), axis.title.x = element_text(vjust=-0.5))

ggplot(bike_share_daily, aes(weathersit, fill=season)) +
  geom_bar(alpha=0.5) +
  xlab("") +
  ylab("Number of Days") +
  scale_x_discrete(breaks=c(1, 2, 3), labels=c("Clear", "Cloudy/Rainy", "Stormy")) +
  coord_flip() +
  facet_wrap(~season, ncol=1) +
  theme_light()

## Plotting bivariate and comparative distributions

### Double density plots

ggplot(bike_share_daily, aes(casual, fill=workingday, color=workingday)) +
  geom_density(alpha=0.5) +
  xlab("Daily Casual Bike Use Count") +
  ylab("") +
  scale_fill_discrete(name="Work Day?") +
  scale_color_discrete(name="Work Day?") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position="top")

### Boxplots

ggplot(bike_share_daily, aes(mnth, casual, fill=workingday)) +
  xlab("Month") +
  ylab("Daily Casual Bike Use Count") +
  geom_boxplot() +
  theme_minimal() +
  scale_fill_discrete(name="Work Day?") +
  scale_color_discrete(name="Work Day?") +
  theme(legend.position="top")

### Beanplots

require(beanplot)

beanplot(casual ~ mnth, data = bike_share_daily, side="second", overallline="median", what=c(1,1,1,0), col=c("gray70", "transparent", "transparent", "steelblue"), ylab = "Month", xlab = "Daily Casual Bike Use Count", horizontal=TRUE)

# boxplot-style orientation
beanplot(casual ~ mnth, data = bike_share_daily, side="first", overallline="median", what=c(1,1,1,0), col=c("gray70", "transparent", "transparent", "blue"), xlab = "Month", ylab = "Daily Casual Bike Use Count")

# look at help file for details
# ?beanplot


### Scatterplots

ggplot(bike_share_daily, aes(x=atemp, y=casual)) +
  xlab("Daily Mean Normalized Air Temperature") +
  ylab("Number of Total Casual Bike Uses") +
  geom_point(col="gray50") +
  theme_bw()

# scatterplot with density
ggplot(bike_share_daily, aes(x=atemp, y=casual)) +
  xlab("Daily Mean Normalized Air Temperature") +
  ylab("Number of Total Casual Bike Uses") +
  geom_point(col="gray50") +
  geom_density2d() +
  theme_bw()

# scatterplot with hexbins (not as useful here)
ggplot(bike_share_daily, aes(x=atemp, y=casual)) +
  xlab("Daily Mean Normalized Air Temperature") +
  ylab("Number of Total Casual Bike Uses") +
  geom_hex() +
  theme_bw()

### Scatterplots with marginal distributions
require(ggExtra)

# create ggplot object
bike_air_temp = ggplot(bike_share_daily, aes(x=atemp, y=casual)) +
  xlab("Daily Mean Normalized Air Temperature") +
  ylab("Number of Total Casual Bike Uses") +
  geom_point(col="gray50") +
  theme_bw()

# show graph from object
bike_air_temp

# add marginals to the graph
bike_air_temp_mh = ggMarginal(bike_air_temp, type="histogram")
bike_air_temp_mh

bike_air_temp_mb = ggMarginal(bike_air_temp, type="boxplot")
bike_air_temp_mb

### Multiple bivariate comparisons with faceting

ggplot(bike_share_daily, aes(casual, fill=workingday, color=workingday)) +
  geom_density(alpha=0.4) +
  theme_minimal() +
  xlab("Daily Casual Bike Use Count") +
  ylab("") +
  scale_fill_discrete(name="Work Day?") +
  scale_color_discrete(name="Work Day?") +
  facet_wrap(~season, ncol=2) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
        legend.position="top")

# using piping
require(dplyr)

bike_share_daily %>%
  filter(holiday == "No" & workingday == "Yes") %>%
  group_by(mnth) %>%
  summarize(mean_casual = mean(casual, na.rm=T),
            sd_casual = sd(casual, na.rm=T),
            sum_casual = sum(casual)) %>%
  mutate(cv_casual = round(sd_casual / mean_casual, 2)) %>%
  ggplot(aes(mean_casual, sd_casual, color=cv_casual, size=sum_casual)) +
  geom_label(aes(label=mnth)) +
  scale_size(range=c(2,6)) +
  theme_bw()

## Plotting ordinal-scale (e.g., survey) data

require(likert)
data(mass)
str(mass)

# Create a "likert" object
mathiness = likert(mass[2:15])

# Plot likert object
plot(mathiness)

# likert object with 3 questions with gender groupings
gender_math = likert(items=mass[,c(4,6,15), drop=FALSE], grouping=mass$Gender)

# Plot with question/gender histograms
plot(gender_math, include.histogram=TRUE)

## Describing trends in non-temporal data

require(quantreg)

# Should always use loess as first pass
ggplot(bike_share_daily, aes(x=atemp, y=cnt)) +
  xlab("Daily Mean Normalized Air Temperature") +
  ylab("Number of Total Bike Uses") +
  geom_point(col="gray50") +
  geom_smooth(method="loess") +
  theme_bw()

# Plot quantile trends
require(quantreg)

ggplot(bike_share_daily, aes(x=temp, y=casual)) +
  xlab("Daily Mean Normalized Temperature") +
  ylab("Number of Casual Bike Uses") +
  geom_point(col="gray50") +
  stat_quantile(aes(color = ..quantile..), quantiles = c(0.05, 0.1, 0.25,
                                                         0.5, 0.75, 0.9, .95)) +
  scale_color_gradient2(midpoint=0.5, low="steelblue", mid="blue", 
                        high="steelblue ") +
  theme_bw()

# Probably your last choice in EDA is lm (linear model)
ggplot(bike_share_daily, aes(x=atemp, y=cnt)) +
  xlab("Daily Mean Normalized Air Temperature") +
  ylab("Number of Total Bike Uses") +
  geom_point(col="gray50") +
  geom_smooth(method="lm") +
  theme_bw()

### Segmented linear trends

require(segmented)
bike_segment = segmented(lm(cnt~atemp, data=bike_share_daily), ~atemp, psi=0.1)

bike_segment$psi

psi = bike_segment$psi[2]

ggplot(bike_share_daily, aes(x=atemp, y=cnt, group = atemp > psi)) +
  xlab("Daily Mean Normalized Air Temperature") +
  ylab("Number of Total Bike Uses") +
  geom_point(col="gray50") +
  geom_smooth(method="lm") +
  theme_bw()

# Added bivariate density, with breakpoint
ggplot(bike_share_daily, aes(x=atemp, y=cnt, group = atemp > psi)) +
  xlab("Daily Mean Normalized Air Temperature") +
  ylab("Number of Total Bike Uses") +
  geom_point(col="gray50") +
  geom_density2d() +
  geom_smooth(method="lm") +
  theme_bw()

## Evaluating quality with control charts

require(qcc)

# Manual data creation
infections = c(6, 2, 5, 1, 3, 4, 2, 6, 3, 2, 4, 7, 1, 1, 4, 4, 1, 5, 2, 3, 5, 2, 3, 2, 4)
patient_days = c(985, 778, 1010, 834, 750, 729, 1002, 639, 985, 578, 976, 540, 829, 723, 908, 1017, 1097, 1122, 1234, 1022, 1167, 1098, 1201, 1045, 1141)

# Create qcc object
infection_control = qcc(infections, sizes=patient_days/1000, type="u")

# Create a data frame and save to csv
infection_control_qcc = data.frame(Month = month.abb[c(1:12, 1:12, 1:1)], infection_control$limits, Rate = (infections / patient_days)*1000)

write.csv(infection_control_qcc, "infection_control_qcc.csv", row.names=FALSE)

# Check for autocorrelation
acf(infection_control_qcc$Rate)

# For comparison of something w/ clear autocorrelation
# acf(bike_share_daily$casual)

# Time series
# mostly using base plots for quick EDA

require(forecast)

# Monthly page views of the CDC website
# read.csv is a wrapper to read.table
cdc = read.csv("https://data.cdc.gov/api/views/rq85-buyi/rows.csv", header=T)

# convert to time series object
cdc_ts = ts(cdc$Page.Views, start=c(2003,7), freq=12)
cdc_ts

# Base R plot default
plot(cdc_ts)
plot(log(cdc_ts))
plot(diff(cdc_ts, lag=1))

# Month and Season plots
monthplot(cdc_ts)
seasonplot(cdc_ts)

# More EDA
?seasonplot
seasonplot(cdc_ts, year.labels=T, year.labels.left = T, col=rainbow(14))
ggseasonplot(cdc_ts, year.labels=T, year.labels.left = T, col=rainbow(14))

# Autocorrelation
acf(cdc_ts)

# Call a function from a package without loading it
# Periodogram, must de-trend first
# Note: Frequency is the reciprocal of the time period
TSA::periodogram(diff(cdc_ts))

# Time series decomposition
plot(decompose(cdc_ts))
plot(decompose(cdc_ts, type="multiplicative"))

# https://anomaly.io/seasonal-trend-decomposition-in-r/

# Basic forecasting
plot(forecast(cdc_ts))

# Heatmap

bike_share_grp = group_by(bike_share_daily, weekday, mnth)
bike_share_mean = summarise(bike_share_grp, mean=mean(casual))

ggplot(bike_share_mean, aes(weekday, mnth)) +
  geom_tile(aes(fill = mean)) +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  xlab("Day of the Week") +
  ylab("Month") +
  ggtitle("Mean Daily Casual-Use Bike Sharing") +
  scale_y_discrete(limits = rev(levels(bike_share_mean$mnth))) +
  theme_bw() 

### Creating calendar heatmaps

# Source the function from a website
source("http://blog.revolutionanalytics.com/downloads/calendarHeat.R")

# Custom color palettes
require(RColorBrewer)
new_color_ramp = brewer.pal(9, "Greens")

calendarHeat(bike_share_daily$dteday, bike_share_daily$casual, varname="Casual Daily Bike Use", color="new_color_ramp")

# interactive table
require(DT)
datatable(bike_share_daily)

datatable(bike_share_daily, filter = 'top')

# Interactive plots example

# https://rstudio.github.io/dygraphs/
library(dygraphs)

infection_control_qcc_ts = ts(infection_control_qcc[2:4], start=c(2017,1), freq=12)

dygraph(infection_control_qcc_ts)	

# install taucharts from GitHub
# https://rpubs.com/hrbrmstr/taucharts
devtools::install_github("hrbrmstr/taucharts")
require(taucharts)

# Anscombe's Quartet: visualization story and interactive graph example

# load data from github
aq = read.csv("https://gist.githubusercontent.com/alansmithy/68be725ac0c00c790c6f/raw/6dc54a4af348c4bd11a1f87b0b841c53d9f5c4f0/anscombe.csv")

# create taucharts 
tauchart(aq, width=700, height = 500) %>%
  tau_point("x", "y", color="seriesname") %>%
  tau_guide_x(min=0, max=22) %>%
  tau_guide_y(min=0, max=14) %>%
  tau_legend() %>%
  tau_tooltip() %>%
  tau_trendline(models = 'linear', showTrend = F) 





##### IF TIME #########

## Obtaining summary and conditional statistics

require(psych)

describe(bike_share_daily[10:16])

describeBy(bike_share_daily[10:16], bike_share_daily$holiday)

table(bike_share_daily$holiday)

describeBy(bike_share_daily[14:16], bike_share_daily$season == "Winter")

describeBy(bike_share_daily[10:13], bike_share_daily$casual <= 1000)

describeBy(bike_share_daily$casual, bike_share_daily$windspeed >  mean(bike_share_daily$windspeed))

sum(bike_share_daily$cnt > 50, na.rm=T)
sum(bike_share_daily$workingday == 'Yes')

table(bike_share_daily[c(3,6:7)]) 

addmargins(table(bike_share_daily[c(3,6:7)]))

prop.table(table(bike_share_daily[c(3,9)]))

require(dplyr)
summarise(group_by(bike_share_daily, season, holiday, weekday), count=n())

#### Finding local maxima/minima
bike_share_daily[which.min(bike_share_daily[,14]),]

## Inference on summary statistics

### Confidence intervals

#| Statistic | Data type | Distribution | Function |
#| --------- | --------- | ------------ | -------- |
#| Median | Any | None | `asbio::ci.median(x)`
#| Mean| Continuous | Normal, t, "normal enough" | `t.test(x)$conf.int` |
#| Proportion | Percentage | Binomial | `binom.test(x, n)$conf.int` |
#| Count | Count | Poisson | `poisson.test(x)$conf.int` |
#| Rate | Count/*n* | Poisson | `poisson.test(x, n)$conf.int` |

require(boot)
sd_boot_function = function(x,i){sd(x[i])}
sd_boot = boot(PlantGrowth$weight, sd_boot_function, R=10000)
sd(PlantGrowth$weight)
boot.ci(sd_boot, type="bca")$bca[4:5]

q75_function = function(x,i){quantile(x[i], probs=0.75)}
q75_boot = boot(PlantGrowth$weight, q75_function, R=10000)
quantile(PlantGrowth$weight,0.75)
boot.ci(q75_boot, type="bca")$bca[4:5]

#| Statistic | Interval Type | Function |
#| --------- | ------------- | -------- |
#| Mean | Bootstrapped CI | `mean_cl_boot(x, conf.int=0.95, ...)` |
#| Mean | Normal CI | `mean_cl_mean_sdl(x, conf.int=0.95, ...)` |
#| Mean | Standard deviation | `mean_sdl(x, mult=2, ...)` |
#| Median | Quantile | `median_hilow(x, conf.int=0.95, ...)`

require(gridExtra)

p1 = ggplot(PlantGrowth, aes(group, weight)) +
  ggtitle("Bootstrapped") +
  stat_summary(fun.data = mean_cl_boot, fun.args=list(conf.int=0.95))


p2 = ggplot(PlantGrowth, aes(group, weight)) +
  ggtitle("Normal") +
  stat_summary(fun.data = mean_cl_normal, fun.args=list(conf.int=0.95))

p3 = ggplot(PlantGrowth, aes(group, weight)) +
  ggtitle("2 SDs") +
  stat_summary(fun.data = mean_sdl, fun.args=list(mult = 2)) 

p4 = ggplot(PlantGrowth, aes(group, weight)) +
  ggtitle("Median+IQR") +
  stat_summary(fun.data = median_hilow, fun.args=list(conf.int=0.5))

grid.arrange(p1, p2, p3, p4, nrow=2)


### Tolerance intervals

require(tolerance)
commute_time = c(68, 42, 40, 69, 46, 37, 68, 68, 69, 38, 51, 36, 50, 37, 41, 68, 59, 65, 67, 42, 67, 62, 48, 52, 52, 44, 65, 65, 46, 67, 62, 66, 43, 58, 45, 65, 60, 55, 48, 46)
commute_time_npti = nptol.int(commute_time, alpha=0.05, P=0.75, side=2)
commute_time_npti

plottol(commute_time_npti, commute_time, side="two", plot.type="both")

par(mfrow=c(1,1))

#| Data type | Distribution | Function |
#| --------- | ------------ | -------- |
#| Percent | Binomial | `bintol.int(x, n, m, ...)` |
#| Count or Rate | Poisson | `poistol.int(x, n, m, side, ...)` |
#| Nonparametric | None | `nptol.int(x, ...)` |
#| Continuous | Normal, t, "normal enough" | `normtol.int(x, side, ...)`|
#| Continuous | Uniform | `uniftol.int(x, ...)` |
#| Lifetime/survival | Exponential | `exptol.int(x, type.2, ...)` |
#| Score | Laplace | `laptol.int(x, ...)` |
#| Indicies | Gamma | `gamtol.int(x, ...)` |
#| Reliability, extreme values | Weibull, Gumbel | `extol.int(x, dist, ...)` |

## Identifying possible breakpoints in a time series

require(strucchange)
US_co2 = read.table("http://www.eia.gov/totalenergy/data/browser/csv.cfm?tbl=T12.01", sep=",", header=T)
US_co2 = filter(US_co2, Column_Order == 14 & YYYYMM >= 200102 & substr(YYYYMM, 5, 7) != 13)
US_co2_ts = ts(US_co2[,3], freq=12, start=c(2001,2))

# Plot the time series
plot(US_co2_ts, main=expression(paste("Breakpoint in Monthly US"~CO[2]~Emissions)), ylab="Million Metric Tons", col="darkblue", lwd=1.5)
# Plot the line at the optimal breakpoint
lines(breakpoints(US_co2_ts ~ 1), col="darkgreen")
# Plot a 90% confidence interval
lines(confint(breakpoints(US_co2_ts ~ 1), level=0.90), col="darkgreen")
# Add breakpoint location text
text(2008.8, 555, "Jan 2009", cex=0.75, col="darkgreen", pos=4, font=3)

# Get CDC flu data

# Need to use inside SCH firewall to allow API download for CDC flu view
require(httr)
set_config(use_proxy(url="http://wwwproxy.childrens.sea.kids", port=8080, username = "dbarr1", password=.rs.askForPassword("Enter yer Password, ya scurvy dog! Rrrrrrrrrrr")))

# Get flu data for PNW (HHS Region 10)
require(cdcfluview)
flu = get_flu_data("hhs", 10, years=2004:2014)

# Covert weekly data to monthly data
flu$yweek = paste0(flu$YEAR, " ", flu$WEEK-1, " 6")
flu$yweek = as.Date(flu$yweek, format = "%Y %U %u")
library(zoo)
flu$monthYear = as.Date(as.yearmon(flu$yweek))

# Summarize
require(dplyr)
flu_subset = flu %>%
  group_by(monthYear) %>%
  summarize(cases = sum(ILITOTAL))

# Get rid of NA row
# NEED TO QA WHY THIS HAPPENS
flu_subset = na.omit(flu_subset)

# Create a time series
flu_ts = ts(flu_subset$cases, start=c(2004,10), freq=12)
plot(flu_ts)

# Evaluate cycle
flu_pg = TSA::periodogram(diff(flu_ts))
flu_pg_df = data.frame(flu_pg$freq, flu_pg$spec)
flu_pg_df[which.max(flu_pg_df[,2]),]


## Dealing with missing data

### Visualizing missing data

require(VIM)
data(tao)

# Rename the Sea.Surface.Temp column to make label fit on plot
colnames(tao)[4] = "Sea.Temp"

# Look at the data, esp. NAs
summary(tao)

matrixplot(tao)

tao_aggr = aggr(tao)
tao_aggr

histMiss(tao[5:6])

histMiss(tao[c(6,5)])

marginplot(tao[5:6])

marginmatrix(tao[4:6])

### Imputation for missing values

# Perform k Nearest Neighbors imputation
# Result is new dataframe with imputed values
tao_knn = kNN(tao)

marginplot(tao_knn[c(5:6, 13:14)], delimiter="_imp")

marginmatrix(tao_knn[c(4:6, 12:14)], delimiter="_imp")

# Perform standard Iterative Robust Model-based Imputation
tao_irmi = irmi(tao)

# Perform robust Iterative Robust Model-based Imputation
tao_irmi_robust = irmi(tao, robust=TRUE)

# Create a mean-imputed air temp variable
tao$tao_airtemp_mean = ifelse(is.na(tao$Air.Temp), mean(tao$Air.Temp, na.rm=TRUE), tao$Air.Temp)

# Make a data frame of each air temp result
tao_compare_airtemp = data.frame(tao=tao[,5], tao_knn=tao_knn[,5], tao_irmi=tao_irmi[,5], tao_irmi_robust=tao_irmi_robust[,5], mean=tao[,9])

# Melt the various air temp results into a long data frame
require(reshape2)
tao_compare_melt = melt(tao_compare_airtemp, value.name="Air.Temp")

# Plot density histograms of each option and 
# add black dotted line to emphasize the original data
ggplot(tao_compare_melt, aes(Air.Temp, color=variable)) +
  geom_density(lwd=1.25) + 
  geom_density(data=subset(tao_compare_melt, variable=="tao"), 
               aes(Air.Temp), lty=3, lwd=1.5, color="black") +
  theme_minimal()


##### A few useful references #####

## Concepts
# https://medium.com/@kennelliott/39-studies-about-human-perception-in-30-minutes-4728f9e31a73
# http://fellinlovewithdata.com/guides/7-classic-foundational-vis-papers 
# http://www.visualcinnamon.com/resources/learning-data-visualization/books

## Graphs in R
# http://www.joyce-robbins.com/wp-content/uploads/2016/04/effectivegraphsmro1.pdf
# http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html 
# http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/
# http://www.cookbook-r.com/Graphs/
# http://www.statmethods.net/advgraphs/ggplot2.html



##### ACCESS EDW #####

# http://sps/Committees/SCHAF/SCHAF%20Wiki/Using%20the%20RODBC%20package%20to%20access%20databases.aspx
