
##### Set up ############################################

#install.packages(c("RGtk2", "magrittr", "stringi", "pmml", "bitops", "colorspace", "ada", "amap", "arules", "arulesViz", "biclust", "cairoDevice", "cba", "cluster", "corrplot", "descr", "doBy", "dplyr", "e1071", "ellipse", "fBasics", "foreign", "fpc", "gdata", "ggdendro", "ggplot2", "gplots", "gridExtra", "gtools", "gWidgetsRGtk2", "hmeasure", "Hmisc", "kernlab", "Matrix", "mice", "nnet", "odfWeave", "party", "playwith", "plyr", "psych", "randomForest", "RColorBrewer", "readxl", "reshape", "rggobi", "RGtk2Extras", "ROCR", "RODBC", "rpart", "rpart.plot", "SnowballC", "stringr", "survival", "timeDate", "tm", "verification", "wskm", "XML"))

# install.packages("rattle")
# library(rattle)

# If the RGtk2 package is not installed, there will be an error popup indicating that 
# libatk-1.0-0.dll is missing from your computer. Click OK and then you will be asked 
# if you would like to install GTK+. Click OK. This then downloads and installs the 
# appropriate GTK+ libraries for your computer. After this has finished, exit from R and 
# restart it so that it can find the newly installed libraries.

##### Analytics Framework ###############################

# Analytic Framework Overview / What and Why for EDA - Bryan 

##### EDA ###############################################

# Look at data
weather = read.csv("file:///C:/Users/USERID/Documents/R/win-library/3.3/rattle/csv/weather.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

# Some quick EDA?
require(ggplot2)

str(weather)
summary(weather)

# Histogram, default
ggplot(weather, aes(x=Humidity3pm)) +
  geom_histogram()

# Histogram, custom binwidth 
ggplot(weather, aes(x=Humidity3pm)) +
  geom_histogram(binwidth=10)

# Histogram / density plot
ggplot(weather, aes(x=Humidity3pm)) + 
  geom_histogram(aes(y =..density..), binwidth = 10) + 
  geom_density()

# Bar plot
ggplot(weather, aes(x=WindDir3pm)) +
  geom_bar()

# Scatter plot
ggplot(weather, aes(x=Pressure3pm, y=Humidity3pm)) +
  geom_point()

# Boxplot
ggplot(weather, aes(x=WindDir3pm, y=Humidity3pm)) +
  geom_boxplot()

# Mosaic plot
mosaicplot(table(weather$WindDir9am, weather$WindDir3pm))

# Trends (loess)
ggplot(weather, aes(x=Pressure3pm, y=Humidity3pm)) +
  geom_point() +
  geom_smooth()

# Trends (quantile-median)
ggplot(weather, aes(x=Pressure3pm, y=Humidity3pm)) +
  geom_point() +
  geom_quantile(quantiles = 0.5)

# Trends (quantile-custom)
ggplot(weather, aes(x=Pressure3pm, y=Humidity3pm)) +
  geom_point() +
  geom_quantile(quantiles = c(0.05, 0.25, 0.50, 0.75, 0.95))

# Trends (linear)
ggplot(weather, aes(x=Pressure3pm, y=Humidity3pm)) +
  geom_point() +
  geom_smooth(method = 'lm')
  
# Polar coordinates (for circular data)
ggplot(weather, aes(x=WindDir3pm, fill=RainTomorrow)) +
    geom_bar() +
    coord_polar()

# Faceted (with linear trend)
ggplot(weather, aes(x=Pressure3pm, y=Humidity3pm)) +
  geom_point() +
  geom_smooth(method = 'loess', se=FALSE) +
  facet_wrap(~WindDir3pm)

# Visualize missing data  
require(VIM)

matrixplot(weather)

aggr(weather)


####################
# Prizes for the most interesting EDA result! a) best viz "story" & b) best image [volunteers, not required]
# email code snippet to Dwight and we'll talk through it on screen?
####################

# Remove data frame
rm(weather)


##### Predictive Analytics  ###############################################

# Interlude: Why Model? What is a model? - Andy

###### START RATTLE #####

library(rattle)
rattle()

# Troubleshoot if needed

##### Decision Tree #####

# Export rpart model to PMML
# (Export button doesn't work on KMA02)
saveXML(pmml(crs$rpart), "rain_rpart.pmml")

# Convert rpart model to SQL

source("https://raw.githubusercontent.com/jasoncapehart/genSQL/master/genSQL.R")
genSQL(crs$rpart)


