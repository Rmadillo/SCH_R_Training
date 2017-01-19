##################################################
# SPC with R workshop
# Seattle Children's Hospital
# Dwight Barry, Enterprise Analytics
# January 19, 2017
##################################################



##################################################
##### Set up ####

# 1
##### install packages first #####
# install.packages(c("tidyverse", "qicharts", "gridExtra", "ggExtra", "forecast", "trend", "readxl", "devtools", "RODBC"), dependencies=TRUE)

# 2
#### load R packages into working environment ####
library(tidyverse)
library(qicharts)

# 3
#### Load data ####
# Download data from sharepoint first
# http://sps/Committees/SCHAF/SCHAF%20Wiki/SPC%20Training.aspx
# Then you can load into R from your own C drive Downloads folder
# IMPORTANT! REPLACE USERID WITH your USERID
pu = read_csv("C:\\Users\\USERID\\Downloads\\pressure_ulcers_fake.csv")

# Note use of extra \ to "escape" the \ in the path 

## If you downloaded to your O drive, you can use this instead
## pu = read_csv("\\\\childrens\\files\\Users01\\USERID\\Downloads\\pressure_ulcers_fake.csv")



##################################################
# Exercise 1: SPC chart in a line of code


#### Create a u-chart in 1 line of code ####
qic(pressure_ulcers, patient_days, week, multiply = 1000, data = pu, chart = "u")


####################
#### Practice 1 ####
# 1. Using the following code, create a nicer u-chart by specifying titles and labels.

qic(y = pressure_ulcers, 
    n = patient_days, 
    x = week,
    multiply = 1000,
    data = pu, 
    chart = "u",
    main = "Hospital acquired pressure ulcers ('U' chart)",
    ylab = "Count per 1000 patient days",
    xlab = "Week")



##################################################

# Exercise 2: Run charts and qic features

#### Run charts with qic ####

# Run chart of count of pressure ulcers
# Using the qic function of the qicharts package
qic(pressure_ulcers, data = pu)

# Run chart with non-random signal "tests" on chart
qic(pressure_ulcers, data = pu, runvals = TRUE)

## Shift signal manual calculation
# Obtain non-median n
n = sum(ifelse(pu$pressure_ulcers != median(pu$pressure_ulcers), 1, 0))
# Maximum points/side for non-randomness
round(log2(n) + 3)

## Crossings signal manual calculation
# Minimum number of median line crossings
qbinom(0.05, n - 1, 0.5)

# Run chart where first 12 weeks are baseline
# Median line is "frozen" on the first 12 weeks
qic(pressure_ulcers, data = pu, runvals = TRUE, freeze = 12)

# Calculate different centerline for before/after
qic(pressure_ulcers, data = pu, runvals = TRUE, breaks = 12)

#### Labels and annotations for qic charts ####

# Labels
qic(pressure_ulcers, 
    data = pu, 
    runvals = TRUE, 
    breaks = 12,
    x = week,
    main = 'Weekly Pressure Ulcers', 
    sub = '"Show No Mercy" Hospital, Aromatherapy Unit',
    ylab = 'Count',
    xlab = 'Week of')

# Annotations

# Create an empty R object
notes = NA

# Create an annotation at point #12
notes[12] = 'This is an annotation'          

qic(pressure_ulcers, 
    data = pu, 
    runvals = TRUE, 
    breaks = 12,
    x = week,
    main = 'Weekly Pressure Ulcers', 
    ylab = 'Count',
    xlab = 'Week of',
    notes = notes)

# Add another note (etc) to object at points #1 and #24
notes[1] = 'Hi, how are ya?'
notes[24] = 'Are we done yet?'

# Run the same qic command
qic(pressure_ulcers, 
    data = pu, 
    runvals = TRUE, 
    breaks = 12,
    x = week,
    main = 'Weekly Pressure Ulcers', 
    ylab = 'Count',
    xlab = 'Week of',
    notes = notes)


####################	
#### Practice 2 #### 
# Open a new R script in the Source window (top left)
# 1. Load the CLABSI data from the workshop data directory
# 2. Create a run chart of the Rate variable (CLABSIs per 1000 Central Line Days)
# 3. Put a break and annotation at October 2010 (annotation wording is your choice)



##################################################	

# Exercise 3: EDA and control chart assumptions


#### Load CLABSI data ####
# IMPORTANT! REPLACE USERID WITH your USERID
CLABSI = read_csv("C:\\Users\\USERID\\Downloads\\CLABSI.csv")

# Convert Month to a Date scale
CLABSI$Month = as.Date(CLABSI$Month)


#### Time series EDA ####

# Development version of forecast *may* be needed
# devtools::install_github("robjhyndman/forecast")

# Load other libraries
library(gridExtra)
library(ggExtra)
library(forecast)

## Basic EDA

# Line plot with loess smoother for assessing trend
p1 = ggplot(CLABSI, aes(x = Month, y = Rate)) + 
    geom_smooth() + 
    geom_line() 

# Histogram with density overlay
p2 = ggplot(CLABSI, aes(Rate)) + 
    geom_histogram(aes(y = ..density..), binwidth = 0.5, color="gray70") +
    geom_density(color="blue")

# Put both into the same plot
grid.arrange(p1, p2, widths = c(0.65, 0.35))

# Another way to view series, trend, and ditribution together
ggMarginal(p1, margins="y", type = "histogram", binwidth=0.5)

## Time series EDA

# Create a time series object of the Rate variable
CLABSI_ts = ts(CLABSI$Rate, start = c(2006, 10), frequency = 12)

# Seasonplot (actually a year plot in this case)
ggseasonplot(CLABSI_ts)

# Monthplot
ggmonthplot(CLABSI_ts)

## Accumulation plot

# Create a cumulative sums by year data frame
CLABSI_accum = 
    CLABSI %>% # Take the CLABSI data
    group_by(FY) %>% # Group by fiscal year
    arrange(Month) %>% # Sort by month
    mutate(cuml_linedays = cumsum(CL_Days), 
           cuml_infections = cumsum(CLABSI))

# Plot accumultation curves of CLABSIs by CL days by FY
ggplot(CLABSI_accum, aes(x = cuml_linedays, y = cuml_infections, group = as.factor(FY))) +
    geom_path(aes(color = as.factor(FY)), size = 1) +
    geom_point(aes(color = as.factor(FY)))+
    scale_y_continuous(name = "Cummulative CLABSIs", breaks = seq(0,120,10)) +
    scale_x_continuous(name = "Cumulative Central Line Days", breaks = seq(0,40000,5000)) +
    scale_colour_brewer(type = "div", palette = "Spectral") +
    guides(color = guide_legend(title = "Fiscal\nYear")) +
    ggtitle("CLABSIs vesus Central Line Days by Year")

#### Test control chart assumptions ####

# Use the trend package's Mann-Kendall trend test
# Note the ::, another way to call functions from unloaded packages
trend::mk.test(CLABSI_ts)

# Check for autocorrelation
acf(CLABSI_ts)

# Better ACF plot
autoplot(acf(CLABSI_ts))

# lag plot to detail what acf plot is showing
lag.plot(CLABSI_ts, lags = 12, do.lines = FALSE)


####################
#### Practice 3 #### 
# 1. Use this command to subset the CLABSI data from October 2010 on into a new dataframe

CLABSI_2010 = filter(CLABSI, Month >= "2010-10-01")

# 2. Create a time series object of this new data frame (remember $)
# 3. Run the control chart assumption tests on the new ts object
# 4. Are the assumptions satisfied now or not?



##################################################

# Exercise 4: Control charts


#### Control Charts ####

# We already saw a u-chart in the intro,
# Pressure ulcers per 1000 patient days
qic(y = pressure_ulcers, 
    n = patient_days, 
    x = week,
    multiply = 1000,
    data = pu, 
    chart = "u",
    main = "Hospital acquired pressure ulcers ('U' chart)",
    ylab = "Count per 1000 patient days",
    xlab = "Week")

# p chart on same fake pressure ulcer data, 
# Proportion of patients with a pressure ulcer
qic(y = patients_with_pu,
    n = discharges,
    x = week,
    data = pu,
    chart = 'p',
    multiply = 100,
    main = 'Hospital acquired pressure ulcers (P chart)',
    ylab = 'Percent patients',
    xlab = 'Week')

# Just change the chart option to make different types of control charts
# use ?qic to see the help file

#### Rare event data control charts ####

# Read in fake CAUTI data from a tab-delimited file
CAUTI = read_tsv("C:\\Users\\USERID\\Downloads\\CAUTI_fake.txt")

# File saved from Excel w/ Excel's stupid date format, so convert to ISO date
CAUTI$Event_Date = as.Date(CAUTI$Event_Date, format="%m/%d/%Y")

# Long cut to view data in source window
View(CAUTI)

# g chart for number of catheter days between CAUTIs (discrete units)
qic(y = Caths_since_last_event, 
    data = CAUTI,
    chart = 'g',
    main  = 'Catheter days between CAUTIs, FY16 (g chart)',
    ylab  = 'Catheter days',
    xlab  = 'CAUTI Patient Number')

# t chart for days between CAUTIs (continuous units)
qic(y = Days_since_last_event,
    data = CAUTI,
    chart = 't',
    main  = 'Days between CAUTIs, FY16 (t chart)',
    ylab  = 'Days',
    xlab  = 'CAUTI number')


####################
#### Practice 4 #### 
# 1. Create a u-chart from the CLABSI_2010 data.
# 2. Create a u-chart from all the CLABSI data with a break at October 1, 2010. 




####################
# If there's time 


#### Loading data from an Excel file ####
# IMPORTANT! REPLACE USERID WITH your USERID
CAUTI_from_xlsx = readxl::read_excel("C:\\Users\\USERID\\Downloads\\fake_data.xlsx", sheet = "cauti", col_types = c("date", "numeric", "numeric"), na = "NA")
                                     
                                     
#### Loading data from the EDW ####

# Load ODBC package
library(RODBC)
                                     
# Set credentials to access EDW
# IMPORTANT! REPLACE (Dwight's) USERID WITH (your) USERID
EDW = odbcConnect(dsn="EDWNetezza", 
    uid="dbarr1",   
    pwd=.rs.askForPassword("Enter password"))
                                     
# Standard SQL query for acquiring hours worked by clinical staff
# (Data provenance is unkown, this could be a test dataset for EDW)
ED_hours_query = "
    SELECT BEGIN_DATE, 
    sum(TOTAL_HOURS)
                                     
    FROM PRD_DISTRIBUTION..ED_UC_CONSOLIDATED_SCHEDULE
                                     
    where SHIFT_CATEGORY like 'Clinical%'
                                     
    group by BEGIN_DATE
    order by BEGIN_DATE"

# Pull data from EDW using the query
ED_hours = sqlQuery(EDW, ED_hours_query, stringsAsFactors=F, believeNRows=FALSE)
                                     
# Close ODBC connection
close(EDW)
                                     
# Make an inital run chart of hours by day for clinical ED/UC staff
qic(y = SUM, 
    x = BEGIN_DATE,
    data = ED_hours, 
    runvals = TRUE)

#### End of File ####
