##########################################################################
# Traffic light R code for Power BI
# Code: Gayle Garson, Amber Yun, Dwight Barry
# Version: 0.9
# Date of this version: December 7, 2017
# Change log:
#       0.9   12/7/17    Code created    Dwight Barry
#       
##########################################################################



#### NOTE TO ANALYST ####

# This code is set up such that your labels are in the first column
# and that your color designator is in the second column of the 
# "dataset" dataframe, which can be a hex color or R named color. 

# The order they appear in the dataframe is the order in which you
# click their checkboxes in the main Power BI window. 

# If you use or add dates and/or times, note that Power BI stores 
# them as ISO format, even if in Power BI tables they are displayed
# in other formats formats. This code assumes that if a date/time is
# used, it will appear in the third column. 

# FAKE DATA FOR EXAMPLE; NOT RUN
# dataset = data.frame(mylabels = c("ABCDEF", "AB", "ABC WW"),
#       colors = c("#ff0000", "green", "yellow"),
#       MY_DATE = c("2017-01-01", "2016-05-01", "2016-04-01"))



##############################
####    Load libraries    ####
##############################

library(stringr)
library(ggplot2)
library(dplyr)



#########################
####    Data prep    ####
#########################

# Check for a third column, if so, ensure that the date variable
# is encoded as a Date object; use as.POSIXct if you have a timestamp
if (ncol(dataset) >= 3) {
    dataset[ , 3] = as.Date(dataset[ , 3])
}

# Create a single-value vector that finds the length of the 
# label string (first column)
plot_label_width = max(str_length(dataset[ , 1]))

# Create a placeholder to line up the circles on x axis = 0
dataset$place = 0



#############################
####    Make the plot    #### 
#############################

if (ncol(dataset) == 2) {

    # Call to ggplot and specify axes variables
    ggplot(data = dataset, aes(x = place, y = dataset[ , 1])) +
        # add the traffic lights from colors in column 2
            geom_point(aes(color = dataset[ , 2]), size = 18, pch = 19) +
            scale_color_manual(values = levels(dataset[ , 2])) +
        # add the label text, left justified
            geom_text(aes(x = -plot_label_width, label = dataset[ , 1]), 
                      hjust = 0, color = "black", size = 6) +
        # size x axis based on longest label
            xlim(-plot_label_width, 1) +
        # remove all other plot elements
            theme_void() +
        # remove legend
            theme(legend.position = 'none')
            
} else {

# If there's a third column, use that Date (or Time) variable
# to order the names by date, "descending" based on plot layout
# where you start from the bottom, i.e., y = 0
    
    # Call to ggplot and specify axes variables
    ggplot(data = dataset, aes(x = place, 
                    y = reorder(dataset[ , 1], desc(dataset[ , 3])))) +
        # add the traffic lights from colors in column 2
            geom_point(aes(color = dataset[ , 2]), size = 18, pch = 19) +
            scale_color_manual(values = levels(dataset[ , 2])) +
        # add the label text, left justified
            geom_text(aes(x = -plot_label_width, label = dataset[ , 1]), 
                    hjust = 0, color = "black", size = 6) +
        # add the date text to the right of the traffic lights
        # formatted with stupid Excel date style and centered
            geom_text(aes(x = plot_label_width / 2, 
                    label = format(dataset[ , 3], "%m/%d/%y")), 
                    hjust = 0.5, color = "black", size = 6) +
        # size x axis based on longest label
            xlim(-plot_label_width, plot_label_width) +
        # remove all other plot elements
            theme_void() +
        # remove legend
            theme(legend.position = 'none')
}
    


#### END OF FILE ####
