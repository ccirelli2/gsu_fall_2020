' Chris Cirelli
  09/27/2020
  Hw3 GSU Data Visualization
  
  Ref: https://cengel.github.io/R-data-viz/data-visualization-with-ggplot2.html
'

# Clear Namespace & Plots
rm(list=ls())
dev.off()

# Import Libraries 
library(ggplot2)
library(dplyr)

# Define Directories
dir.data <- "C:\\Users\\chris.cirelli\\Desktop\\repositories\\gsu_fall_2020\\Visualizations\\hw3\\data"
dir.scripts <- "C:\\Users\\chris.cirelli\\Desktop\\repositories\\gsu_fall_2020\\Visualizations\\hw3"

# Load Data
setwd(dir.data)
ms.data <- read.csv("MS_stops.csv")
lr.data <- read.csv("ar_little_rock_2020_04_01.csv")
setwd(dir.scripts)

# Inspect Data
summary(ms.data)
head(ms.data)
str(ms.data)
summary(lr.data)
head(lr.data)
str(lr.data)

# Remove nna values
ms.omit.na <- na.omit(ms.county.stops)
lr.omit.na <- na.omit(lr.data)

# Add Date Column 
lr.date <- as.Date(lr.omit.na$date, tryFormats = "%Y-%m-%d")
lr.month <- strftime(lr.omit.na$date, format = "%m")
lr.omit.na$date <- lr.date
lr.omit.na$month <- lr.month

head(lr.omit.na)

######################################################################
# Transform Data
######################################################################

lr.groupby.sex <- lr.omit.na %>%
                  group_by(month, subject_sex) %>%
                  summarise(cnt = n())

lr.groupby.veh.sex <- lr.omit.na %>%
                      group_by(vehicle_type, subject_sex) %>%
                      summarise(cnt = n())


######################################################################
# Plot 1 - 3
######################################################################

# Scatter Plot
p1 <- ggplot(data=lr.groupby.sex, aes(x=month, y=cnt, color=subject_sex)) +
      geom_point() + ggtitle('Stop Frequency By Date By Gender')
p1


# Build Plot iteratively (add features to plot object after declaration)
p1 + geom_point(alpha=0.3, color='blue')
p1 + geom_abline(intercept = 5)


# Bar Plot
p2 <- ggplot(data=lr.groupby.veh.sex, aes(x=vehicle_type, y=cnt, color=subject_sex)) +
      geom_col()

# Flip Coordinates
p2 + coord_flip()

# Reorder
p3 <- ggplot(data = lr.groupby.veh.sex, aes(x = reorder(vehicle_type, cnt), y = cnt, color=subject_sex)) +
               geom_col() + coord_flip()
p3




######################################################################
# Plot 4 - 6
######################################################################

# Bar Plot
p4 <- ggplot(ms.omit.na, aes(violation)) + geom_bar()
p4

# Color Fill
p4 + geom_bar(fill='green')

# Color By Gender

p5 <- ggplot(ms.omit.na, aes(violation)) + geom_bar(aes(fill = driver_gender))
p5

# Fill Bars To Represent Pct of 100
p6 <- ggplot(ms.omit.na, aes(violation)) + geom_bar(aes(fill = driver_gender), position='fill')
p6


######################################################################
# Plot 7 - 9
######################################################################

# Obtian Data For A Single County
head(ms.omit.na)
jones.county <- filter(ms.omit.na, county_name == 'Jones County')

# Box Plot
p7 <- ggplot(jones.county, aes(x=violation, y=driver_age)) + geom_boxplot()
p7

# Add Points To Understand Count of Observations Per Box
p7 + geom_jitter()

# Make Clearer with a change to the transparency of each point & color
p7 + geom_jitter(alpha = 0.5, color="tomato") 

# Change Position So that Box is in front
p8 <- ggplot(jones.county, aes(x=violation, y=driver_age)) +
      geom_jitter(alpha=0.2, color="tomato") + geom_boxplot()
p8

# Change Transparency of Box
p9 <- ggplot(jones.county, aes(x=violation, y=driver_age)) +
  geom_jitter(alpha=0.2, color="tomato") + geom_boxplot(alpha=0.3)
p9


######################################################################
# Violen Plot - Alternative to BoxPlot That Shows Disribution
######################################################################

'Note: The shape of the violen is the distribution about the mean'
p10 <- ggplot(jones.county, aes(x=violation, y=driver_age)) +
  geom_jitter(alpha=0.2, color="tomato") + geom_violin(alpha=0.3)
p10

# Add Mean & Median
p10 + stat_summary(fun=mean, geom="point", shape=23, size=2, color='blue')

# Use Function to Produce Summary Statistics
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

p10 + stat_summary(fun.data = data_summary)




######################################################################
# Headmap 
######################################################################
head(jones.county)

jones.cnt.by.wkday <- jones.county %>% 
                      group_by(wk_day,violation) %>%
                      summarise(n = n())

p11 <- ggplot(jones.cnt.by.wkday, aes(x=wk_day, y=violation)) +
      geom_tile(aes(fill=n)) + 
      scale_fill_gradient(low="grey95", high='tomato') +
      theme_dark()
p11



######################################################################
# Faceting 
######################################################################










