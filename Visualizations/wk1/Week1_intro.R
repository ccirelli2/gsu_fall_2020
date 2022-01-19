# references: 
# 1. https://rkabacoff.github.io/datavis/ ## lots of the example come from here
# 2. https://r4ds.had.co.nz/transform.html ## for more examples on data transformation
# 3. https://towardsdatascience.com/a-guide-to-data-visualisation-in-r-for-beginners-ef6d41a34174

#################################################
##
##       part 1: install and load packages. 
##
##################################################

rm(list=ls())

install.packages("carData") ## only need to run this once to install this package to your computer.
require("carData") ## need to run this every time you start a new R session, to load it to the current session

install.packages("readr") ## to import data
require("readr")

install.packages("dplyr") ## for data manipulation
## You will be asked: Do you want to install from sources the packages which need compilation? (Yes/no/cancel) 
## type no, then hit enter
require("dplyr")

#################################################
##
##        part 2: Data preparation: importing data and cleaning data
##
##################################################


#################################################
## part 2.1: importing data
##################################################

## there are different ways to import data 

data(Salaries, package="carData") ## if data is contained in a package


## if data is stored on your local computer

## type 1: text file

library(readr)

## data can be otained here: https://rkabacoff.github.io/datavis/Data.html#Salaries
# import data from a comma delimited file
Salaries <- read_csv("salaries.csv")

# import data from a tab delimited file
setwd('C:\\Users\\chris.cirelli\\Desktop\\repositories\\gsu_fall_2020\\Visualizations\\data')
Salaries <- read_tsv("salaries.csv")

## type 2: excel spreadsheets

library(readxl)

# import data from an Excel workbook
Salaries <- read_excel("salaries.xlsx", sheet=1)

## type 3: other statistical packages

library(haven)

# import data from Stata
Salaries <- read_dta("salaries.dta")

# import data from SPSS
Salaries <- read_sav("salaries.sav")

# import data from SAS
Salaries <- read_sas("salaries.sas7bdat")

#################################################
## part 2.2: data exploration
##################################################

## after you import a data into R, the first thing is always to double check to make sure it's correctely imported and understand the data structure
str(Salaries)

colnames(Salaries)

dim(Salaries)

head(Salaries,5)

tail(Salaries,5)

summary(Salaries)

View(Salaries)



#################################################
## part 2.3: data cleaning
##################################################

## selecting variables
library(dplyr)

colnames(starwars)
dim(starwars)

## excercise: use the functions we learned from part 2.2 to look at the starwars data.

################################################################
## select function is used to select specific columns
################################################################

# keep the variables name, height, and gender
newdata <- select(starwars, name, height, gender)
newdata <- select(starwars,c(name,height,gender))


# keep the variables name and all variables 
# between mass and species inclusive
newdata <- select(starwars, name, mass:species)

# keep all variables except birth_year and gender
newdata <- select(starwars, -birth_year, -gender)
newdata <- select(starwars,-c(name))

##################################################
## filter function is used to select rows
##################################################

## selecting observations

# select females
newdata <- filter(starwars, gender == "female")

# select females that are from Alderaan
newdata <- filter(starwars, gender == "female" & homeworld == "Alderaan")


# select individuals that are from 
# Alderaan, Coruscant, or Endor
newdata <- filter(starwars, homeworld == "Alderaan" | homeworld == "Coruscant" | homeworld == "Endor")

# this can be written more succinctly as
newdata <- filter(starwars, homeworld %in% c("Alderaan", "Coruscant", "Endor"))


## Creating/Recoding variables
#######################################################################################
#The mutate function allows you to create new variables or transform existing ones.
#######################################################################################

# convert height in centimeters to inches, 
# and mass in kilograms to pounds
newdata <- mutate(starwars,height = height * 0.394, mass   = mass   * 2.205)

### exercise: Based the Salaries data set, 
### construct a new data set with rank, yrs.service and salary for 
### male with salary between 150k and 200k, yrs of service greater than 30 yrs 
### also, showing the salary in thousands



# The ifelse function (part of base R) can be used for recoding data. The format is ifelse(test, return if TRUE, return if FALSE).

# if height is greater than 180 
# then heightcat = "tall", 
# otherwise heightcat = "short"

newdata <- mutate(starwars, heightcat = ifelse(height > 180, "tall","short"))
                  
# convert any eye color that is not 
# black, blue or brown, to other
newdata <- mutate(starwars,eye_color = ifelse(eye_color %in% c("black", "blue", "brown"),eye_color,"other"))
                                    
# set heights greater than 200 or 
# less than 75 to missing
newdata <- mutate(starwars,height = ifelse(height < 75 | height > 200, NA, height))

## summarizing data

# calculate mean height and mass
newdata <- summarize(starwars, mean_ht = mean(height, na.rm=TRUE),mean_mass = mean(mass, na.rm=TRUE))
newdata

# calculate mean height and weight by gender
newdata <- group_by(starwars, gender)
newdata <- summarize(newdata,mean_ht = mean(height, na.rm=TRUE),mean_wt = mean(mass, na.rm=TRUE))
newdata


### exercise: Based the Salaries data set, 
### create a variable called status junior if yrs.service<10, middle if yrs.service between 10 and 20, senior if yrs.service >=20.  
### calculate the mean salary by status. 


##########################
## using pipes
############################

## Packages like dplyr and tidyr allow you to write your code in a compact format using the pipe %>% operator. Here is an example.

library(dplyr)

# calculate the mean height for women by species
newdata <- filter(starwars,gender == "female")
newdata <- filter(starwars,gender == "feminine")
newdata <- group_by(species)
newdata <- summarize(newdata,mean_ht = mean(height, na.rm = TRUE))
newdata

# this can be written as
newdata <- starwars %>%
  filter(gender == "female") %>%
  group_by(species) %>%
  summarize(mean_ht = mean(height, na.rm = TRUE))

##########################################
## reshaping data
############################################

install.packages("tidyr")
require("tidyr")

wide_data <- iris[c(1, 51, 101), ]
# gather Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
long_data = gather(wide_data, key = "flower_att", value = "measurement",
       Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
wide_data <- spread(long_data, flower_att, measurement)
