'Create data dictionaries for each sheet Excel data file
'

# Clear Namespace ---------------------------------------
rm(list=ls())

# Load Packages -----------------------------------------
library(readxl)
library(writexl)
library(Hmisc)
library(skimr)

# Load Data ---------------------------------
sheet <- 'Case Info'
setwd("C:\\Users\\chris.cirelli\\Desktop\\repositories\\CoreProfitability_PreferredReports\\data\\past_yr")
data <- read_excel('Completed_Cases_By_Customer_copy.xlsx', sheet=sheet)

# Create Data Dictionary --------------------------------
col.names <- colnames(data)
skm <- skim(data)
hd <- data.frame(t(head(data)))
hd$colnames <- col.names
mrg <- merge(x=skm, y=hd, by.x = 'skim_variable', by.y='colnames')

# Write to File -----------------------------------------
setwd("C:\\Users\\chris.cirelli\\Desktop\\repositories\\CoreProfitability_PreferredReports\\output\\data_dict")
write_xlsx(mrg, paste('data_dict_', sheet, '.xlsx'))



test <- list('Case Info', 'Recommendations', 'Hazards',
  'QA Checklist - CORE',
  'Core - Commercial Property (2', 'Core - Basic Habitational (2.',
  'Commercial Cooking Suppleme1', 'Photo Report 1.0',
  'Core Habitational 1.4', 'Commercial Cooking Suppleme2',
  'QA Checklist 2G- CORE BASIC H', 'Core - Liability 1.0',
  'Rapid Sketch', 'Core - Habitational (12.12.19)',
  'Core Cover', 'Core - Coversheet ',
  'Core - Habitational (07.16.20', 'Core - Commercial Property Re',
  'Core - Basic Habitational (05', 'Core - Basic Habitational 1.4',
  'Core - Habitational (2.4.2020)', 'Core - Basic Habitational (02',
  'Core - Habitational (07.15.20', 'Recommendation Follow-Up Mast',
  'QA Checklist 2G- CORE HAB 8_5', 'Cancellation Report',
  'Core - Habitational (05.16.20', 'Non-Productive Report 1.2',
  'Core - Shopping Center 1.0')




for (i in seq(1, length(test))){
  setwd("C:\\Users\\chris.cirelli\\Desktop\\repositories\\CoreProfitability_PreferredReports\\data\\past_yr")
  data <- read_excel('Completed_Cases_By_Customer_copy.xlsx', sheet= i)
  col.names < colnames(data)
  col.names <- colnames(data)
  skm <- skim(data)
  hd <- data.frame(t(head(data)))
  hd$colnames <- col.names
  mrg <- merge(x=skm, y=hd, by.x = 'skim_variable', by.y='colnames')
  
  # Write to File -----------------------------------------
  setwd("C:\\Users\\chris.cirelli\\Desktop\\repositories\\CoreProfitability_PreferredReports\\output\\data_dict")
  write_xlsx(mrg, paste('data_dict_', i, '_', test[i], '.xlsx'))
  
  }
