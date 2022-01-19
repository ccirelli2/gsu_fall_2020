library(arrow)
library(tidyverse)
library(purrr)
library(dplyr)
library(ggplot2)
library(Hmisc)
library(xlsx)
library(ggplot2)
library(data.table)
library(base)

data <- read_parquet("C:\\Users\\chris.cirelli\\Desktop\\repositories\\Core_profitability\\data\\combined_core_20200630.parquet", as_tibble = TRUE)

colnames(claims_wide)

#Get the Claims Count data 
claims_wide <- data %>% select(State, contains("CalendarYear")) %>% select(State, contains("Claims"))
claims_long <- gather(claims_wide, ClaimType, ClaimCount,  ClaimsCalendarYear2020:ClaimsPropertySpecialWaterNonWeatherCalendarYear2016)
claims_long <- claims_long %>% group_by(ClaimType, State) %>%
               summarise(ClaimCount = sum(ClaimCount)) %>%
               mutate(CY = str_sub(ClaimType,-4,-1))
claims_long$ClaimType <- str_sub(claims_long$ClaimType, 7, str_length(claims_long$ClaimType)-16)

test <- claims_long %>% group_by(ClaimType, State) %>%
              summarise(Counts = sum(ClaimCount))

#Get the Exposure data 
exposure_wide <- data %>% select(State, starts_with("ExposureCalendarYear"))
exposure_long <- gather(exposure_wide, ExposureType, Exposure,  ExposureCalendarYear2020:ExposureCalendarYear2016)
exposure_long <- exposure_long %>% group_by(ExposureType, State) %>%   
  summarise(Exposure = sum(Exposure)) %>%
  mutate(CY = str_sub(ExposureType,-4,-1))
#delete column that we no longer need
exposure_long$ExposureType <- NULL

#Get the Loss data 
incurred_wide <- data %>% select(State, contains("Incurred")) %>% select(State, contains("CalendarYear"))
incurred_long <- gather(incurred_wide, ClaimType, Incurred,  IncurredCalendarYear2020:IncurredPropertySpecialWaterNonWeatherCalendarYear2016)
#create various capping at 50k, 100k and 250k
incurred_long <- incurred_long %>% mutate(Inc50kCap = pmin(Incurred, 50000))%>% 
                                   mutate(Inc100kCap = pmin(Incurred, 100000)) %>%
                                   mutate(Inc250kCap = pmin(Incurred, 250000))
#Summarize by claim tpye and state
incurred_long <- incurred_long %>% group_by(ClaimType, State) %>%
  summarise(Incurred = sum(Incurred), Inc50kCap = sum(Inc50kCap),
            Inc100kCap = sum(Inc100kCap),Inc250kCap = sum(Inc250kCap)) %>%
  mutate(CY = str_sub(ClaimType,-4,-1))
incurred_long$ClaimType <- str_sub(incurred_long$ClaimType, 9, str_length(incurred_long$ClaimType)-16)


#merge the pure premium data
incurred_claims_merge <- merge(incurred_long,claims_long, all=TRUE)
PPdata <- merge(incurred_claims_merge, exposure_long, all=TRUE)

#create metrics
PPdata <- PPdata %>% mutate(Freq = ClaimCount/Exposure) %>%
                     mutate(Sev50Cap = ifelse(Incurred == 0, 0, Inc50kCap/ClaimCount))%>% 
                     mutate(Sev100Cap = ifelse(Incurred == 0, 0,Inc100kCap/ClaimCount))%>% 
                     mutate(Sev250Cap = ifelse(Incurred == 0, 0,Inc250kCap/ClaimCount))%>%
                     mutate(Sev = ifelse(Incurred == 0, 0,Incurred/ClaimCount))%>%
                     mutate(PurePremium = Incurred/Exposure) %>%
                     mutate(PP50Cap = Inc50kCap/Exposure) %>%
                     mutate(PP100Cap = Inc100kCap/Exposure) %>%
                     mutate(PP250Cap = Inc250kCap/Exposure) %>%
                     mutate(ClaimType = ifelse(ClaimType == "", "All", ClaimType)) #accounting for blanks in totals

#create Property/Liab indicator
PPdata <- PPdata %>% mutate(PropLiabind = ifelse(ClaimType == "Liability" | ClaimType =="Property"| ClaimType == "Crime",1,0))

#create COL indicator
PPdata <- PPdata %>% mutate(COLind = ifelse(ClaimType == "Liability" | ClaimType =="PropertyBGI"| 
                                                ClaimType == "PropertyBGII" | ClaimType == "PropertySpecial" |
                                                ClaimType == "PropertyEarthquake" | ClaimType == "PropertyEquipmentBreakdown" | 
                                                ClaimType ==  "PropertyFlood"  ,1,0))

#create Peril indicator
PPdata <- PPdata %>% mutate(PerilInd = ifelse(ClaimType == "LiabilityBodilyInjury" | ClaimType =="LiabilityEPL"| 
                                              ClaimType == "LiabilityHabitational" | ClaimType == "LiabilityOther" |
                                              ClaimType == "PropertyBGIFire" | ClaimType == "PropertyBGIOtherNonWind" | 
                                              ClaimType == "PropertyBGIVandalism" | ClaimType == "PropertyBGIIOtherNonWind" |
                                              ClaimType == "PropertyBGIFire" | ClaimType == "PropertyBGIIHail" |
                                              ClaimType == "PropertyBGIIHurricane" | ClaimType == "PropertyBGIINonHurricaneWind" |
                                              ClaimType == "PropertyBGIIOtherNonWind" | ClaimType == "PropertySpecialOtherNonWind" |
                                              ClaimType == "PropertySpecialWaterNonWeather" | 
                                              ClaimType == "PropertySpecialWaterWeather", 1, 0))

#write Pure Premium data to csv
write_csv(PPdata, "C:\\Users\\devyn.mcnicoll\\Dropbox (Swyfft)\\Core_profitability\\Analysis\\AYPurePremiumData.csv")


#on-level the premium
onLevelData <- data %>% select(State, Company, EffectiveDate, contains("Earned")) %>% 
  select(State, Company, EffectiveDate, contains("CalendarYear")) %>% 
  select(-contains("i.")) %>% mutate(EffectiveDate= as.Date(EffectiveDate))
onLevelData$LiabCRL <- ifelse(onLevelData$State == "CA" & 
                          onLevelData$Company %in% c("State National Ins. Co - Ascot",
                                                     "State National Insurance Company, Inc. - Endurance",
                                                     "State National Insurance Co., Inc. -Qatar Re") &
                          onLevelData$EffectiveDate < as.Date("2018-10-06"),1.299,
                  ifelse(onLevelData$State == "NY" & 
                          onLevelData$Company %in% c("Clear Blue Specialty Insurance Company",
                                                            "Clear Blue Insurance Co.") &
                          onLevelData$EffectiveDate < as.Date("2019-08-01"),1.2*1.2,
                   ifelse(onLevelData$State == "NY" & 
                            onLevelData$Company %in% c("Clear Blue Specialty Insurance Company",
                                                       "Clear Blue Insurance Co.") &
                            onLevelData$EffectiveDate < as.Date("2020-08-01"),1.2,
                   ifelse(onLevelData$State == "IL" & 
                            onLevelData$Company %in% c("State National Ins. Co - Ascot",
                                                       "State National Insurance Company, Inc. - Endurance",
                                                       "State National Insurance Co., Inc. -Qatar Re") &
                            onLevelData$EffectiveDate < as.Date("2019-07-01"),1.15*1.3,
                   ifelse(onLevelData$State == "IL" & 
                            onLevelData$Company %in% c("State National Ins. Co - Ascot",
                                                       "State National Insurance Company, Inc. - Endurance",
                                                       "State National Insurance Co., Inc. -Qatar Re") &
                            onLevelData$EffectiveDate < as.Date("2020-03-01"),1.3, 1)))))

onLevelData$PropCRL <-  ifelse(onLevelData$State == "IL" & 
                                 onLevelData$Company %in% c("State National Ins. Co - Ascot",
                                                            "State National Insurance Company, Inc. - Endurance",
                                                            "State National Insurance Co., Inc. -Qatar Re") &
                                 onLevelData$EffectiveDate < as.Date("2020-03-01"),1.1,1)

#create new columns for on-leveled Liability Earned 
onLevelData <- onLevelData %>% 
  mutate(EarnedLiabilityOnLevelCalendarYear2016 = EarnedLiabilityCalendarYear2016 *LiabCRL,
         EarnedLiabilityOnLevelCalendarYear2017 = EarnedLiabilityCalendarYear2017 *LiabCRL,
         EarnedLiabilityOnLevelCalendarYear2018 = EarnedLiabilityCalendarYear2018 *LiabCRL,
         EarnedLiabilityOnLevelCalendarYear2019 = EarnedLiabilityCalendarYear2019 *LiabCRL,
         EarnedLiabilityOnLevelCalendarYear2020 = EarnedLiabilityCalendarYear2020 *LiabCRL)


#Get the Premium data 
earned_wide <- onLevelData %>% select(State, contains("Earned")) %>% select(State, contains("CalendarYear"))
earned_long <- gather(earned_wide, PremType, Earned,  EarnedCalendarYear2020:EarnedLiabilityOnLevelCalendarYear2020)
earned_long <- earned_long %>% group_by(PremType, State) %>%
  summarise(Earned = sum(Earned)) %>%
  mutate(CY = str_sub(PremType,-4,-1))
earned_long$PremType <- str_sub(earned_long$PremType, 7, str_length(earned_long$PremType)-16)

#This is a short term premium merge that only penetrates the Cause of Loss level
prem4merge <- earned_long %>% mutate(ClaimType = ifelse(PremType == "BGI", "PropertyBGI",
                                                          ifelse(PremType == "BGII", "PropertyBGII",
                                                          ifelse(PremType == "Special", "PropertySpecial",
                                                          ifelse(PremType == "LiabilityOnLevel", "Liability",
                                                          ifelse(PremType == "Property", "Property",
                                                          ifelse(PremType == "Property", "Property",
                                                          ifelse(PremType == "Crime", "Crime",
                                                          ifelse(PremType == "Flood", "PropertyFlood",
                                                          ifelse(PremType == "Earthquake", "PropertyEarthquake",
                                                                 "NA"))))))))))  %>%
                              filter(ClaimType != "NA") 

#get rid of column we don't want in merge
prem4merge$PremType <- NULL

AggData <- merge(PPdata, prem4merge, all.x=TRUE)
AggData <- AggData %>%mutate(LR =  ifelse(Earned == 0 , 0 , Incurred/Earned),
                             LR50kCap = ifelse(Earned == 0 , 0 , Inc50kCap/Earned),
                             LR100kCap = ifelse(Earned == 0 , 0 , Inc100kCap/Earned),
                             LR250kCap = ifelse(Earned == 0 , 0 , Inc250kCap/Earned))

#write aggregate data to CSV
write_csv(AggData, "C:\\Users\\devyn.mcnicoll\\Dropbox (Swyfft)\\Core_profitability\\Analysis\\AYAggregateDataOnLevelLiab.csv")


