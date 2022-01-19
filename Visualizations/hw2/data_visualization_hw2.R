'

author : Chris Cirelli
'


# Clear Namespace & Plots
rm(list=ls())
dev.off()

# Load Libraries
library(ggplot2)
library(dplyr)
library(treemapify)

# Load Data
setwd("C:\\Users\\chris.cirelli\\Desktop\\repositories\\data_misc")
data.main <- read.csv("insurance_data.csv")
data.claims <- read.csv('claims_data.csv')

###################################################################
# Inspect Data
##################################################################

# Dimensions
dim(data.main)

# Date Range
get_eff_date <- as.Date(data.main$EffectiveDate, tryFormats = "%Y-%m-%d")
get_exp_date <- as.Date(data.main$ExpirationDate, tryFormats = "%Y-%m-%d")
get_eff_yr <- format(as.Date(data.main$EffectiveDate, tryFormats = "%Y-%m-%d"), "%Y")
get_exp_yr <- format(as.Date(data.main$ExpirationDate, tryFormats = "%Y-%m-%d"), "%Y")
unique(get_yr)

# Add Date & Year to Dataset
data.main$EffDate <- get_eff_date
data.main$EffYr <- get_eff_yr
data.main$ExpDate <- get_exp_date
data.main$ExpYr <- get_exp_yr


# Group Pertinent Data Fields By Year
pnl.by.yr <- data.main %>% 
                 group_by(EffYr) %>%
                 summarise(n = n(),
                           sum_incurred=sum(IncurredLiability),
                           sum_earned=sum(EarnedLiability))

# Plot Count By Year
ggplot(pnl.by.yr, aes(x = EffYr, y = n)) + 
  geom_bar(stat = "identity") + ggtitle('Location Count By Eff Year') +
  geom_text(aes(label = n), vjust=-0.5) +
  theme_minimal()


# Plot Premium & Loss By Year
ggplot(pnl.by.yr, aes(x = EffYr, y = sum_incurred)) + 
  geom_bar(stat = "identity") + ggtitle('Sum Incurred By Eff Year') +
  geom_text(aes(label = sum_incurred), vjust=-0.5) +
  theme_minimal()

ggplot(pnl.by.yr, aes(x = EffYr, y = sum_earned)) + 
  geom_bar(stat = "identity") + ggtitle('Sum Earned By Eff Year') +
  geom_text(aes(label = sum_earned), vjust=-0.5) +
  theme_minimal()


# Plot Loss Ratio By Year
loss.ratio <- earn.incur.by.yr$sum_incurred / earn.incur.by.yr$sum_earned
pnl.by.yr$LossRatio <- round(loss.ratio,3)

ggplot(pnl.by.yr, aes(x = EffYr, y = LossRatio, color=EffYr)) + 
  geom_bar(stat = "identity") + ggtitle('Loss Ratio By Eff Year') +
  geom_text(aes(label = LossRatio), vjust=-0.5) + 
  theme_minimal()


###################################################################
# Composition of Claims
###################################################################

# Add Date Yr Columns
get_acc_date <- as.Date(data.claims$ACC_DATE, tryFormats = "%Y-%m-%d")
get_acc_yr <- format(as.Date(data.claims$ACC_DATE, tryFormats = "%Y-%m-%d"), "%Y")
data.claims$AccDate <- get_acc_date
data.claims$AccYr <- get_acc_yr

# Subset Data 2016 to 2020
data.claims <- subset(data.claims, AccYr >=2016)

# Plot Claims By Coverage Group
claim.cnt.by.covg.group <- data.claims %>%
                           group_by(AccYr, CLAIM_TYPE_COVERAGE_GROUP) %>%
                           summarise(n = n())


ggplot(claim.cnt.by.covg.group, aes(x = AccYr, y = n, fill=CLAIM_TYPE_COVERAGE_GROUP)) + 
  geom_bar(stat = "identity", position="dodge") + ggtitle('Claim Coverage Type By Year') +
  theme_minimal() + theme(legend.position="bottom")



# Subset Claims By Liability
data.claims.liability <- subset(data.claims, (SUB_PERIL != "") & (SUB_PERIL !="Winter-Storm"))
data.claims.liability <- subset(data.claims.liability, PERIL = "Liability")


claim.cnt.by.yr2 <- data.claims.liability %>%
                    group_by(AccYr, SUB_PERIL) %>%
                    summarise(n = n())


ggplot(claim.cnt.by.yr2, aes(x = AccYr, y = n, fill=SUB_PERIL)) + 
  geom_bar(stat = "identity", position="dodge") + ggtitle('Claim Sub Type By Year') +
  theme_minimal() + theme(legend.position="bottom")




###################################################################
# Habitational Claims Attributes
###################################################################

# Subset Data to Habitational Claims Only
data.claims.hab <- subset(data.claims, SUB_PERIL = 'Habitational')

# Distributon By State

# Group Data Main By State
data.main.bystate <- data.main %>%
                     group_by(State) %>%
                     summarise(n=n())

ggplot(data.main.bystate, 
       aes(fill = State, area = n, label = State)) +
  geom_treemap() + 
  geom_treemap_text(colour = "white", place = "centre") +
  geom_treemap_text(aes(label= n)) + 
  labs(title = "Location Count By State") +
  theme(legend.position = "right")+ scale_y_continuous(labels = scales::percent)


data.claims.hab.state <- data.claims.hab %>%
                         group_by(RISK_ST) %>%
                         summarise(n = n(), sum_incurred = sum(TOTAL_INCURRED))

ggplot(data.claims.hab.state, 
       aes(fill = RISK_ST, area = n, label = RISK_ST)) +
  geom_treemap() + 
  geom_treemap_text(colour = "white", place = "centre") +
  geom_treemap_text(aes(label= n)) + 
    labs(title = "Habitational Claim Count By State") +
  theme(legend.position = "right")+ scale_y_continuous(labels = scales::percent)



ggplot(data.claims.hab.state, 
       aes(fill = RISK_ST, area = sum_incurred, label = RISK_ST)) +
  geom_treemap() + 
  geom_treemap_text(aes(label=sum_incurred)) + 
  geom_treemap_text(colour = "white", place = "centre") +
  labs(title = "Habitational Claim Incurred Amount By State") +
  theme(legend.position = "right")



# Distributoin Habitational Claims By Amount
summary(data.claims.hab$TOTAL_INCURRED)
ggplot(data=data.claims.hab, aes(TOTAL_INCURRED)) +
  geom_histogram() +  ggtitle('Habitation Claims - Incurred Distribution') + theme_minimal()



# Subset By Incurred Amounts > 250k
data.claims.hab.max <- subset(data.claims.hab, TOTAL_INCURRED > 250000)
hist(data.claims.hab.max$TOTAL_INCURRED, main='Hab Claims > $250k Incurred')

# Group By Year
large.hab.claims.by.yr <- data.claims.hab.max %>%
                          group_by(AccYr) %>%
                          summarise(n = n())

ggplot(large.hab.claims.by.yr, aes(x = AccYr, y = n)) + 
  geom_bar(stat = "identity", position="dodge") + ggtitle('Habitational Claims Count By Yr > 250k Incurred') +
  theme_minimal() + theme(legend.position="bottom")
