# Homework 1 ------------------------------------------------

# Clear Namespace
rm(list=ls())

# Import Packages
library(dplyr)
library(DBI)
library(readxl)
library(fitdistrplus)
library(Hmisc)
library(skimr)
library(writexl)

# Set Working Directory
setwd("C:\\Users\\chris.cirelli\\Desktop\\repositories\\gsu_fall_2020\\Basic_rate_making\\data")

# Read Data
data <- read.csv("lossdata.csv")
col.names <- colnames(data)

# Create Data Dict
col.names <- colnames(data)
skm <- skim(data)
hd <- data.frame(t(head(data)))
hd$colnames <- col.names
mrg <- merge(x=skm, y=hd, by.x = 'skim_variable', by.y='colnames')
write_xlsx(mrg, paste('data_dict.xlsx'))

###############################################################
## QUESTIONS 1 & 2
###############################################################

# Distribution  - Loss
hist(data$Loss, breaks=50, main='Loss Data', xlab='Incurred Amount')

# Fit Distribution To Data - Loss
plotdist(data$Loss, histo=TRUE, demp=TRUE)
descdist(data$Loss, boot=1000)


# Distribution ALAE
hist(data$ALAE, breaks=50, main='ALAE', xlab='ALAE')
plotdist(data$ALAE, histo=TRUE, demp=TRUE)
descdist(data$ALAE, boot=1000)


d.gamma <- fitdist(data$Loss/1000000, discrete=FALSE, "gamma", method='mle')
d.gamma
summary(d.gamma)

# Parameters
alpha = .506
beta  = 1/ 0.0152
mu = alpha * beta
std.dv = sqrt(alpha * beta ^2)
range = seq(0, mu + 10 * std.dv, 0.2)

# Draw Gamma Distribution 
y = dgamma(x = range, shape = alpha, rate = 1/beta)
plot(range, y, type='l', main='Gamma Distribution Fit 2 Loss Data', 
     xlab = 'incurred (per 100,000)', ylab = 'percentage')




###############################################################
## QUESTIONS 3
###############################################################

# Separate Loss Into Two Vectors
loss <- data$Loss
loss.1.0k <- loss[1:1000]
loss.1.5k <- loss[1001:1500]

length(loss.1.0k)
length(loss.1.5k)

# Apply Caps
loss.1.0k.capped <- replace(loss.1.0k, loss.1.0k > 5000, 5000)
loss.1.5k.capped <- replace(loss.1.5k, loss.1.5k > 25000, 25000)
loss.capped <- c(loss.1.0k.capped, loss.1.5k.capped)

# Plot Capped Losses
plotdist(loss.capped)
