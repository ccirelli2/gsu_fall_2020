####################################################################################
#  reference: https://rkabacoff.github.io/datavis/
#  https://r4ds.had.co.nz/data-visualisation.html
####################################################################################

####################################################################################
#  part I: Univariate Graphs
####################################################################################

# 1.1 Categorical 

## 1.1.1 bar chart

library(ggplot2)
library(dplyr)
data(Marriage, package = "mosaicData")

# plot the distribution of race
ggplot(Marriage, aes(x = race)) + geom_bar()

## add color, label
ggplot(Marriage, aes(x = race)) + 
  geom_bar(fill = "cornflowerblue", color="black") +
  labs(x = "Race", y = "Frequency", title = "Participants by race")

# show percent

ggplot(Marriage, aes(x = race, y = ..count.. / sum(..count..))) + 
  geom_bar() +
  labs(x = "Race", y = "Percent", title  = "Participants by race") +
  scale_y_continuous(labels = scales::percent)

## sorting categories

library(dplyr)
plotdata <- Marriage %>% count(race)

# plot the bars in ascending order
ggplot(plotdata, aes(x = reorder(race, n), y = n)) + 
  geom_bar(stat = "identity") +
  labs(x = "Race", y = "Frequency", title  = "Participants by race")

# plot the bars with numeric labels
ggplot(plotdata, aes(x = race, y = n)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust=-0.5) +
  labs(x = "Race", y = "Frequency", title  = "Participants by race")

## overlapping labels

ggplot(Marriage, aes(x = officialTitle)) + 
  geom_bar() +
  labs(x = "Officiate", y = "Frequency", title = "Marriages by officiate")

## option one: flip the x and y axes

## option two: rotate the axis labels.

ggplot(Marriage, aes(x = officialTitle)) + 
  geom_bar() +
  labs(x = "", y = "Frequency", title = "Marriages by officiate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## 1.1.2 treemap 
install.packages("treemapify")
library(treemapify)

# create a treemap of marriage officials
plotdata <- Marriage %>%
  count(officialTitle)

ggplot(plotdata, aes(fill = officialTitle, area = n)) +
  geom_treemap() + 
  labs(title = "Marriages by officiate")

## with labels

ggplot(plotdata, 
       aes(fill = officialTitle, area = n, label = officialTitle)) +
  geom_treemap() + 
  geom_treemap_text(colour = "white", place = "centre") +
  labs(title = "Marriages by officiate") +
  theme(legend.position = "none")

## 1.2 continuous variables

# histogram

ggplot(Marriage, aes(x = age)) +
  geom_histogram(fill = "cornflowerblue", color = "white", binwidth = 5) + 
  labs(title="Participants by age", subtitle = "binwidth = 5 years", x = "Age")

## kernel density plot

ggplot(Marriage, aes(x = age)) + geom_density() + 
  labs(title = "Participants by age")

####################################################################################
#  part II: Bivariate graphs
####################################################################################

## 1. categorical vs categorical 

ggplot(mpg, aes(x = class, fill = drv)) + geom_bar(position = "fill") +
  labs(y = "Proportion") 

## improving the color and the labeling

# reordered bars, and better labels and colors
library(scales)
ggplot(mpg, aes(x = factor(class, levels = c("2seater", "subcompact", "compact", "midsize", "minivan", "suv", "pickup")),
           fill = factor(drv, levels = c("f", "r", "4"), labels = c("front-wheel", "rear-wheel", "4-wheel")))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), label = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Percent", fill = "Drive Train", x = "Class", title = "Automobile Drive by Class") +
  theme_minimal()
## scale_fill_brewer can be replaced by scale_fill_manual 
## if you want to specify the color manually

## 2. quantitative vs quantitative

## line plot
install.packages("gapminder")
data(gapminder, package="gapminder")
# Select US cases
library(dplyr)
plotdata <- filter(gapminder, country == "United States")

# simple line plot
ggplot(plotdata,aes(x = year, y = lifeExp)) + geom_line() 

ggplot(plotdata, aes(x = year, y = lifeExp)) +
  geom_line(size = 1.5, color = "lightgrey") +
  geom_point(size = 3, color = "steelblue") +
  labs(y = "Life Expectancy (years)", x = "Year",
       title = "Life expectancy changes over time",
       subtitle = "United States (1952-2007)",
       caption = "Source: http://www.gapminder.org/data/")

## 3. Categorical vs. Quantitative
## grouped kernel density plots

setwd("~/Dropbox/teaching_this_semester/MSA8020_DataVisualization/2020 fall/Data/")
Salaries = read.csv("Salaries.csv")

ggplot(Salaries, aes(x = salary,fill = rank)) +
  geom_density(alpha = 0.4) +
  labs(title = "Salary distribution by rank")

## Cleveland Dot Charts

## useful when you want to compare a numeric statistic for a large number of groups.

data(gapminder, package="gapminder")

# subset Asian countries in 2007
library(dplyr)
plotdata <- gapminder %>%
  filter(continent == "Asia" & year == 2007)

# basic Cleveland plot of life expectancy by country
ggplot(plotdata,  aes(x= lifeExp, y = country)) +
  geom_point()

# Sorted Cleveland plot
ggplot(plotdata, aes(x=lifeExp, y=reorder(country, lifeExp))) +
  geom_point()

# Fancy Cleveland plot
ggplot(plotdata, aes(x=lifeExp, y=reorder(country, lifeExp))) +
  geom_point(color="blue", size = 2) +
  geom_segment(aes(x = 40, xend = lifeExp, y = reorder(country, lifeExp), yend = reorder(country, lifeExp)),
               color = "lightgrey") +
  labs (x = "Life Expectancy (years)", y = "", title = "Life Expectancy by Country", subtitle = "GapMinder data for Asia - 2007") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

## many more interesting plots can be found here: https://rkabacoff.github.io/datavis/Bivariate.html
