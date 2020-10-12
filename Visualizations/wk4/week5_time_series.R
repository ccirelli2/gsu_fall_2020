## references: https://rkabacoff.github.io/datavis/Time.html

##################################################
##
## part 1: time series
##
#################################################

library(ggplot2)
ggplot(economics, aes(x = date, y = psavert)) +
  geom_line() +
  labs(title = "Personal Savings Rate", x = "Date", y = "Personal Savings Rate")

## The scale_x_date function can be used to reformat dates
library(ggplot2)
library(scales)
ggplot(economics, aes(x = date, y = psavert)) +
  geom_line(color = "indianred3", size=1 ) +
  geom_smooth() +
  scale_x_date(date_breaks = '5 years', labels = date_format("%b-%y")) +
  labs(title = "Personal Savings Rate", subtitle = "1967 to 2015", x = "", y = "Personal Savings Rate") +
  theme_minimal()


## # multivariate time series

# install.packages("quantmod")

library(quantmod)
library(dplyr)

# get apple (AAPL) closing prices
getSymbols("AAPL", return.class = "data.frame", from="2018-01-01", to="2018-08-31")

apple <- AAPL %>% 
  mutate(Date = as.Date(row.names(.))) %>%
  select(Date, AAPL.Close) %>%
  rename(Close = AAPL.Close) %>%
  mutate(Company = "Apple")

# get facebook (FB) closing prices
getSymbols("FB", return.class = "data.frame", from="2018-01-01", to="2018-08-31")

facebook <- FB %>% 
  mutate(Date = as.Date(row.names(.))) %>%
  select(Date, FB.Close) %>%
  rename(Close = FB.Close) %>%
  mutate(Company = "Facebook")

# combine data for both companies
mseries <- rbind(apple, facebook)

# plot data
library(ggplot2)
ggplot(mseries, aes(x=Date, y= Close, color=Company)) + 
  geom_line(size=1) +
  scale_x_date(date_breaks = '1 month', labels = scales::date_format("%b")) +
  scale_y_continuous(limits = c(20, 220), breaks = seq(20, 220, 10), labels = scales::dollar) +
  labs(title = "NASDAQ Closing Prices", subtitle = "Jan - Aug 2018",
       caption = "source: Yahoo Finance", y = "Closing Price") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2")

##################################################
##
## part 2: Dummbbell charts
##
#################################################
install.packages("ggalt") # for geom_dumbbell
library(ggalt)
library(tidyr) # for spread
library(dplyr)

# load data
data(gapminder, package = "gapminder")

# subset data
plotdata_long <- filter(gapminder, continent == "Americas" & year %in% c(1952, 2007)) %>%
  select(country, year, lifeExp)

# convert data to wide format
plotdata_wide <- spread(plotdata_long, year, lifeExp)
names(plotdata_wide) <- c("country", "y1952", "y2007")

# create dumbbell plot
ggplot(plotdata_wide, aes(y = country, x = y1952, xend = y2007)) + geom_dumbbell()


# create dumbbell plot after sorting
ggplot(plotdata_wide, aes(y = reorder(country, y1952), x = y1952, xend = y2007)) +  
  geom_dumbbell(size = 1.2, size_x = 3, size_xend = 3, colour = "grey", colour_x = "blue", colour_xend = "red") +
  theme_minimal() + 
  labs(title = "Change in Life Expectancy", subtitle = "1952 to 2007", x = "Life Expectancy (years)", y = "")


##################################################
##
## part 3: slope graphs
##
#################################################

# To create a slope graph, we'll use the newggslopegraph function from the CGPfunctions package.

# The newggslopegraph function parameters are (in order)
#  data frame
#  time variable (which must be a factor)
#  numeric variable to be plotted
#  and grouping variable (creating one line per group).

install.packages("CGPfunctions")
library(CGPfunctions)

# Select Central American countries data 
# for 1992, 1997, 2002, and 2007

df <- gapminder %>%
  filter(year %in% c(1992, 1997, 2002, 2007) &
           country %in% c("Panama", "Costa Rica", "Nicaragua", "Honduras", "El Salvador", "Guatemala", "Belize")) %>%
  mutate(year = factor(year), lifeExp = round(lifeExp)) 

# create slope graph

newggslopegraph(df, year, lifeExp, country) +
  labs(title="Life Expectancy by Country", subtitle="Central America", caption="source: gapminder")


##################################################
##
## part 4: Area Charts
##
#################################################


# A simple area chart is basically a line graph, with a fill from the line to the x-axis.

# basic area chart
ggplot(economics, aes(x = date, y = psavert)) +
  geom_area(fill="lightblue", color="black") +
  labs(title = "Personal Savings Rate", x = "Date", y = "Personal Savings Rate")

# A stacked area chart can be used to show differences between groups over time.
install.packages("gcookbook")
# stacked area chart
data(uspopage, package = "gcookbook")
ggplot(uspopage, aes(x = Year, y = Thousands, fill = AgeGroup)) +
  geom_area() +
  labs(title = "US Population by age", x = "Year", y = "Population in Thousands")

## exercise: plot a stacked bar instead.


# stacked area chart
data(uspopage, package = "gcookbook")
ggplot(uspopage, aes(x = Year, y = Thousands/1000, fill = forcats::fct_rev(AgeGroup))) +
  geom_area(color = "black") +
  labs(title = "US Population by age", subtitle = "1900 to 2002",
       caption = "source: U.S. Census Bureau, 2003, HS-3",
       x = "Year", y = "Population in Millions", fill = "Age Group") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()
