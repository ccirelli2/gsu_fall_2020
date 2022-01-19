####################################################################################
#  reference: https://rkabacoff.github.io/datavis/
#  https://r4ds.had.co.nz/data-visualisation.html
####################################################################################

require("ggplot2")

####################################################################################
#  part I: introduction to ggplot2 and understanding the grammar
####################################################################################

## 1.1 Aes mapping 

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))
#> Warning: Using size for a discrete variable is not advised.

## 1.2 Facets

## facet plot by a single variable: facet_wrap()
' **********   Could be usefule in regards to feature inspection'
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)
unique(mpg$class)



## facet plot on the combination of two variables: facet_grid()

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

## not facet in the rows or columns dimension, use a . instead of a variable name
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(. ~ cyl)

## this is equivalent to the following:
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ cyl, nrow = 1)

## 1.3 Geometric objects

# scatter plot
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# smoothed line
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))


x = runif(100, min=0, max=100)
y = runif(100, min=0, max=100)


ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = drv))

## equivalently
ggplot(data = mpg,mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_smooth() + 
  geom_point()

## Exercise
## 1. what's wrong with this code:
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))

## 2. for the salary data, do seprate histogram for each rank
setwd("C:\\Users\\chris.cirelli\\Desktop\\repositories\\gsu_fall_2020\\Visualizations\\data")
salary = read.csv("Salaries.csv")


## 1.4 statistical transformations
## reference: https://ggplot2.tidyverse.org/reference/stat_summary.html
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds) + stat_count(mapping = aes(x = cut))
# every geom has a default stat; and every stat has a default geom

ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, y = stat(prop), group = 1))

ggplot(data = diamonds) + 
  stat_summary( mapping = aes(x = cut, y = depth), 
                fun.min = min, fun.max = max, fun = median )

ggplot(diamonds, aes(cut)) + stat_summary(aes(y = price), fun = "mean", geom = "bar")

## 1.5 position adjustments

ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, colour = cut))

ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = cut))

ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = clarity))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

## jitter helps to show overlapping points
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")

## exercise:

## 1. What is the problem with this plot? How could you improve it?
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point()

## 2. What parameters to geom_jitter() control the amount of jittering?

## 1.6 Coordinate systems  switches the x and y axes

# coor_flip()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + geom_boxplot() + coord_flip()

# coord_polar() uses polar coordinates. 

bar <- ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut), show.legend = FALSE, width = 1) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar
bar + coord_flip()
bar + coord_polar()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + geom_point() +  geom_abline()

####################################################################################
#  part II: A complete example
####################################################################################

install.packages("mosaicData")
require("mosaicData")
## load data
data(CPS85 , package = "mosaicData")

## 1: specify dataset and mapping using ggplot
library(ggplot2)
ggplot(data = CPS85,mapping = aes(x = exper, y = wage))
# Why is the graph empty? 

## 2. geoms

# add points
ggplot(data = CPS85, mapping = aes(x = exper, y = wage)) + geom_point()

# delete outlier
library(dplyr)
plotdata <- filter(CPS85, wage < 40)

# redraw scatterplot
ggplot(data = plotdata, mapping = aes(x = exper, y = wage)) + geom_point()


# 3. extra parameters: make points blue, larger, and semi-transparent
# for a list of colors available

ggplot(data = plotdata, mapping = aes(x = exper, y = wage)) +
  geom_point(color = "cornflowerblue", alpha = .7, size = 3)

# add a line of best fit.
ggplot(data = plotdata, mapping = aes(x = exper, y = wage)) +
  geom_point(color = "cornflowerblue", alpha = .7, size = 3) +
  geom_smooth(method = "lm")

# 4. scales 
# Scales control how variables are mapped to the visual characteristics of the plot.
# modify the x and y axes and specify the colors to be used
ggplot(data = plotdata, mapping = aes(x = exper, y = wage, color = sex)) +
  geom_point(alpha = .7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, size = 1.5) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  scale_y_continuous(breaks = seq(0, 30, 5), label = scales::dollar) +
  scale_color_manual(values = c("indianred3", "cornflowerblue"))

## exercise: for the above plot, why am I getting two regression lines? 
## what should I do if I only want to get one overall regression line?

# 5. facets
# reproduce plot for each level of job sector
ggplot(data = plotdata, mapping = aes(x = exper, y = wage, color = sex)) +
  geom_point(alpha = .7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  scale_y_continuous(breaks = seq(0, 30, 5), label = scales::dollar) +
  scale_color_manual(values = c("indianred3", "cornflowerblue")) +
  facet_wrap(~sector)


# 6. labels

# add informative labels
ggplot(data = plotdata, mapping = aes(x = exper,y = wage,color = sex)) +
  geom_point(alpha = .7) +
  geom_smooth(method = "lm",se = FALSE) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  scale_y_continuous(breaks = seq(0, 30, 5),label = scales::dollar) +
  scale_color_manual(values = c("indianred3","cornflowerblue")) +
  facet_wrap(~sector) +
  labs(title = "Relationship between wages and experience",
       subtitle = "Current Population Survey",
       caption = "source: http://mosaic-web.org/",
       x = " Years of Experience",
       y = "Hourly Wage",
       color = "Gender")

# 7. themes
# Finally, we can fine tune the appearance of the graph using themes. 
# Theme functions (which start with theme_) control background colors, fonts, grid-lines, legend placement, 
# and other non-data related features of the graph
# see https://ggplot2.tidyverse.org/reference/ggtheme.html for a list of complete theme options
ggplot(data = plotdata,mapping = aes(x = exper, y = wage, color = sex)) +
  geom_point(alpha = .6) +
  geom_smooth(method = "lm",se = FALSE) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  scale_y_continuous(breaks = seq(0, 30, 5),label = scales::dollar) +
  scale_color_manual(values = c("indianred3", "cornflowerblue")) +
  facet_wrap(~sector) +
  labs(title = "Relationship between wages and experience",
       subtitle = "Current Population Survey",
       caption = "source: http://mosaic-web.org/",
       x = " Years of Experience",
       y = "Hourly Wage",
       color = "Gender") +
       theme_bw()
