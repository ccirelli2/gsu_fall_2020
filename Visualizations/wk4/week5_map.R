## reference: https://rkabacoff.github.io/datavis/GeoMaps.html#dot-density-maps
## https://www.littlemissdata.com/blog/maps
## courses and videos on choroplethr: https://www.census.gov/data/academy/courses/choroplethr.html

## main packages: ggmap and choroplethr

setwd("D:/ycData/Dropbox/teaching_this_semester/MSA8020_DataVisualization/2020 fall/Data/")
setwd("~/Dropbox/teaching_this_semester/MSA8020_DataVisualization/2020 fall/Data/")

#################################################
##
## part 1: dot density maps using ggmap
##
###################################################

install.packages("ggmap")
library("ggmap")
data(crime, package="ggmap")

# subset the data
library(dplyr)
rapes <- filter(crime, offense == "rape") %>%
  select(date, offense, address, lon, lat)

# view data
head(rapes)

register_google(key = "your API key")

key = read.table("../presentation/Week5_map_ts/API.txt",header=FALSE)
register_google(key = key)

houston_center <- geocode("Houston, TX")


houston_map <- get_map(c(lon=-95.3698, lat=29.76043), 
                       zoom = 12, maptype = "roadmap")
dim(houston_map)
save(houston_map,file="houston_map.Rda")

ggmap(houston_map) ## you can change the number after zoom to control the size of the map

# add incident locations
ggmap(houston_map,base_layer = ggplot(data = rapes, aes(x=lon, y = lat))) +
  geom_point(color = "red", size = 3, alpha = 0.5)

# remove long and lat numbers and add titles
ggmap(houston_map, 
      base_layer = ggplot(aes(x=lon, y = lat), data = rapes)) +
  geom_point(color = "red", size = 3, alpha = 0.5) +
  theme_void() +
  labs(title = "Location of reported rapes",
       subtitle = "Houston Jan - Aug 2010",
       caption = "source: http://www.houstontx.gov/police/cs/")

########################################
##
## part II: Choropleth maps
## required packages: choroplethrMaps, choroplethr
##
################################################

install.packages("choroplethrMaps") # Contains Maps Used by the 'choroplethr' Package
#Contains 3 maps. 1) US States 2) US Counties 3) Countries of the world.
require("choroplethrMaps")
#install.packages("devtools")
#library(devtools)
#install_github("trulia/choroplethrMaps")
#install_github("trulia/choroplethr") ## not working
#install.packages("XML", repos = "http://www.omegahat.net/R") ## not working

install.packages("choroplethr",dependencies = TRUE)
#install.packages("XML", type = "binary")
#install.packages("acepack")
require("choroplethr")
library(dplyr) ## for filter

########################################
## function: country_choropleth
################################################

data(gapminder, package = "gapminder")
plotdata <- gapminder %>%
  filter(year == 2007) %>%
  rename(region = country, value = lifeExp) %>%
  mutate(region = tolower(region))

#Now lets create the map.
country_choropleth(plotdata)


data(country.map, package = "choroplethrMaps")

data(gapminder, package = "gapminder")
plotdata <- gapminder %>%
  filter(year == 2007) %>%
  rename(region = country,
         value = lifeExp) %>%
  mutate(region = tolower(region)) %>%
  mutate(region = recode(region,
                         "united states"    = "united states of america",
                         "congo, dem. rep." = "democratic republic of the congo",
                         "congo, rep."      = "republic of congo",
                         "korea, dem. rep." = "south korea",
                         "korea. rep."      = "north korea",
                         "tanzania"         = "united republic of tanzania",
                         "serbia"           = "republic of serbia",
                         "slovak republic"  = "slovakia",
                         "yemen, rep."      = "yemen"))
country_choropleth(plotdata)

## change palette and add titles, captions.
country_choropleth(plotdata, num_colors=9) +
  scale_fill_brewer(palette="YlOrRd") +
  labs(title = "Life expectancy by country", subtitle = "Gapminder 2007 data",
       caption = "source: https://www.gapminder.org", fill = "Years")

########################################
## function: state_choropleth
################################################


library(ggplot2)
library(choroplethr)

data(continental_us_states)
head(continental_us_states)

# see a list of available functions and data sets
# https://cran.r-project.org/web/packages/choroplethr/choroplethr.pdf

# input the data
library(readr)
mex_am <- read_csv("mexican_american.csv")

mex_am <- read_tsv("mexican_american.csv")

head(mex_am)

# prepare the data
mex_am$region <- tolower(mex_am$state)
mex_am$value <- mex_am$percent

# create the map
# The zoom = continental_us_states option will create a map that excludes Hawaii and Alaska.
library("")
register_google(key = key)
state_choropleth(mex_am, num_colors=9, zoom = continental_us_states) +
  scale_fill_brewer(palette="YlOrBr") +
  labs(title = "Mexican American Population",
       subtitle = "2010 US Census",
       caption = "source: https://en.wikipedia.org/wiki/List_of_U.S._states_by_Hispanic_and_Latino_population",
       fill = "Percent") 

###############################################
## function: county_choropleth
################################################

library(ggplot2)
library(choroplethr)
library(dplyr)

# enter violent crime rates by county
crimes_ct <- data.frame(
  county = c("fairfield", "hartford", "litchfield", "middlesex", "new haven", "new london", "tolland", "windham"),
  value = c(3.00, 3.32, 1.02, 1.24, 4.13, 4.61, 0.16, 1.60)
)

crimes_ct

# Our dataset has country names (e.g. fairfield). However, we need region codes (e.g., 9001). 
# We can use the county.regions dataset to lookup the region code for each county name.

## get the region codes from county.regions data set
data(county.regions, package = "choroplethrMaps")
region <- county.regions %>% filter(state.name == "connecticut")

region


plotdata <- inner_join(crimes_ct, region, by=c("county" = "county.name"))
plotdata

## use the option reference_map = TRUE to add a reference map from Google Maps.
library(ggmap)
register_google(key = "your API key")
county_choropleth(plotdata, state_zoom = "connecticut", 
                  #reference_map = TRUE,
                  num_colors = 8) +
  scale_fill_brewer(palette="YlOrRd") +
  labs(title = "Connecticut Violent Crime Rates", subtitle = "FBI 2012 data",
       caption = "source: https://ucr.fbi.gov", fill = "Violent Crime\n Rate Per 1000")

## you can also use choroplethrZip package to plot at zip code level

##############################################################################
##
## Exercise: Make a choropleth map showing per-capita income by state.
##
###############################################################################

#To do this we'll use the data set df_state_demographics from choroplethr package

#Recall that state_choropleth requires one column to be named region and one column to be named value. 
