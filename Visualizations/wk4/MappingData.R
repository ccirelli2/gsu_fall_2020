# Free Trial
'
https://cloud.google.com/maps-platform/?utm_source=google&utm_medium=cpc&utm_campaign=FY18-Q2-global-demandgen-paidsearchonnetworkhouseads-cs-maps_contactsal_saf&utm_content=text-ad-none-none-DEV_c-CRE_460848633793-ADGP_Hybrid+%7C+AW+SEM+%7C+BKWS+~+Google+Maps-KWID_43700036076725537-kwd-21146301231-userloc_9010943&utm_term=KW_%2Bgoogle%20%2Bmap-ST_%2Bgoogle+%2Bmap&gclid=Cj0KCQjwnqH7BRDdARIsACTSAdtQ6GOfzu92FVlYwnXbxElSOvHZqfnHowwYdR1KuzYPhAg5fKzI_jEaAi4EEALw_wcB

API =       AIzaSyAYlABxxPJxY3yRFlBEoqCDIdeDxKD0-Xw
MapAPIKey = AIzaSyAYlABxxPJxY3yRFlBEoqCDIdeDxKD0-Xw

Howto Python 
          https://medium.com/future-vision/google-maps-in-python-part-2-393f96196eaf

'
# Install Package
install.packages("ggmap")
library("ggmap")
library(dplyr)

# Load Data
data(crime, package="ggmap")
head(crime)
my.key = "AIzaSyAYlABxxPJxY3yRFlBEoqCDIdeDxKD0-Xw"
locations <- filter(crime, crime$offense =='rape') %>% select(date, offense, address, lon, lat)
locations$

register_google(key=my.key)

houston_center <- geocode("Houston, TX")
lon.houston <- houston_center$lon
lat.houston <- houston_center$lat

houston_map <- get_map(c(lon=lon.houston, lat=lat.houston),
                       zoom=10, maptype="roadmap")

'Error: Remove 228 rows, becuase the map is not big enough'
ggmap(houston_map, base_layer=ggplot(data=locations, aes(x=lon, y=lat))) +
  geom_point(color = "red", size = 3, alpha = 0.5)



install.packages("acepack")
install.packages("choroplethrMaps")
install.packages("gapminder")
require("acepack")
require("choroplethrMaps")
require("gapminder")

data(gapminder, package = "gapminder")
plotdata <- gapminder %>%
  filter(year == 2007) %>%
  rename(region = country, value = lifeExp) %>%
  mutate(region = tolower(region))

