install.packages('rgeos')
install.packages('rgdal')
install.packages("gpclib", type="source")

library(rgdal)
library(ggplot2)
library(plyr)
library(dplyr)
library(gpclib)
library(rgeos)
library(maptools)
library(gridExtra)
require("maptools")

# set the working directory
setwd('~/Desktop/qmss/stat4701/edav-project3/')
# read in the demography data
demo <- read.csv('./data/demo.csv')
# read in the race data
race.melted <- read.csv('./data/race_melt.csv')
# Prep the shape files
shp <- readOGR("./data/nynta_16a/","nynta") %>% spTransform(CRS("+proj=longlat +datum=WGS84"))
shp.f = shp %>% fortify(region = 'NTACode')
merged <- merge(shp.f, demo, by.x = 'id', by.y = 'GeoID')

###### DEMOGRAPHICS ######
# New York is a divers place.  In it's 195 Neighborhoods  

# NYC Black
# TODO: (1) remove axes from and axis labels from chart, (2) Facet/trellis by race
cb <- ggplot(merged, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = BlNHE)) +
  labs(title = "Black Population by Neighborhood",
       x = "latitude", y = "Longitude") +
  scale_fill_gradient(low='white', high='red', na.value = 'white')

# NYC White
cw <- ggplot(merged, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = WtNHE)) +
  labs(title = "White Population by Neighborhood",
       x = "latitude", y = "Longitude") +
  scale_fill_gradient(low='white', high='orange', na.value = 'white')

# NYC Hisp
ch <- ggplot(merged, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = HspE)) +
  labs(title = "Hist Population by Neighborhood",
       x = "latitude", y = "Longitude") + 
  scale_fill_gradient(low='white', high='yellow', na.value = 'white')

# NYC Asian
ca <- ggplot(merged, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = AsnNHE)) +
  labs(title = "Asian Population by Neighborhood",
       x = "latitude", y = "Longitude") + 
  scale_fill_gradient(low='white', high='green', na.value = 'white')


grid.arrange(cb, cw, ch, ca, ncol=2)

# Zoom in on Brownsville 
p2 <- ggplot(merged[merged$id=='BK81',], aes(long, lat, group=group)) +
    geom_polygon(colour="black", fill=NA)
p2

mean_bl = mean(as.integer(demo$BlNHE), na.rm = T)
mean_hisp = mean(as.integer(demo$HspE), na.rm = T)
b_race <- ggplot(bvil, aes(x=reorder(race, -estimate), y=estimate)) + 
  geom_bar(stat='identity', fill='steelblue') +
  geom_hline(aes(yintercept = mean_bl, linetype="city avg"), linetype='dotted') + 
  geom_hline(aes(yintercept = mean_hisp, linetype="city avg"), linetype='dotted') + 
  # geom_text(aes(1, mean_bl,label = "Citywide Average", vjust = -1)) + 
  labs(title = "Brownsvill Population Estimates by Race",
       x = "Race",
       y = "Estimated Population")
b_race

# Top 25 African-American Populations
ordered <- demo[order(demo$BlNHE, decreasing = T),]
ggplot(head(ordered, 25), aes(x=reorder(GeogName, -BlNHE), y=BlNHE)) + 
  geom_bar(stat='identity', fill='steelblue') + 
  labs(title = 'Top 25 Neighborhoods by African-American Population',
       y = 'African American Population Estimate',
       x = 'Neighborhood') + 
  theme(axis.text.x = element_text(angle = -60, hjust=0))

