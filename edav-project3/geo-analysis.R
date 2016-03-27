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
require("maptools")

setwd('~/Desktop/qmss/stat4701/edav-project3/')

demo <- read.csv('./data/demo.csv')
race.melted <- read.csv('./data/race_melt.csv')

shp <- readOGR("./data/nynta_16a/","nynta") %>% spTransform(CRS("+proj=longlat +datum=WGS84"))
shp.f = shp %>% fortify(region = 'NTACode')
merged <- merge(shp.f, demo, by.x = 'id', by.y = 'GeoID')


p1 <- ggplot(merged, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = BlNHE)) +
  labs(title = "Black Population by Neighborhood",
       x = "latitude", y = "Longitude")

p2 <- ggplot(merged[merged$id=='BK81',], aes(long, lat, group=group)) +
    geom_polygon(colour="black", fill=NA)

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

# Top 25 African-American Populations
ggplot(head(demo, 25), aes(x=reorder(GeogName, -BlNHE), y=BlNHE)) + 
  geom_bar(stat='identity', fill='steelblue') + 
  labs(title = 'Top 25 Neighborhoods by African-American Population') #+ 
  # theme(axis.text.x = element_text(angle = 90))

