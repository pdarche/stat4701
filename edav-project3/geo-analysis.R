install.packages('rgdal')
install.packages("gpclib", type="source")
install.packages('rgeos')

library(rgdal)
library(ggplot2)
library(dplyr)
library(plyr)
library(gpclib)
library(rgeos)
library(maptools)
require("maptools")

demo <- read.csv('./data/demo.csv')
shp <- readOGR("./data/nynta_16a/","nynta") %>% spTransform(CRS("+proj=longlat +datum=WGS84"))
shp.f = shp %>% fortify(region = 'NTACode')
merged <- merge(shp.f, shp@data, by.x = 'id', by.y = 'NTACode')
ggplot(merged, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = MdAgeE))