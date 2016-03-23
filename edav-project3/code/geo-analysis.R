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

shp <- readOGR("./data/nynta_16a/","nynta") %>% spTransform(CRS("+proj=longlat +datum=WGS84"))