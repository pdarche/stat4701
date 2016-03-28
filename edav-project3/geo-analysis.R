install.packages('rgeos')
install.packages('rgdal')
install.packages("gpclib", type="source")
install.packages("ineq")

library(rgdal)
library(ggplot2)
library(plyr)
library(dplyr)
library(gpclib)
library(rgeos)
library(maptools)
library(gridExtra)
require("maptools")
library(ineq)

# set the working directory
setwd('~/Desktop/qmss/stat4701/edav-project3/')
# read in the demography data
demo <- read.csv('./data/demo.csv')
# read in the race data
race.melted <- read.csv('./data/race_melt.csv')
age.melted <- read.csv('./data/age_melt.csv')
gender.melted <- read.csv('./data/gender_melt.csv')
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


# Zoom in on Brownsville & Chelsea
sb <- ggplot(merged[merged$id %in% c('BK81', 'MN13'),], aes(long, lat, group=group)) +
    geom_polygon(aes(fill = BlNHE)) +
    scale_fill_gradient(low='white', high='red', na.value = 'white')

sw <- ggplot(merged[merged$id %in% c('BK81', 'MN13'),], aes(long, lat, group=group)) +
  geom_polygon(aes(fill = WtNHE)) +
  scale_fill_gradient(low='white', high='orange', na.value = 'white')

sh <- ggplot(merged[merged$id %in% c('BK81', 'MN13'),], aes(long, lat, group=group)) +
  geom_polygon(aes(fill = HspE)) +
  scale_fill_gradient(low='white', high='yellow', na.value = 'white')

sa <- ggplot(merged[merged$id %in% c('BK81', 'MN13'),], aes(long, lat, group=group)) +
  geom_polygon(aes(fill = AsnNHE)) +
  scale_fill_gradient(low='white', high='green', na.value = 'white')

grid.arrange(sb, sw, sh, sa, ncol=2)

mean_bl = mean(as.integer(demo$BlNHE), na.rm = T)
mean_white = mean(as.integer(demo$WtNHE), na.rm = T)
mean_hisp = mean(as.integer(demo$HspE), na.rm = T)
mean_asian = mean(as.integer(demo$AsnNHE), na.rm = T)

b_race <- ggplot(bvil, aes(x=reorder(race, -estimate), y=estimate)) + 
  geom_bar(stat='identity', fill='steelblue') +
  geom_hline(aes(yintercept = mean_bl, linetype="city avg"), linetype='dotted') + 
  # geom_hline(aes(yintercept = mean_hisp, linetype="city avg"), linetype='dotted') + 
  # geom_text(aes(1, mean_bl,label = "Citywide Average", vjust = -1)) + 
  labs(title = "Brownsvill Population Estimates by Race",
       x = "Race",
       y = "Estimated Population")

chels = race.melted[race.melted$id=='MN13',]
c_race <- ggplot(chels, aes(x=reorder(race, -estimate), y=estimate)) + 
  geom_bar(stat='identity', fill='steelblue') +
  geom_hline(aes(yintercept = mean_white, linetype="city avg"), linetype='dotted') + 
  # geom_hline(aes(yintercept = mean_hisp, linetype="city avg"), linetype='dotted') + 
  # geom_text(aes(1, mean_bl,label = "Citywide Average", vjust = -1)) + 
  labs(title = "Chelsea Population Estimates by Race",
       x = "Race",
       y = "Estimated Population")

grid.arrange(b_race, c_race, row=2)

# Top 25 African-American Populations
ordered <- demo[order(demo$BlNHE, decreasing = T),]
ggplot(head(ordered, 25), aes(x=reorder(GeogName, -BlNHE), y=BlNHE)) + 
  geom_bar(stat='identity', fill='steelblue') + 
  labs(title = 'Top 25 Neighborhoods by African-American Population',
       y = 'African American Population Estimate',
       x = 'Neighborhood') + 
  theme(axis.text.x = element_text(angle = -60, hjust=0))

# Top 25 Whit Populations
ordered <- demo[order(demo$WtNHE, decreasing = T),]
ggplot(head(ordered, 25), aes(x=reorder(GeogName, -WtNHE), y=WtNHE)) + 
  geom_bar(stat='identity', fill='steelblue') + 
  labs(title = 'Top 25 Neighborhoods by White Population',
       y = 'African American Population Estimate',
       x = 'Neighborhood') + 
  theme(axis.text.x = element_text(angle = -60, hjust=0))

######### AGE #########
a <- ggplot(merged, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = Pop20t24E)) +
  labs(title = "Population of 20 to 24 Year-olds by Neighborhood",
       x = "latitude", y = "Longitude") +
  scale_fill_gradient(low='white', high='red', na.value = 'white')

b <- ggplot(merged, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = Pop25t34E)) +
  labs(title = "Population of 25 to 34 Year-olds by Neighborhood",
       x = "latitude", y = "Longitude") +
  scale_fill_gradient(low='white', high='orange', na.value = 'white')

c <- ggplot(merged, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = Pop35t44E)) +
  labs(title = "Population of 35 to 44 Year-olds by Neighborhood",
       x = "latitude", y = "Longitude") +
  scale_fill_gradient(low='white', high='yellow', na.value = 'white')

d <- ggplot(merged, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = Pop45t54E)) +
  labs(title = "Population of 45 to 54 Year-olds by Neighborhood",
       x = "latitude", y = "Longitude") +
  scale_fill_gradient(low='white', high='green', na.value = 'white')

e <- ggplot(merged, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = Pop55t59E)) +
  labs(title = "Population of 55 to 59 Year-olds by Neighborhood",
       x = "latitude", y = "Longitude") +
  scale_fill_gradient(low='white', high='blue', na.value = 'white')

f <- ggplot(merged, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = Pop15t19E)) +
  labs(title = "Population of 15 to 19 Year-olds by Neighborhood",
       x = "latitude", y = "Longitude") +
  scale_fill_gradient(low='white', high='green', na.value = 'white')

grid.arrange(f, a, b, c, d, e, ncols=2)

# Age Distribution
bv_age = age.melted[age.melted$id=='BK81',]
b_age <- ggplot(bv_age, aes(x=age, y=estimate)) + 
  geom_bar(stat='identity', fill='steelblue') +
  labs(title = "Brownsvill Population Estimates by Age",
       x = "Age",
       y = "Estimated Population") +
  theme(axis.text.x = element_text(angle = -60, hjust=0))
 

ch_age = age.melted[age.melted$id=='MN13',]
c_age <- ggplot(ch_age, aes(x=age, y=estimate)) + 
  geom_bar(stat='identity', fill='steelblue') +
  labs(title = "Chelsea Population Estimates by Age",
       x = "Age",
       y = "Estimated Population") +
  theme(axis.text.x = element_text(angle = -60, hjust=0))
  
grid.arrange(b_age, c_age, ncol=2)


######## GENDER #########
# Gender Distribution
bv_gen = gender.melted[age.melted$id=='BK81',]
b_gen <- ggplot(bv_gen, aes(x=gender, y=estimate)) + 
  geom_bar(stat='identity', fill='steelblue') +
  labs(title = "Brownsvill Population Estimate by Gender",
       x = "Gender",
       y = "Estimated Population") +
  theme(axis.text.x = element_text(angle = -60, hjust=0))

ch_gen = gender.melted[age.melted$id=='MN13',]
c_gen <- ggplot(ch_gen, aes(x=gender, y=estimate)) + 
  geom_bar(stat='identity', fill='steelblue') +
  labs(title = "Chelsea Population Estimates by Gender",
       x = "Gender",
       y = "Estimated Population") +
  theme(axis.text.x = element_text(angle = -60, hjust=0))

grid.arrange(b_gen, c_gen, ncol=2)

# Density by Neighborhood
m <- ggplot(merged, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = MaleE)) +
  labs(title = "Population of Men by Neighborhood",
       x = "latitude", y = "Longitude") +
  scale_fill_gradient(low='white', high='green', na.value = 'white')

f <- ggplot(merged, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = FemE)) +
  labs(title = "Population of Women by Neighborhood",
       x = "latitude", y = "Longitude") +
  scale_fill_gradient(low='white', high='purple', na.value = 'white')

grid.arrange(m, f, col=2)
