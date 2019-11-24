
###----Tutorial9, include=FALSE--------------------------------------------
library(tidyverse)
###----L9Yollin1,  warning = warn, eval = EVAL-----------------------------
# load libraries
library(sp)
library(rgdal)
library(rgeos)
library(dplyr)

# Create a string containing the location of the shapefile
localDir <- "../Data/wria"
# create a layer name for the shapefiles (text before file extension)
layerName <- "WRIA_poly"
# read data into a SpatialPolygonsDataFrame object
dataProjected <- readOGR(dsn=localDir, layer=layerName)
class(dataProjected)


###----L9Yollin2,  warning = warn, eval = EVAL-----------------------------
# add to data a new column termed "id" composed of the rownames of data
dataProjected@data$id <- rownames(dataProjected@data)
# create a data.frame from our spatial object
watershedPoints <- fortify(dataProjected, region = "id")
head(watershedPoints)
# merge the "fortified" data with the data from our spatial object
watershedDF <- left_join(watershedPoints, dataProjected@data, by = "id")
head(watershedDF)
#Alternative: watershedDF <- merge(watershedPoints, dataProjected@data, by = "id")
# NOTE :
# : An equivalent SQL statement might look something like this:
# : SELECT *
# : FROM dataProjected@data
# : INNER JOIN watershedPoints
# : ON dataProjected@data$id = watershedPoints$id
#OR using the plyr package
# library(plyr)
# watershedDF <- join(watershedPoints, dataProjected@data, by = "id")



###----L9Yollin3,  warning = warn, eval = EVAL-----------------------------
N# Plotting with ggplot2
ggWatershed <- ggplot(data = watershedDF, aes(x=long, y=lat, group = group,
                                              fill = WRIA_NM)) +
  geom_polygon() +
  geom_path(color = "white") +
  scale_fill_hue(l = 40) +
  coord_equal() +
  theme(legend.position = "none", title = element_blank(),
        axis.text = element_blank())
print(ggWatershed)


###----T9quickmap, eval = EVAL---------------------------------------------
worldMap <- ggplot(map_data("world2"), aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "dark green") +
  xlab(NULL) + ylab(NULL)
# worldMap + coord_quickmap()
worldMap + coord_map("mercator")


