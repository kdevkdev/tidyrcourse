
###----Lecture9, include=FALSE---------------------------------------------
library(knitr)
library(tidyverse)

###----L9ShapefileChang1,warning = warn, eval = EVAL-----------------------

library(rgdal)
library(sp)
SF <- readOGR(dsn = "../Data/gadm36_SWE_shp/gadm36_SWE_2.shp", layer ="gadm36_SWE_2")  #1
#SF <- readRDS("../Data/gadm36_SWE_2_sp.rds")                  # 1.
class(SF)                                                     # 1.
head(SF@data)                                                 # 2.
#plot(SF)                                                      # 3.
cMap2 <- fortify(SF)                                          # 4.
head(cMap2)                                                   # 5.
myMap <- ggplot(cMap2, aes(x=long, y= lat, group = group))+ geom_path()
#myMap <- myMap + coord_equal()  +ylim(-45,-8) +xlim(110,155)
myMap <- myMap + coord_map("mercator")
#myMap = myMap + coord_map("ortho", orientation = c(-34, 151, 0))
myMap


###----L9BofH1, warning = warn, eval = EVAL--------------------------------
# This is a simple plot of the Burden of Health resulting from Back Pain
# The units are YLD - years lived with disability per 100,000.
# The borders of the Sage countries have been included in RED
# and their names annotated.
# The BofH plot covers most countries. Greenland has been set equal to Iceland
# South Sudan has been set equal to Sudan
# Note that merging has been done on country names so some smaller countries with
# Inconsistent names in the two dataframes may not have had their BofH included
# (Better to use ISO-3 names for uniqueness and consistency)

library(rworldmap)
library(ggplot2)
library(scales)
library(dplyr)

data(countriesLow)                                                    #1.
plot(countriesLow)
class(countriesLow)                                                   #1.
head(countriesLow@data)
wmap_df <- fortify(countriesLow)  # Converts  to a data.frame
head(wmap_df)
names(wmap_df)[6] <- "NAME"
#------------------------------------------------------------------------------
bp50_69 <- read.csv("../Data/YLD_50-69.csv",header=TRUE,stringsAsFactors=FALSE)   #2
head(bp50_69)    # Just country name and YLD

# merge the dataframes
map.df <- left_join(wmap_df, bp50_69, by = "NAME")             #3.
map.df <- left_join(wmap_df, bp50_69)             #3.
map.df
map2 <- arrange(map.df,order) # sort to original polygon order

breaks <- c(1500, 2000, 2500, 3000, 3500, 4000, 4500)  # For legend

theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.background = element_blank(),
                         plot.background = element_rect(fill="lightblue"),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         plot.title = element_text(size=22)))            #4.

pp <- ggplot(data = map2, aes(x=long, y=lat,group=group, fill=YLDs)) +
  #pp <- ggplot(data = map2, aes(x=long, y=lat, fill=YLDs)) +
  scale_fill_gradientn(colours = rainbow(7),na.value="white",
                       breaks = breaks, labels = format(breaks))+
  coord_fixed() +
  geom_polygon(colour="white")
pp                                                                       #5.
#-------------------------------------------------------------------------------
# Now we add the SAGE country outlines

SageCountry <- c("China", "Ghana","India", "Mexico", "Russia", "South Africa")
# strings for names on map
labelLong <-   c(   100.,   -10.,    66.,     -120.,     90.,         20.)
# location for country name on map
labelLat  <-   c(   37.,     0.,     18.,      20.,      60.,        -40.)
# location for country name on map

# Now construct a data.frame for ggplot
cL  <- data.frame( long = labelLong, lat= labelLat,                     #6.
                   txt = SageCountry,stringsAsFactors=FALSE )

# First we plot the SAGE country borders
for ( i in 1:length(SageCountry)){                                      #7.
  bb <-  map2[map2$NAME== SageCountry[i],]  # grab the Sage country borders
  pp <- pp + geom_path(data = bb,aes(x=long,y=lat), colour = "magenta",size=1)
  rm(bb)  #
}

# Now do the annotation of country names
pp <- pp + annotate("text", x = cL$long, y = cL$lat,                  #8.
                    colour= "black",
                    size  = 4,
                    label = cL$txt)

# Include a title for the plot
tt1 <- "Years lived with disability (YLDs) per 100,000 - Group age 50-69 "
pp <- pp +
  ggtitle(tt1)+
  theme(plot.title = element_text(size = rel(2), colour = "blue"))
pp                                                                     #9.



###----L9theme_clean, warning = warn, eval = EVAL--------------------------
theme_clean <- function(base_size = 12) {
  require(grid) # Needed for unit() function
  theme_grey(base_size) %+replace%
    theme(
      axis.title        = element_blank(),
      axis.text         = element_blank(),
      panel.background  = element_blank(),
      panel.grid        = element_blank(),
      axis.ticks.length = unit(0, "cm"),
      panel.spacing     = unit(0, "lines"),
      plot.margin       = unit(c(0, 0, 0, 0), "lines"),
      #      plot.background   = element_rect(colour = "black"),  # leave out for no box
      complete = TRUE
    )
}
# Attach it t the pp object:
pp + theme_clean()


###----L9readOGR,  warning = warn, eval = EVAL-----------------------------
library(maps)
library(ggplot2)
library(rgdal)

# read shapefile
wmap <- readOGR(dsn = "../Data/ne_110m_admin_0_countries",
                layer="ne_110m_admin_0_countries")
class(wmap)    # SpatialPolygonsDataFrame
Amap <- wmap[wmap$name == "Australia",]      # Subsetting to grab Australian data
plot(Amap)
wmap_df <- fortify(wmap, regions = ISO_A2)  # convert SpatialPolygonsDataFrame to a dataframe
head(wmap_df)

base_world <- ggplot(data = wmap_df, aes(x = long, y = lat)) + coord_fixed()+
  
  geom_polygon(aes(group = group), fill = "wheat") +
  geom_path(aes(group = group), colour = "grey40") +
  theme(panel.background  = element_rect(fill = "lightsteelblue2", colour = "grey"),
        panel.grid.major  = element_line(colour = "grey90"),
        axis.ticks        = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        axis.text.x       = element_text(size = 14, vjust = 0),
        axis.text.y       = element_text(size = 14, hjust = 0.3)) +
  labs(y="",x="")
base_world
