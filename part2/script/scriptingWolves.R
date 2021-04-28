# Install the relevant libraries - do this one time
# install.packages("lubridate")
# install.packages("ggplot2")
# install.packages("data.table")
# install.packages("ggrepel")
# install.packages("dplyr")
# install.packages("data.table")
# install.packages("tidyverse")

#Load the relevant libraries
library(lubridate)
# # library(ggplot2)
# # library(dplyr)
# library(data.table)
# library(ggrepel)
library(ggmap)
library(tidyverse)
library(rgdal)
library(sp)
library(maptools)
library(ggspatial)
# setup
register_google(key = "AIzaSyDvpJHmU1qbEdJLqgF-PwgxKcPf0BvtThE")

# functions
# points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
#   
#   # Convert to SpatialPointsDataFrame
#   coordinates(data) <- c(long, lat)
#   
#   # If there is a sort field...
#   if (!is.null(sort_field)) {
#     if (!is.null(id_field)) {
#       data <- data[order(data[[id_field]], data[[sort_field]]), ]
#     } else {
#       data <- data[order(data[[sort_field]]), ]
#     }
#   }
#   
#   # If there is only one path...
#   if (is.null(id_field)) {
#     
#     lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
#     
#     return(lines)
#     
#     # Now, if we have multiple lines...
#   } else if (!is.null(id_field)) {  
#     
#     # Split into a list by ID field
#     paths <- sp::split(data, data[[id_field]])
#     
#     sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
#     
#     # I like for loops, what can I say...
#     for (p in 2:length(paths)) {
#       id <- paste0("line", as.character(p))
#       l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
#       sp_lines <- spRbind(sp_lines, l)
#     }
#     
#     return(sp_lines)
#   }
# }

# Load data file and assign variables
movementData <- "part2/data/ggplotVisualize/ABoVE_ Hebblewhite Alberta-BC Wolves.csv" %>% read.csv()
movementData %>% head()

# convert into spatialPoints dataframe WGS 84
movementSpatialData <- movementData %>% select(location.long, location.lat) %>% SpatialPointsDataFrame(data = movementData, proj4string = CRS("+init=epsg:32648"))

# identify the UTM zones of the data >> convert the data into BC Albers
UTM_z <- movementData %>% select(utm.zone) %>% pull() %>% unique()
# since there are more than 1 UTM zone for the area of interest, select the appropriate projection system, i.e. BC Albers (rationale desc. here: https://ibis.geog.ubc.ca/~brian/Course.Notes/bceprojection.html)
movementSpatialData <- movementSpatialData %>% spTransform(CRS("+init=epsg:3005"))

mapCentre <- c(sum(movementData$location.long)/nrow(movementData), sum(movementData$location.lat)/nrow(movementData))
# Playing with visualization
mapLayout <- ggmap(get_googlemap(center = c(lon = mapCentre[1], lat = mapCentre[2]),
                         zoom = 6, scale = 2,
                         maptype ='terrain',
                         color = 'bw'))
# zoom only accepts integer; 
# mapLayout <- ggmap(get_map(mapCentre, source = "osm")) # does not work, unfortunately
simpleLayout <- mapLayout + geom_point(aes(x = location.long, y = location.lat, colour = individual.local.identifier), data = movementData, size = 0.5) + theme(legend.position = "bottom") + coord_fixed() #+ geom_path(data = wolfLines, aes(x = long, y = lat, group = group))

print(simpleLayout)

# play around with transparency display
densLayout <- mapLayout + geom_point(aes(x = location.long, y = location.lat), data = movementData, alpha=0.15, size = 0.5, colour = "yellow3") + theme(legend.position="bottom")

# heatMap
heatLayout <- mapLayout + stat_density2d(
  aes(x = location.long, y = location.lat, fill = ..level.., alpha = 0.1),
  size = 0.01, bins = 30, data = movementData,
  geom = "polygon"
)

# convert into line
# wolfLines <- points_to_line(data = movementData, long = "location.long", lat = "location.lat",
#                             id_field = "individual.local.identifier", sort_field = "event.id")
# alpha level (transparency)

# simple centre point calculation as a separate set of points; calculate distance to the centre point
# display centre point with labels or symbols
# heatmap
