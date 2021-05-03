
# First things first: 
# Have you downloaded the dataset?
# Where is it stored?
# Where is this script?

getwd()       # this tells you where you are
list.files()  # this lets you see the files inside the folder where you are

# Now, load your packages
# the order in which you load them can affect some functions with the same name from other packages
# Don't loose your head over this. Usually when a function that you always use stops working is because 
# a function is 'masked' by a function with the same name from another package that was loaded after.
# Thi can be fixed by wirtting <package.name>::<function> and run as always

# library (bcmaps)-> we won't use this one today but if you have data in BC I recommend you chek this one out at: https://github.com/bcgov/bcmaps

library (ggmap)
library (lubridate)
library (ggplot2)
library (data.table)
library (ggrepel)
library (dplyr)
library (tidyverse)
library(rgdal)
library(raster)

# To use google maps we are akes for a key. Here is one:]
register_google(key = "AIzaSyDvpJHmU1qbEdJLqgF-PwgxKcPf0BvtThE")

####### FUNCTIONS #######
# Let's do some functions
   points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
     
     # Convert to SpatialPointsDataFrame
     coordinates(data) <- c(long, lat)
     
     # If there is a sort field...
     if (!is.null(sort_field)) {
       if (!is.null(id_field)) {
         data <- data[order(data[[id_field]], data[[sort_field]]), ]
       } else {
         data <- data[order(data[[sort_field]]), ]
       }
     }
     
     # If there is only one path...
     if (is.null(id_field)) {
       
       lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
       
       return(lines)
       
       # Now, if we have multiple lines...
     } else if (!is.null(id_field)) {  
       
       # Split into a list by ID field
       paths <- sp::split(data, data[[id_field]])
       
       sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
       
       # I like for loops, what can I say...
       for (p in 2:length(paths)) {
         id <- paste0("line", as.character(p))
         l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
         sp_lines <- spRbind(sp_lines, l)
       }
       
       return(sp_lines)
     }
   }


 
###### LOAD DATA #####
# Now, load your data:
movementData <- read.csv ("/Users/alix/Downloads/Alberta-BC Wolves.csv") # yes, you have to change this to the location of your file

# Always inspect your data
movementData %>% head()

dim (movementData)   # what is this telling you?
str (movementData) 
summary (movementData)
# What is(are) the coordinate system(s) in this data set?

### ADD MONTH COLUMN #####
# Let's add a fake column of month of observation (you will see later what this is for)
month <- sample(c("januray","may", "june"), 174443, replace=TRUE, prob = c(0.03, 0.57, 0.4))
movementData$month<- month
# Did this work?
# Are you sure?
# Let's make sure it is a factor
movementData$month<-as.factor(movementData$month)
# Maybe you can add another fake column with some other categorical variable and play around with facet_wrap(~)


######## SUBSET ##########
# The data set is so big!
# But you can always subset...
# To select a random subset of 100 samples:

random_100<-movementData[sample(nrow(movementData), 100), ]
  
 # Or since a same wolf (you can know by the tag identifier) was observed several times
 # we could group observations by individual:
 
 each_wolf<-movementData %>%                        # this is the beauty of dplyr
  group_by(tag.local.identifier) %>%          # this is grouping data by tag
  summarise(mean_long=mean(location.long),    # with this we can calculate the mean coordinates for each individual
    mean_lat=mean(location.lat),
   count=n())                                 # as well as the number of times that each tag is repeated in the data set (n=number of observations)
   
as.data.frame(each_wolf)

# Now you have two subsets from the same data. Inspect both data subsets. What are the differences?

############################################
# HOW TO MAKE A VERY SIMPLE MAP WITH ggmap:
# This does not require google's Key
# First, you need to know your maximum coordinates, which you canobtain apllying the functions mean() and max() to your coordinate columns
# What are the extreme coordinates?

p<- get_map(location = c(-122, 50, -114,  56), source = "osm") # you can use add different zoom values after source, like zoom = 10
ggmap(p)+ 
  geom_point (data=random_100, aes(x=location.long, y=location.lat), size=0.05) 
  
# Change the location of "size=" inside the aes parenthesis and see how that affects the size of your points
# Change the "source" and "zoom" of the get_map function and see what happens
# Use different data subsets.
# this is in the style of ggplot so it's easy to play around and change style like labels, text and colour.
# and there are many resources on the internet, and there are color sets for color blind people.

# What if we want to locate the regions with more observations?
# One way to do that is looking for density of ALL observations
 
 ggmap(p) + 
  stat_density2d(data=movementData, 
                 aes(x = location.long, y = location.lat, fill = ..level.., alpha = 0.25), geom = "polygon")+ # geom can have different kinds like geom = "density2d"
  geom_point(data=movementData,  # you can try this to look only for individual B042: [which(movementData$individual.local.identifier=="B042"),]
             aes(x = location.long, y = location.lat, stroke = 2), size =0.05, color="darkred") +
             theme(legend.position = "none")
             
# Now try that with the data set that is grouped by tag

# What would you do if you wanted that each point reflected the number of observations per tag?
# hint: change size of geom_point
# in the data set grouped by tag that is represented in the column count
# What happens if you want each tag to be represented by a different colour?  

  
# If you wanna focus on a specific region in the map you can just change your coordinates
p_zoom<- get_map(location = c(-121, 53, -117,  55), source = "osm") 
ggmap(p_zoom) + 
  stat_density2d(data=each_wolf, 
                 aes(x = mean_long, y = mean_lat, fill = ..level.., alpha = 0.25), geom = "polygon")+ #geom = "density2d"
  geom_point(data=each_wolf, 
             aes(x = mean_long, y = mean_lat, stroke = 2,color=factor(tag.local.identifier), size=count))
 
# Now try with theme(legend.position = "none")

# Let's add a fake column with month data
month <- sample(c("january","may", "june"), 174443, replace=TRUE, prob = c(0.15, 0.57, 0.28))
wolves$month<- month
# Did this work?
# Are you sure? 
# Ok, let's make sure it is a factor
wolves$month<-as.factor(wolves$month)
# Now, let's get 1,000 random points

thousand_wolves<-movementData[sample(nrow(movementData), 1000), ] 
str(thousand_wolves)

# Arrange by tag
each_wolf_month<-thousand_wolves %>%
  group_by(tag.local.identifier) %>%
  summarise(mean_long=mean(location.long),
            mean_lat=mean(location.lat),
            count=n(),
            january= sum (month=='january'),
            may= sum (month=='may'),
            june= sum (month=='june'))
as.data.frame(each_wolf_month) 


# Let's plot pie chart
ggmap(p) + 
scale_fill_manual(
  breaks = c("january", "may", "june"),
  labels = c("january", "may", "june"),
  values = c("january" = "black",
             "may" = "#FC8D62",
             "june" = "#E7298A") )+
  geom_scatterpie(data = each_wolf_month,
                  aes(mean_long, mean_lat,r= sqrt(count)/20 ), # r= sqrt(count)/200 #group=tag.local.identifier
                  cols = c("january", "may", "june" ), 
                  color=NA,
                  alpha = 0.7) +
   theme(legend.position = "right")

# You can play around with changing the number of wolves, the zoom of the map, the aesthetics of it, etc...

# Another way to visualize this same data set is by making the
# each_wolf_month subset into long format
each_wolf_month_long<- each_wolf_month%>%
  gather(month, count, january:june)

# plot
ggmap(p)+ 
  geom_point (data=each_wolf_month_long, aes(x=mean_long, y=mean_lat), size=0.05)+
  facet_wrap(~month)


#### A MORE PROFESSIONAL WAY OF DOING IT#####
# Let's use our functions
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

