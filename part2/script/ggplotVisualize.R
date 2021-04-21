# R ggplot adaptation of the GEOB 370 lab
# AD
# 4/21/2021

# 0. Loading libraries======
library(tidyverse)
library(rgdal)
library(raster)
# 0. \ends--------

# 1. Inputs======
# data folder
data_dir <- "part2/data/ggplotVisualize/"
# load occurrence point data
occurrence_point <- shapefile(paste0(data_dir, "occurrences.shp"))
# Define function to automatically check the Coordinate Reference System (CRS) consistency among dataset
# dummy trial to test function
# file.in = paste0(data_dir, "transportation.shp")
# reference = occurrence_point
loadSpatial <- function(file.in = character(), reference = occurrence_point){
  # check the file extension: .tif (raster) or .shp (vector)
  file.isRaster <- grepl(".tif$", file.in)
  if(file.isRaster) file.in <- raster(file.in) else file.in <- shapefile(file.in)
  # apply the appropriate read function accordingly
  # check the CRS consistency with the reference dataset
  CRS_ref <- proj4string(reference)
  CRS_input <- proj4string(file.in)
  if(CRS_ref == CRS_input){
    print("Data successfully imported.. CRS consistency check passed")
  } else print("Data successfully imported.. CRS consistency check failed. Please use with caution or transform")
}
# load road network data
road_line <- loadSpatial(file.in = paste0(data_dir, "transportation.shp"))
# load river network data

# load Digital Elevation Model raster data
# load Slope raster data
# load Aspect raster data
# 1. \ends-------

# 2. Processing=======
# Generate raster stack and extract pixel values
# Visualize
# Check the consistency of the CRS
# 2. \ends-----

# 3. Layouting =======
basicMap <- ggplot() + geom_point(data = occurrence_point, mapping = aes(x=longitude, y=latitude), color="red")
# DATA: bacground, sample_grid
# minor.label
# aggregation_map <- ggplot(background) + geom_raster(aes(fill=as.character(value)), show.legend = FALSE) #+ scale_fill_manual(values = st_cols, breaks = 0)
# # adding the sample_grid
# aggregation_map <- aggregation_map + geom_polygon(data = sample_grid, aes(x = long, y = lat, group = group, fill = NA, colour = "Unit Agregasi"), show.legend = TRUE)
# aggregation_map <- aggregation_map + scale_fill_manual(values = c("1" = "#FFCC66"), breaks = "") +
#   theme(panel.background = element_rect(fill="lightgrey"),
#         legend.key =  element_rect(colour = NA, fill = NA))
# # adding customized x and y axis label
# # defining breaks
# brk_x <- unique(poly.data$X.cor)
# brk_x <- brk_x[order(brk_x)]
# brk_y <- unique(poly.data$Y.cor)
# brk_y <- brk_y[order(brk_y, decreasing = TRUE)]
# 
# # input table
# raw_difa_tab.init <- read.csv(paste0(quesb_folder, "/DIFA_", location, "_", T1, ".csv"), stringsAsFactors = FALSE)
# raw_difa_tab.init <- raw_difa_tab.init[,c("ID.x", "ID.y")]
# raw_difa_tab.final <- read.csv(paste0(quesb_folder, "/DIFA_", location, "_", T2, ".csv"), stringsAsFactors = FALSE)
# raw_difa_tab.final <- raw_difa_tab.final[,c("ID.x", "ID.y")]
# # 
# c_grid <- data.frame(unique(rbind(raw_difa_tab.init, raw_difa_tab.final)), stringsAsFactors = FALSE)
# # application of different text color for column and row ids which is not used
# # x
# x_tab <- data.frame(ID.x = unique(poly.data[order(poly.data$X.cor), "ID.x"]), stringsAsFactors = FALSE)
# x_tab$ID <- seq(nrow(x_tab))
# # x_tab <- data.frame(ID.x = unique(poly.data$ID.x), stringsAsFactors = FALSE)
# # attempt to write the alternative
# x_tab$colour <- grey(0.7)
# x_tab[x_tab$ID.x %in% c_grid$ID.x, "colour"] <- grey(0.2)
# # y
# y_tab <- data.frame(ID.y = unique(poly.data[order(poly.data$Y.cor, decreasing = TRUE), "ID.y"]), stringsAsFactors = FALSE)
# y_tab$ID <- seq(nrow(y_tab))
# # attempt to write the alternative
# y_tab$colour <- grey(0.7)
# y_tab[y_tab$ID.y %in% c_grid$ID.y, "colour"] <- grey(0.2)
# # apply the customized breaks and labels
# aggregation_map <- aggregation_map + scale_x_continuous(breaks = brk_x, labels = x_tab$ID.x) + scale_y_continuous(breaks = brk_y, labels = y_tab$ID.y)
# # apply text colour
# aggregation_map <- aggregation_map + theme(axis.text.x = element_text(color = x_tab$colour, size = 5), axis.text.y = element_text(color = y_tab$colour, size = 6.5),
#                                            axis.ticks.x = element_line(color = x_tab$colour), axis.ticks.y = element_line(color = y_tab$colour))
# # aggregation_map <- aggregation_map + geom_text(data = c_grid, aes(x = X.cor, y = Y.cor, label = ID.grid), size = 2)
# aggregation_map <- aggregation_map + guides(colour = guide_legend(override.aes = list(fill = NA)))
# aggregation_map <- aggregation_map + labs(colour = "Legenda") + theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
#                                                                        legend.title = element_text(face = "bold"),
#                                                                        legend.key.size = unit(0.5, "cm"),
#                                                                        plot.margin=unit(c(0,0,0,0), "cm"),
#                                                                        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
#                                                                        legend.key.width = unit(0.6, "cm")) + coord_equal()
# return(aggregation_map)
# 3. \ends-----