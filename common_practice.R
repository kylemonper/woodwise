library(openxlsx)
library(tidyverse)
library(sf)

####################################################################
# first perform a spatial join of plot locations into supersections#
####################################################################

# read in the shapefles
supersections_sub <- read_sf("spatial_data", layer = "supersections_subset_b")
plot_loc_geom <- read_sf("spatial_data", layer = "plot_loc")

# make supersections have the same crs as plot locations
st_crs(supersections_sub) = 4326

plot(supersections_sub)
plot(plot_loc_geom)


# join the supersecon to the points
plot_loc_ss <- st_join(plot_loc_geom, supersections_sub)

area<- read_csv("assessment_area_data.csv")

forest_type <- read_csv("plot_loc.csv")
