library(openxlsx)
library(tidyverse)
library(sf)
library(stringr)

####################################################################
# first perform a spatial join of plot locations into supersections#
####################################################################

# read in the shapefles
supersections <- read_sf("spatial_data", layer = "Supersections_5_1_15")
plot_loc_geom <- read_sf("spatial_data", layer = "plot_loc")

# make supersections have the same crs as plot locations
supersections = st_transform(supersections, 4326)

# join the supersecon to the points
plot_loc_ss <- st_join(plot_loc_geom, supersections, left = FALSE)

#plot id and supersection
plot_id_ss <- plot_loc_ss %>% 
  select(ID, SSection)


###############################################################










