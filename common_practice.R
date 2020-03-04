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
  select(ID, SSection) %>% 
  rename(Supersection = SSection)


###############################################################
<<<<<<< HEAD







=======
# now need to join supersection by tree species
##################################################
# read in the files
area<- read_csv("assessment_area_data.csv")
forest_type <- read_csv("plot_loc.csv")

# pair down and convert co2e to carbon
area_need <- area %>% 
  select(Supersection, Associated.Species, `Common.Practice.-.Above.Ground.Carbon.Mean.(Metric.Tonnes.CO2-equivalent)`) %>% 
  mutate(carbon_metric_tons =
           `Common.Practice.-.Above.Ground.Carbon.Mean.(Metric.Tonnes.CO2-equivalent)`*(12/44))

# check what different tree species we have
forest_look <- forest_type %>% 
  group_by(MEANING) %>% 
  tally()

area_trees <- area_need %>% 
  group_by(Associated.Species) %>% 
  tally()

# combine datafames and mutate new column to say true if words in meaning appear in associated species
area_forest <- forest_type %>% 
  select(ID, MEANING) %>% 
  left_join(plot_id_ss) %>% 
  left_join(area_need, by = "Supersection") %>% 
  mutate(result=str_detect(Associated.Species, gsub(" ", "|", MEANING)))
>>>>>>> 3becc28f317b2b889a52f4c7b8d0ca1974cc7f26



