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

###################################################
# now need to join supersection by tree species
##################################################

# read in the files
area<- read_csv("assessment_area_data.csv")
forest_type <- read_csv("plot_loc.csv")

# pair down and convert co2e to carbon
area_need <- area %>% 
  select(Supersection, Associated.Species, 
         cp = `Common.Practice.-.Above.Ground.Carbon.Mean.(Metric.Tonnes.CO2-equivalent)`) %>% 
  mutate(carbon_metric_tons = cp*(12/44))

# check what different tree species we have
forest_look <- forest_type %>% 
  group_by(MEANING) %>% 
  tally()

area_trees <- area_need %>% 
  group_by(Associated.Species) %>% 
  tally()

# combine datafames 
area_forest <- forest_type %>% 
  select(ID, MEANING) %>% 
  left_join(plot_id_ss) %>% 
  left_join(area_need, by = "Supersection") 

# clean the dataset

forest_clean <- area_forest %>% 
  mutate(MEANING = gsub("-", " ", MEANING),
         MEANING = tolower(MEANING)) %>% 
  mutate(Associated.Species = tolower(Associated.Species)) 
 
# add in the fixing of words 
forest_clean$MEANING2 = 0
forest_clean$MEANING2  = ifelse(forest_clean$MEANING == "california white oak (valley oak)",
                       "white oak", forest_clean$MEANING)
forest_clean$MEANING2  = ifelse(forest_clean$MEANING == "cercocarpus (mountain brush) woodland",
                                "cercocarpus (mountain brush)", forest_clean$MEANING2)
forest_clean$MEANING2  = ifelse(forest_clean$MEANING == "canyon live oak",
                                "california live oak", forest_clean$MEANING2)
                  

# mutate new column to say true if words in meaning appear in associated species 
  forest_match <- forest_clean %>% 
    mutate(result=str_detect(MEANING2, gsub(",", "|", Associated.Species))) %>% 
    select(ID, Supersection, MEANING2, Associated.Species, result, 
           carbon_metric_tons, cp)

## true areas
  forest_match_true <- forest_match %>% 
    filter(result == "TRUE") 
## true areas_tally
forest_match_true_tally <- forest_match %>% 
  group_by(MEANING2, result) %>% 
  tally()

## false
forest_match_false <- forest_match %>% 
  filter(result == "FALSE") 

need_to_fix <- as.data.frame(unique(forest_match_false$MEANING2))


# should be 2,289 plots 



###############################################
# kyle
###############################################

# now need to join supersection by tree species
area <- read_csv("assessment_area_data.csv")
Ca_areas <- area %>% 
  filter(Supersection %in% plot_id_ss$SSection & Site.Class %in% c("High", "All")) %>% 
  select(ss = Supersection, aa = Assessment.Area, species = Associated.Species, class = Site.Class, cp = `Common.Practice.-.Above.Ground.Carbon.Mean.(Metric.Tonnes.CO2-equivalent)`) %>% 
  mutate(species = tolower(species),
         cp_mt = cp *(12/44))

forest_type <- read_csv("plot_loc.csv") %>% 
  mutate(ftype = tolower(MEANING)) %>% 
  left_join(plot_id_ss) %>% 
  select(ID, SS = SSection, ftype) %>% 
  mutate(ftype = str_replace(ftype, "-", " "),
         ftype = str_replace(ftype, "coast live oak", "live oak"),
         ftype = str_replace(ftype, "canyon live oak", "live oak"),
         ftype = str_replace(ftype, "oregon live oak", "live oak"))

ftypes <- unique(forest_type$SS)


results <- NULL
for (i in 1:length(ftypes)) {
  sub_type <- forest_type %>% 
    filter(SS == ftypes[i])
  
  sub_area <- Ca_areas %>% 
    filter(ss == ftypes[i])
  
  test <- vector()
  for (j in 1:nrow(sub_type)) {
    test[j] <- sub_area %>% 
      filter(str_detect(species, paste(sub_type$ftype[j]))) %>% 
      select(cp_mt) %>% 
      as.numeric()
    
  }
  
  sub_type$cp <- test
  
  results <- bind_rows(results, sub_type)
  
}







