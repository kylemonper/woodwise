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
  filter(Site.Class == "High" | Site.Class == "All") %>% 
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
         MEANING = tolower(MEANING),
         MEANING = gsub("/", "|", MEANING)) %>% 
  mutate(Associated.Species = tolower(Associated.Species)) 

# fixing species wording to match
forest_clean$MEANING2 = 0
forest_clean$MEANING2  = ifelse(forest_clean$MEANING == 
                                "california white oak (valley oak)",
                                "white oak", forest_clean$MEANING)
forest_clean$MEANING2  = ifelse(forest_clean$MEANING == 
                                "cercocarpus (mountain brush) woodland",
                                "cercocarpus", forest_clean$MEANING2)
forest_clean$MEANING2  = ifelse(forest_clean$MEANING == 
                                "canyon live oak",
                                "live oak", forest_clean$MEANING2)
forest_clean$MEANING2  = ifelse(forest_clean$MEANING == 
                                "oregon live oak",
                                "live oak", forest_clean$MEANING2)
forest_clean$MEANING2  = ifelse(forest_clean$MEANING == 
                                "coast live oak",
                                "live oak", forest_clean$MEANING2)
forest_clean$MEANING2  = ifelse(forest_clean$MEANING == 
                                "interior live oak",
                                "live oak", forest_clean$MEANING2)
forest_clean$MEANING2  = ifelse(forest_clean$MEANING == 
                                "miscellaneous western softwoods",
                                "douglas fir", forest_clean$MEANING2)
forest_clean$MEANING2  = ifelse(forest_clean$MEANING == 
                                "pinyon | juniper woodland",
                                "pinyon", forest_clean$MEANING2)
forest_clean$MEANING2  = ifelse(forest_clean$MEANING == 
                                "monterey pine",
                                "pine", forest_clean$MEANING2)
forest_clean$MEANING2  = ifelse(forest_clean$MEANING == 
                                "other hardwoods",
                                "white fir", forest_clean$MEANING2)
forest_clean$MEANING2  = ifelse(forest_clean$MEANING == 
                                "juniper woodland",
                                "juniper", forest_clean$MEANING2)


# mutate new column to say true if words in meaning appear in associated species 
forest_match <- forest_clean %>% 
  mutate(result=str_detect(Associated.Species, MEANING2)) %>% 
  select(ID, Supersection, MEANING2, Associated.Species, result, 
         carbon_metric_tons, cp)


######################
# Analyze the output #
#####################

## true areas
forest_match_true <- forest_match %>% 
  filter(result == "TRUE") 

## true areas_tally
forest_match_true_tally <- forest_match %>% 
  group_by(MEANING2, result) %>% 
  tally()

## FAlse areas tally
forest_false <- forest_match_true_tally %>% 
  filter(result == "FALSE")

# plots not chosen 
plots <- forest_match %>% 
  group_by(ID, result) %>% 
  tally() %>% 
  filter(result == "TRUE")

### missing plots
# assign missing plots to the highest common practice to be conservative
plots_missing <- as.data.frame(setdiff(1:2289, plots$ID)) %>% 
  rename(ID = "setdiff(1:2289, plots$ID)") %>% 
  mutate(species = "no_species") %>% 
  left_join(forest_match, by = "ID") %>% 
  select(ID, Supersection, carbon_metric_tons) %>% 
  group_by(ID) %>% 
  filter(carbon_metric_tons == max(carbon_metric_tons))

## final dataframe to feed back into our model

carb_base <- forest_match_true %>% 
  select(ID, Supersection, carbon_metric_tons) %>% 
  bind_rows(plots_missing) %>% 
  arrange(ID) %>% 
  group_by(ID) %>% 
  filter(carbon_metric_tons == max(carbon_metric_tons))

ids_missing <- setdiff(1:2289, carb_base$ID)
  

######################
# Kyle version #
#####################

# now need to join supersection by tree species
area <- read_csv("assessment_area_data.csv")
Ca_areas <- area %>% 
  filter(Supersection %in% plot_id_ss$Supersection & Site.Class %in% c("High", "All")) %>% 
  select(ss = Supersection, aa = Assessment.Area, species = Associated.Species, class = Site.Class, cp = `Common.Practice.-.Above.Ground.Carbon.Mean.(Metric.Tonnes.CO2-equivalent)`) %>% 
  mutate(species = tolower(species),
         cp_mt = cp *(12/44))

forest_type <- read_csv("plot_loc.csv") %>% 
  mutate(ftype = tolower(MEANING)) %>% 
  left_join(plot_id_ss) %>% 
  select(ID, SS = Supersection, ftype) %>% 
  mutate(ftype = str_replace(ftype, "-", " "),
         ftype = str_replace(ftype, "/", "|"),
         ftype = str_replace(ftype, "coast live oak", "live oak"),
         ftype = str_replace(ftype, "canyon live oak", "live oak"),
         ftype = str_replace(ftype, "oregon live oak", "live oak"),
         ftype = str_replace(ftype, "interior live oak", "live oak"))

ftypes <- unique(forest_type$SS)


results <- NULL
for (i in 1:length(ftypes)) {
  sub_type <- forest_type %>% 
    filter(SS == ftypes[i])
  
  sub_area <- Ca_areas %>% 
    filter(ss == ftypes[i])
  
  test <- vector()
  for (j in 1:nrow(sub_type)) {
    test <- sub_area %>% 
      filter(str_detect(species, paste(sub_type$ftype[j]))) %>% 
      select(cp_mt) %>% 
      as.numeric()
    
  }
  
  sub_type$cp_mt <- test
  
  results <- bind_rows(results, sub_type)
  
}


NA_results <- results %>% 
  filter(is.na(cp_mt))

na_counts <- NA_results %>% 
  group_by(ftype) %>% 
  tally()






