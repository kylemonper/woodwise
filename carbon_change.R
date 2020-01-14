#################################################
### This code reads in relevent pre and post
### tables along with the optimization selections
### for calculating change in carbon sequestration 
### over the course of the 40 year treatments

library(tidyverse)
library(RODBC)

## ensure that plot ids are read in as entire number (vs being auto-converted to scientific notation)
options(scipen = 999)

##################
## read in data ##
##################

#get harvest cost/ relevant acres
conn <- odbcConnectAccess2007("optimizer_results_cycle_1_MaxMerch_Carbon_Stored_2019-12-02_09-43-16.accdb")
acres <- sqlFetch(conn, "stand_costs_revenue_volume_sum_by_rxpackage", as.is = T)
cost <- sqlFetch(conn, "product_yields_net_rev_costs_summary_by_rxpackage", as.is = T)
odbcCloseAll()



#carbon data
conn <- odbcConnectAccess2007("PREPOST_FVS_CARBON.ACCDB")
pre_carb <- sqlFetch(conn, "PRE_FVS_CARBON", as.is = T)
post_carb <- sqlFetch(conn, "POST_FVS_CARBON", as.is = T)
odbcCloseAll()


# harvested carbon data
conn <- odbcConnectAccess2007("PREPOST_FVS_HRV_CARBON.ACCDB")
pre_carb_hrv <- sqlFetch(conn, "PRE_FVS_HRV_CARBON", as.is = T)
post_carb_hrv <- sqlFetch(conn, "POST_FVS_HRV_CARBON", as.is = T)
odbcCloseAll()




####################
## select columns ##
####################

## optimized packages + harvest cost per acre
cost_sel <- cost %>% 
  mutate(complete_cpa = harvest_onsite_cpa + haul_chip_cpa + haul_merch_cpa) %>% 
  select(biosum_cond_id, rxpackage, complete_cpa)
# by only selective harvest_cpa we are explicity ignoring transportation

## stand carbon
pre_carb_sel <- pre_carb %>% 
  select(biosum_cond_id, rxpackage, rxcycle, Total_Stand_Carbon)

post_carb_sel <- post_carb %>% 
  select(biosum_cond_id, rxpackage, rxcycle, Total_Stand_Carbon)


## harvest carbon
pre_hrv_sel <- pre_carb_hrv %>% 
  select(biosum_cond_id, rxpackage, rxcycle, Merch_Carbon_Removed)

post_hrv_sel <- post_carb_hrv %>% 
  select(biosum_cond_id, rxpackage, rxcycle, Merch_Carbon_Removed)



## join carbon tables
pre_carbon_tot <- left_join(pre_carb_sel, pre_hrv_sel)
post_carbon_tot <- left_join(post_carb_sel, post_hrv_sel)

## sum stand and harvest carbon to get total
pre_carbon_tot$tot_all <- pre_carbon_tot$Total_Stand_Carbon 
post_carbon_tot$tot_all <- post_carbon_tot$Total_Stand_Carbon


# join in acreage
pre_carbon_tot <- left_join(acres[,c("biosum_cond_id", "acres")], pre_carbon_tot)
post_carbon_tot <- left_join(acres[,c("biosum_cond_id", "acres")], post_carbon_tot)

### join to econ
pre_full <- left_join(cost_sel, pre_carbon_tot)  %>% 
  distinct()
post_full <- left_join(cost_sel, post_carbon_tot)  %>% 
  distinct()




############################
##### calculate change #####
############################


### now find the total carbon that was stored/sequesterd by subtracting 
# total_carb at in pre_rx 1 and post_rx 4
pre_rx1 <- pre_full %>% 
  filter(rxcycle == 1) %>% 
  select(biosum_cond_id, rxpackage, rxcycle, tot_all)


post_rx4 <- post_full %>% 
  filter(rxcycle == 4) %>% 
  select(biosum_cond_id, rxpackage, rxcycle, tot_all) 



pre_post <- rbind(pre_rx1, post_rx4) %>% 
  distinct()

# check to see that there are only two of each
# should only be 2
tmp <- pre_post %>% 
  group_by(biosum_cond_id, rxpackage) %>% 
  tally()
unique(tmp$n)

wide <- pre_post %>% 
  tidyr::spread(rxcycle, tot_all) %>% 
  mutate(change = `4`  - `1`)


### add in changes in total removed carbon
removed <- post_hrv_sel %>% 
  group_by(biosum_cond_id, rxpackage) %>% 
  summarise(total_removed = sum(Merch_Carbon_Removed))


all_change <- left_join(wide, removed)

all_change$total_change <- all_change$change + all_change$total_removed

total_cost <- cost %>% 
  group_by(biosum_cond_id, rxpackage) %>% 
  summarise(total_cost = sum(complete_cpa))

#### grow only scenario
baseline <- all_change %>% 
  filter(rxpackage == "032") %>% 
  select(biosum_cond_id, change) %>% 
  rename("base_change" = "change")

### join together all w/ baseline and subtract
all_base <- left_join(all_change, baseline) 


change_relative <- all_base %>% 
  mutate(change_rel = total_change-base_change)
# join in total cost per stand


all_data <- left_join(change_relative, total_cost) %>% 
  distinct() 

all_data_acre <- left_join(all_data, acres[,c("biosum_cond_id","acres")])


##### TEMPORARY!!!!! #######
# ~~ return acre NA's with average acreage
all_data_acre$acres[is.na(all_data_acre$acres)] <- mean(all_data_acre$acres, na.rm = T)


total_carbon_acre <- all_data_acre %>% 
  mutate(change_acre = total_change*acres,
         cost_acre = total_cost*acres,
         cpu = cost_acre/change_acre)


##### optimize
## for each plot, which package has the lowest cpu
opt <- total_carbon_acre %>% 
  select(biosum_cond_id, rxpackage, cpu) %>% 
  group_by(biosum_cond_id) %>% 
  filter(cpu == min(cpu))

opt_change <- left_join(opt, total_carbon_acre[,c("biosum_cond_id", "rxpackage","change_acre")]) %>% 
  distinct()

uniq_plot <- opt_change %>% 
  select(biosum_cond_id, cpu, change_acre) %>% 
  distinct()

####investigate number of plots later#####
tmp <- uniq_plot %>% 
  group_by(biosum_cond_id) %>% 
  tally()

#####################################
#### get coordinates for mapping  ###
#####################################

# conn <- odbcConnectAccess2007("master.mdb")
# plot_m <- sqlFetch(conn, "plot", as.is = T)
# cond_m <- sqlFetch(conn, "cond", as.is = T)
# odbcCloseAll()
# 
# 
# plot_sel <- plot_m %>% 
#   select(biosum_plot_id, lat, lon)
# 
# cond_sel <- cond_m %>% 
#   select(biosum_cond_id, biosum_plot_id)
# 
# cond_lat_lon <- left_join(cond_sel, plot_sel)
# 
# change_lat_long <- left_join(cond_lat_lon, all_data)
# 
# final <- change_lat_long %>% 
#   filter(!is.na(change))
# 
# write_csv(final, "change_coords.csv")

#################################
##### marginal cost curve  ######
#################################

###~~~~~~~~~~~~~~~~~~~~~
### need to add in acres to get total carbon 
### right now this calculation is showing per plot??


cumm <- uniq_plot %>%  
  filter(cpu >= 0 & cpu < 1000) %>%  ### this gets rid of sites where carbon is lost overtime and sites with very low change and therefor very high CPU
  arrange(cpu)

cumm$cummulative <- cumsum(cumm$change_acre) 

cumm <- cumm %>% 
  mutate(mega_tons_c = cummulative/1000000)


## optimal marginal cost curve
ggplot(cumm, aes(x = mega_tons_c, y = cpu)) + 
 # geom_point() +
  geom_line(size = 2) +
  scale_x_continuous(expand = c(0,0), limits = c(0,1000)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,200)) +
  labs(
    x = "Megatons C Stored",
    y = "$/ton C") +
  theme_bw()

## Which packages are the most common?
opt_count <- opt %>% 
  group_by(rxpackage) %>% 
  tally() %>% 
  arrange(-n) %>% 
  head(10)

####make a graph of 5 top package RX curves###

# make the process of cleaning the data a function where you can say what package you want to look at 
##use the total_carbon_acre data frame

make_package_mc = function(df, package = "") {
   
  opt_packages <- df %>% 
    select(biosum_cond_id, rxpackage, cpu) %>% 
    group_by(biosum_cond_id) %>% 
    filter(rxpackage == package)
  
  opt_packages_change <- left_join(opt_packages,
                                   total_carbon_acre[,c("biosum_cond_id", "rxpackage","change_acre")]) %>% 
    distinct()
  
  cumm_package <- opt_packages_change %>%  
    filter(cpu >= 0 & cpu < 1000) %>%  ### this gets rid of sites where carbon is lost overtime and sites with very low change and therefor very high CPU
    arrange(cpu)
  
  cumm_package$cummulative <- cumsum(cumm_package$change_acre) 
  
  cumm_package <- cumm_package %>% 
    mutate(mega_tons_c = cummulative/1000000)
  
  return(cumm_package)
}

# order of most common package from 1-5  
mc_p018 <- make_package_mc(total_carbon_acre, package = "018")
mc_p014 <- make_package_mc(total_carbon_acre, package = "014")
mc_p007 <- make_package_mc(total_carbon_acre, package = "007")
mc_p006 <- make_package_mc(total_carbon_acre, package = "006")
mc_p004 <- make_package_mc(total_carbon_acre, package = "004")

# puts top 5 packages into 1 dataframe
top_5_packages_all <- bind_rows(mc_p018, mc_p014, mc_p007, mc_p006, mc_p004)
  

# plot the different marginal cost curves w/ optimal
ggplot(top_5_packages_all, aes(x = mega_tons_c, y = cpu)) + 
  geom_line(aes(color = rxpackage)) +
  geom_line(data = cumm, aes(x = mega_tons_c, y = cpu), color = "black", size = 1.5) +
  scale_x_continuous(expand = c(0,0), limits = c(0,1000)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,200)) +
  labs(
    x = "Megatons C Stored",
    y = "$/ton C",
    color = "Prescription Package") +
  theme_bw()


# boxplots of total change for each package
ggplot(total_carbon_acre, aes(x = rxpackage, y = change_acre)) +
  geom_boxplot()


## look at the difference in potential based on ownership

conn <- odbcConnectAccess2007("master.mdb")
cond <- sqlFetch(conn, "cond", as.is = T)
odbcCloseAll()

# try with making it all into one dataframe


## add in forest type and variant