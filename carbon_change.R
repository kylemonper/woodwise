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

conn <- odbcConnectAccess2007("PREPOST_FVS_POTFIRE.ACCDB")
pre_fire <- sqlFetch(conn, "PRE_FVS_POTFIRE", as.is = T)
post_fire <- sqlFetch(conn, "POST_FVS_POTFIRE", as.is = T)
odbcCloseAll()




conn <- odbcConnectAccess2007("PREPOST_FVS_CARBON.ACCDB")
pre_carb <- sqlFetch(conn, "PRE_FVS_CARBON", as.is = T)
post_carb <- sqlFetch(conn, "POST_FVS_CARBON", as.is = T)
odbcCloseAll()



conn <- odbcConnectAccess2007("PREPOST_FVS_HRV_CARBON.ACCDB")
pre_carb_hrv <- sqlFetch(conn, "PRE_FVS_HRV_CARBON", as.is = T)
post_carb_hrv <- sqlFetch(conn, "POST_FVS_HRV_CARBON", as.is = T)
odbcCloseAll()


conn <- odbcConnectAccess2007("optimizer_results.accdb")
cost <- sqlFetch(conn, "product_yields_net_rev_costs_summary_by_rxpackage", as.is = TRUE)
opt_plot_rx <- sqlFetch(conn, "all_cycles_best_rx_summary", as.is = T)
odbcCloseAll()


####################
## select columns ##
####################

## optimized packages + harvest cost per acre
cost_sel <- cost %>% select(biosum_cond_id, rxpackage, harvest_onsite_cpa)
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
##~~~~~~ need to sum total removed for all cycles ~~~~~###
pre_carbon_tot$tot_all <- pre_carbon_tot$Total_Stand_Carbon + pre_carbon_tot$Merch_Carbon_Removed
post_carbon_tot$tot_all <- post_carbon_tot$Total_Stand_Carbon + post_carbon_tot$Merch_Carbon_Removed


### join to econ
pre_full <- left_join(cost_sel, pre_carbon_tot)
post_full <- left_join(cost_sel, post_carbon_tot)


### filter by optimal
pre_opt <- left_join(opt_plot_rx, pre_full)
post_opt <- left_join(opt_plot_rx, post_full)

############################
##### calculate change #####
############################


### now find the total carbon that was stored/sequesterd by subtracting total_carb at in pre_rx 1 and post_rx 4
pre_rx1 <- pre_opt %>% 
  filter(rxcycle == 1) %>% 
  select(biosum_cond_id, rxcycle, tot_all)


post_rx4 <- post_opt %>% 
  filter(rxcycle == 4) %>% 
  select(biosum_cond_id, rxcycle, tot_all) 



pre_post <- rbind(pre_rx1, post_rx4)

wide <- pre_post %>% 
  spread(rxcycle, tot_all) %>% 
  mutate(change = `4`  - `1`)

# join in total cost per stand


all_data <- left_join(wide, post_opt[, c("biosum_cond_id","harvest_onsite_cpa")]) %>% 
  distinct() 

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


cumm <- all_data %>%  
  mutate(CPU = harvest_onsite_cpa/change) %>% 
  filter(CPU >= 0 & CPU < 1000) %>%  ### this gets rid of sites where carbon is lost overtime and sites with very low change and therefor very high CPU
  arrange(CPU) %>% 
  mutate(cumsum = cumsum(change))
  

tmp <- post_carb %>% 
  select(biosum_cond_id, rx, rxcycle)

ggplot(cumm, aes(x = cumsum, y = CPU)) + 
  geom_point() +
  labs(
    x = "total carbon stored",
    y = "$/ton C") +
  theme_bw()



## add in forest type and variant
