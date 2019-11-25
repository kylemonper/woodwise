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

conn <- odbcConnectAccess2007("scenario_results_allrx.mdb")
cost <- sqlFetch(conn, "harvest_costs", as.is = T)
odbcCloseAll()

conn <- odbcConnectAccess2007("optimizer_results_all_cycles_MaxTotal_Stand_CarbonNRgtP0_2019-11-03_10-20-26 (1).accdb")
acres <- sqlFetch(conn, "all_cycles_best_rx_summary", as.is = T)
odbcCloseAll()



conn <- odbcConnectAccess2007("PREPOST_FVS_CARBON.ACCDB")
pre_carb <- sqlFetch(conn, "PRE_FVS_CARBON", as.is = T)
post_carb <- sqlFetch(conn, "POST_FVS_CARBON", as.is = T)
odbcCloseAll()



conn <- odbcConnectAccess2007("PREPOST_FVS_HRV_CARBON.ACCDB")
pre_carb_hrv <- sqlFetch(conn, "PRE_FVS_HRV_CARBON", as.is = T)
post_carb_hrv <- sqlFetch(conn, "POST_FVS_HRV_CARBON", as.is = T)
odbcCloseAll()


####################
## select columns ##
####################

## optimized packages + harvest cost per acre
cost_sel <- cost %>% select(biosum_cond_id, rxpackage, complete_cpa)
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
pre_carbon_tot <- left_join(pre_carb_sel, pre_hrv_sel)  %>% 
  distinct()
post_carbon_tot <- left_join(post_carb_sel, post_hrv_sel)  %>% 
  distinct()

## sum stand and harvest carbon to get total
pre_carbon_tot$tot_all <- pre_carbon_tot$Total_Stand_Carbon 
post_carbon_tot$tot_all <- post_carbon_tot$Total_Stand_Carbon


### join to econ
pre_full <- left_join(cost_sel, pre_carbon_tot)  %>% 
  distinct()
post_full <- left_join(cost_sel, post_carbon_tot)  %>% 
  distinct()

####################
#carbon discounting
####################

# test 1

















