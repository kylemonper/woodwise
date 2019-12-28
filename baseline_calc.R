##### calculating baseline amount of forest carbon #####
###
###
## known harvest amounts in 2010:
## private/tribal : ~1,034,000
## state : ~ 8,000
## federal ~ 265,000
## blm: 553
## total: ~ 1,300,000 thousand board feet
##

library(tidyverse)
library(RODBC)
options(scipen = 999)

##################
## read in data ##
##################

#get harvest cost/ relevant acres
conn <- odbcConnectAccess2007("optimizer_results_cycle_1_MaxMerch_Carbon_Stored_2019-12-02_09-43-16.accdb")
acres <- sqlFetch(conn, "stand_costs_revenue_volume_sum_by_rxpackage", as.is = T)
cost <- sqlFetch(conn, "product_yields_net_rev_costs_summary_by_rx", as.is = T)
odbcCloseAll()

acres_sub <- acres %>% 
  select(biosum_cond_id, owngrpcd, acres)

harv <- cost %>% 
  select(biosum_cond_id, rxpackage, rxpackage, rxcycle, merch_yield_cf)

harv_ownr <- merge(acres_sub, harv) %>% 
  distinct()

private <- harv_ownr %>% 
  filter(owngrpcd == 40) %>% 
  mutate(merch_yield_bf = merch_yield_cf*acres*12)



tot_priv_bf <- data.frame(matrix(NA, nrow = length(unique(private$rxpackage)), ncol = 2))
names(tot_priv_bf) <- c("rxpackage", "harv_bf")
for(i in 1:length(unique(private$rxpackage))){
  
  package <- unique(private$rxpackage)[i]
 
  cc <- filter(private, rxpackage == package)
  
  cc <- filter(cc, rxcycle == 1)
  
  total_private_harv_cf <- sum(cc$merch_yield_bf)
  
  tot_priv_bf$rxpackage[i] <-  package
  tot_priv_bf$harv_bf[i] <- total_private_harv_cf * 12 
  
}


#### next need to choose which plots to pick based on 2012 harvest numbers by county 



