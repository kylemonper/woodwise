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
cost <- sqlFetch(conn, "product_yields_net_rev_costs_summary_by_rx", as.is = T)
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
  select(biosum_cond_id, rxpackage, rxcycle, complete_cpa) %>% 
  arrange(biosum_cond_id, rxpackage, rxcycle)
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
pre_full <- left_join(pre_carbon_tot, cost_sel)  %>% 
  distinct()
post_full <- left_join(post_carbon_tot, cost_sel)  %>% 
  distinct()


##### write to csv
write_csv(pre_full, "pre_harv_full.csv")
write_csv(post_full, "post_harv_full.csv")

pre_full <- read_csv("pre_harv_full.csv")
post_full <- read_csv("post_harv_full.csv")

####################
#carbon discounting
####################

# test 1

# filter down to just one plot
plot_pre <- pre_full %>% 
  filter(biosum_cond_id==1200506050501500909686420) %>% 
  filter(rxpackage=="001") %>% 
  filter(complete_cpa!=0) %>% # need to change this
  select(-tot_all) %>% 
  mutate(time = rxcycle)

# make a loop to match cycle to year

for (i in length(plot_pre$time)){
  
  plot_pre$time = ifelse(plot_pre$time==1, 0, plot_pre$time) 
  plot_pre$time = ifelse(plot_pre$time==2, 10, plot_pre$time) 
  plot_pre$time = ifelse(plot_pre$time==3, 20, plot_pre$time) 
  plot_pre$time = ifelse(plot_pre$time==4, 30, plot_pre$time) 
  
}

#### for post
plot_post <- post_full %>% 
  filter(biosum_cond_id==1200506050501500909686420) %>% 
  filter(rxpackage=="001") %>% 
  filter(complete_cpa!=0) %>% ## need to change this
  select(-tot_all) %>% 
  mutate(time = rxcycle) 

# make a loop to match cycle to year
for (i in length(plot_post$time)){
  
  plot_post$time = ifelse(plot_post$time==1, 1, plot_post$time) 
  plot_post$time = ifelse(plot_post$time==2, 11, plot_post$time) 
  plot_post$time = ifelse(plot_post$time==3, 21, plot_post$time) 
  plot_post$time = ifelse(plot_post$time==4, 31, plot_post$time) 
  
}

#stack pre and post on top of each other
pre_post <- rbind(plot_pre, plot_post)

#add columns that show to each year carbon increments
# ** we want to look at from cycle 1--> 10, 11 --> 20, 21--> 30 so ignore the #
# changes from 0-->1 etc bc that's merchantable **** #

pre_post <- pre_post %>% 
  # make sure in the right order
  arrange(time) %>% 
  # find the difference between the the stand carbon in cycle 1 and 2, 2 and 3 etc
  mutate(diff = Total_Stand_Carbon - lag(Total_Stand_Carbon, default = first(Total_Stand_Carbon))) %>%
  # what happens each year
  mutate(each_year = diff/10)

# now create a new dataframe with all of the times and make sure time is an integer
plot_all <- tibble(time = 0:31)

# now merge together the dataframes
plot_all <- left_join(plot_all, pre_post, by = "time") 

###
# now need to fill in the empty total stand carbon columns 
for (i in 1:nrow(plot_all)){
  plot_all$each_year = ifelse(plot_all$time <=9 & plot_all$time >=2, 
                              plot_all$each_year[11],
                              plot_all$each_year)
  plot_all$each_year = ifelse(plot_all$time <=19 & plot_all$time >=12, 
                              plot_all$each_year[21],
                              plot_all$each_year)
  plot_all$each_year = ifelse(plot_all$time <=29 & plot_all$time >=22, 
                              plot_all$each_year[31],
                              plot_all$each_year)
}

## add a discounted column that is discounted by 0.05 
# that is the cumulative sum for each year of discounted carbon

plot_all <- plot_all %>% 
  mutate(discount_carb = each_year/((1+0.05)^time)) %>% 
  mutate(cum_discount_carb = cumsum(discount_carb))











