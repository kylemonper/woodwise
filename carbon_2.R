#################################################
### This code reads in relevent pre and post
### tables along with the optimization selections
### for calculating change in carbon sequestration 
### over the course of the 40 year treatments
# 

#### next steps: ####
##~    - think about products
##~      -- merchantble (smith paper/ http://maps.gis.usu.edu/HWP/Home/About ?)
##~       - non-merch: Bodies numbers ()
##~    - subtract baseline
##~      -- HOW??
##~    - vary discount rates (read SCC article: https://www.carbonbrief.org/qa-social-cost-carbon)
##~    - clean workflow


### library ####

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

#### get lat long data
conn <- odbcConnectAccess2007("master.mdb")
plot_m <- sqlFetch(conn, "plot", as.is = T)
cond_m <- sqlFetch(conn, "cond", as.is = T)
odbcCloseAll()


plot_sel <- plot_m %>%
  select(biosum_plot_id, lat, lon)

cond_sel <- cond_m %>%
  select(biosum_cond_id, biosum_plot_id)

cond_lat_lon <- left_join(cond_sel, plot_sel)




#########################
## select columns/join ##
#########################

## optimized packages + harvest cost per acre
cost_sel <- cost %>%
  mutate(complete_cpa = harvest_onsite_cpa + haul_chip_cpa + haul_merch_cpa) %>%
  select(biosum_cond_id, rxpackage, rxcycle, complete_cpa, chip_yield_gt, merch_yield_cf) %>%
  arrange(biosum_cond_id, rxpackage, rxcycle)


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



# join in acreage
pre_carbon_tot <- left_join(acres[,c("biosum_cond_id", "acres", "owngrpcd")], pre_carbon_tot)
post_carbon_tot <- left_join(acres[,c("biosum_cond_id", "acres", "owngrpcd")], post_carbon_tot)

### join to econ
pre_full <- left_join(pre_carbon_tot, cost_sel)  %>%
  distinct()
post_full <- left_join(post_carbon_tot, cost_sel)  %>%
  distinct()



#### give cond_id new numbers that will make them easier to join together after reimporting data
plots <- unique(post_full$biosum_cond_id)
new_id <- data.frame(biosum_cond_id = plots, ID = 1:length(plots))


plots_loc <- left_join(new_id, cond_lat_lon)


# write_csv(plots_loc, "plot_loc.csv")

# ##### write to csv
# write_csv(pre_full, "pre_harv_full.csv")
# write_csv(post_full, "post_harv_full.csv")

# pre_full <- read_csv("pre_harv_full.csv")
# post_full <- read_csv("post_harv_full.csv")


# name pre and post for future reference
pre_full$section <- "pre"
post_full$section <- "post"


## full df 
all_data <- bind_rows(pre_full,post_full)

all_data <- left_join(all_data, new_id)


#####################
#### data cleaning ##
#####################


# filter down to just one plot
plot_all <- all_data %>% 
  mutate(time = rxcycle) %>% 
  filter(biosum_cond_id %in% unique(all_data$biosum_cond_id)[10] & rxpackage == "001")

#### match cycle to year ####
for (i in length(plot_all$time)){
  
  plot_all$time = ifelse(plot_all$time==1 & plot_all$section == "pre", 0, plot_all$time) 
  plot_all$time = ifelse(plot_all$time==2 & plot_all$section == "pre", 10, plot_all$time) 
  plot_all$time = ifelse(plot_all$time==3 & plot_all$section == "pre", 20, plot_all$time) 
  plot_all$time = ifelse(plot_all$time==4 & plot_all$section == "pre", 30, plot_all$time) 
  plot_all$time = ifelse(plot_all$time==1 & plot_all$section == "post", 1, plot_all$time) 
  plot_all$time = ifelse(plot_all$time==2 & plot_all$section == "post", 11, plot_all$time) 
  plot_all$time = ifelse(plot_all$time==3 & plot_all$section == "post", 21, plot_all$time) 
  plot_all$time = ifelse(plot_all$time==4 & plot_all$section == "post", 31, plot_all$time) 
  
}

#make sure time is numeric
plot_all$time <- as.numeric(plot_all$time)


### clean merch_carbon removed ####

# if cpa is NA, harvesting did not occur, therfor carbon removed == 0
plot_all$Merch_Carbon_Removed <- if_else(is.na(plot_all$complete_cpa), 0, plot_all$Merch_Carbon_Removed)
# remove duplicate caused by the pre/post
plot_all$Merch_Carbon_Removed <- if_else(plot_all$Merch_Carbon_Removed > 0 & plot_all$section == "pre" ,0, plot_all$Merch_Carbon_Removed)

### repeat for chip_yield
plot_all$chip_yield_gt <- if_else(is.na(plot_all$complete_cpa), 0, plot_all$chip_yield_gt)
# remove duplicate caused by the pre/post
plot_all$chip_yield_gt <- if_else(plot_all$chip_yield_gt > 0 & plot_all$section == "pre" ,0, plot_all$chip_yield_gt)


## clean cpa
# if cpa is NA, harvesting did not occur, therfor cost == 0
plot_all$complete_cpa <- if_else(is.na(plot_all$complete_cpa), 0 , plot_all$complete_cpa)
# remove duplicate caused by the pre/post
plot_all$complete_cpa <- if_else(plot_all$complete_cpa > 0 & plot_all$section == "pre", 0 , plot_all$complete_cpa)



############################################
#####  add discounting and optimize  #######
############################################

### create functions

## discount
add_discounting = function(df){
  pre_post <- df %>% 
    # make sure in the right order
    arrange(time) %>% 
    # find the difference between the the stand carbon in cycle 1 and 2, 2 and 3 etc
    mutate(diff = Total_Stand_Carbon - lag(Total_Stand_Carbon, default = first(Total_Stand_Carbon))) %>%
    # what happens each year
    mutate(each_year = diff/10)
  
  
  
  # now create a new dataframe with all of the times and make sure time is an integer
  tmp <- tibble(time = 0:31)
  
  # now merge together the dataframes
  plot_time <- left_join(tmp, pre_post, by = "time") 
  
  # now need to fill in the empty total stand carbon columns 
  for (i in 1:nrow(plot_time)){
    plot_time$each_year = ifelse(plot_time$time <=9 & plot_time$time >=2, 
                                 plot_time$each_year[11],
                                 plot_time$each_year)
    plot_time$each_year = ifelse(plot_time$time <=19 & plot_time$time >=12, 
                                 plot_time$each_year[21],
                                 plot_time$each_year)
    plot_time$each_year = ifelse(plot_time$time <=29 & plot_time$time >=22, 
                                 plot_time$each_year[31],
                                 plot_time$each_year)
  }
  
  ## add a discounted column that is discounted by 0.05 
  # that is the cumulative sum for each year of discounted carbon
  
  plot_time_discounts <- plot_time %>% 
    # discounted carbon for each year
    # mutate(discount_carb = each_year/((1+0.05)^time)) %>% 
    # cumulative discounted carbon
    mutate(cum_discount_carb = cumsum(each_year/((1+0.05)^time))) %>% 
    # discounted cost
    mutate(discount_cost = complete_cpa/((1+0.05)^time)) %>% 
    mutate(discount_cost = replace_na(discount_cost,0)) %>% 
    mutate(cum_discount_cost = cumsum(discount_cost)) 
  
  ## the information we want to end up with for each distinct plot and package
  final_cumulative <- plot_time_discounts %>% 
    filter(time == 31) %>% 
    select(biosum_cond_id, rxpackage, cum_discount_carb, cum_discount_cost)
  
  return(final_cumulative)
}


## create function `optimize_treatmen` that applies the discount function  ^ to each plot+package 
## then selects the optimal package for each plot
## optimal == lowest (non-negative), marginal cost

optimize_treatment <- function(df) {
  
  
  # pull out unique biosum ids and unique packages
  uniq_biosum_ids <- df %>% 
    distinct(biosum_cond_id) 
  
  uniq_biosum_ids <- uniq_biosum_ids[["biosum_cond_id"]]
  
  # loop through everything
  final_df = NULL
  optimal_full <- NULL
  
  number_completed = 0
  total_to_complete = length(uniq_biosum_ids)
  
  for (i in 1:length(uniq_biosum_ids)){
    
    # select plot
    id <- uniq_biosum_ids[i]
    
    plot <- plot_all %>% 
      filter(biosum_cond_id == paste(id))
    
    uniq_packages <- unique(plot$rxpackage)
    
    for (j in 1:length(uniq_packages)){
      
      pkg <- uniq_packages[j]
      
      plot_package <- plot %>% 
        filter(rxpackage == pkg) 
      
      test2 <- add_discounting(plot_package) 
      
      final_df <- rbind(final_df, test2)
      
      
    }
    
    optimal <- final_df %>% 
      mutate(cpu = cum_discount_cost/cum_discount_carb) %>% 
      filter(cpu > 0) %>% 
      filter(cpu == min(cpu)) 
    
    rm(final_df)
    final_df <- NULL
    
    if (i == 1) {
      optimal_full <- optimal
    } else {
      optimal_full <- bind_rows(optimal_full, optimal)
    }
    
    
    ### progress tracker
    number_completed = number_completed + 1
    if (number_completed %% 200 == 0 || number_completed == total_to_complete) {
      print(sprintf("Percentage completion: %.2f%%", (number_completed / length(uniq_biosum_ids)) * 100))
    }
    
  }
  
  return(optimal_full)
  
}

## run
optimal_treatments <- optimize_treatment(plot_all)



















