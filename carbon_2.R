#################################################
### This code reads in relevent pre and post
### tables along with the optimization selections
### for calculating change in carbon sequestration 
### over the course of the 32 years of treatments
# 

#### next steps: ####
##~    - think about products
##~      -- merchantble (smith paper/ http://maps.gis.usu.edu/HWP/Home/About ?)
##~       - non-merch: Bodies numbers ()
##~    - subtract baseline
##~      -- see baseline_calc.R , but where to add into the workflow?
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




##############################
#### select columns/join ####
############################

## optimized packages + harvest cost per acre
cost_sel <- cost %>%
  mutate(complete_cpa = harvest_onsite_cpa + haul_chip_cpa + haul_merch_cpa) %>%
  select(biosum_cond_id, rxpackage, rxcycle, complete_cpa, chip_yield_gt, merch_yield_cf, merch_yield_gt) %>%
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

## clean environment of everything except the new full dataset
rm(list = setdiff(ls(),"all_data"))


###############################
#### Select Baseline plots ####
###############################


##############################
####### data wrangling #######
##############################
# read in spatial joined plot w/ counties
# read in harvest data from 2012
# join, convert to mmb, and select appropriate data for private and federal


counties <- read_csv("plot_county.csv") %>% 
  select(ID, NAME)
harvest_2012 <- read_csv("CA_harvest_2012.csv")

### joing with plot_all (from carbon_2.R after running through line 144)
plot_county <- left_join(all_data, counties)
plot_area <- left_join(plot_county, harvest_2012, by = c("NAME" = "county"))

### remove rows where area == NA (this means harvesting did not occur in this area within 2012)
plot_actual <- filter(plot_area, !is.na(area)) 

## convert merch_yield_cf to mmbf 
plot_bf <- mutate(plot_actual, merch_yield_mmbf = round(merch_yield_cf * 12 / 1000000,8))

#### select only "baseline packages" (clearcut for private, and !!!!!!002!!!!! for federal), and first cycle
# is 002 the right package to use?
plot_sel <- plot_bf %>% 
  filter(rxpackage == "032" & owngrpcd == 40 |
           rxpackage == "002" & owngrpcd == 10) %>% 
  select(biosum_cond_id, ID, acres, owngrpcd, rxcycle, rxpackage, NAME, mmbf_county, merch_yield_mmbf) %>% 
  rename("county" = "NAME")
### get rid of pre and post
plot_sel <- distinct(plot_sel)


###############################
####### run selection  ########
###############################
### STEPS:
#~ 1) set targets for private and federal w/in each county
#~ 2) randomly select plots within each county that meet those targets.

## first: define percent allocation of harvest to federal and private (based on 2012 harvest report)
#~ !!!! this is a major assumption that this same ration applies accross all counties/ areas

private_pct <- .837
federal_pct <- 1 - private_pct

## write function that sets the target harvest for each owner type within each county. 
#~ set target for total harvest based on each ratio^^


set_target <- function(county_sel) {
  ## filter county
  selected_county <- filter(plot_sel, county == county_sel)
  
  ## set target based on defined ratio
  result <- selected_county %>% 
    mutate(target = if_else(owngrpcd == 40, mmbf_county * private_pct, mmbf_county * federal_pct))
  
  
}

unique_counties <- unique(plot_sel$county)
target_list <- lapply(unique_counties, set_target)
plot_targets <- bind_rows(target_list)


#### function for randomizing sites within each area based on some specifed harvest range (detirmed by the error around the target harvest value for each area)
## FUNCTION WORKFLOW:
#~ select sites within specific county & ownder group & are harvested in year 0
#~ generate random numbers to simulate the harvested area of each plot (turning mmbf/acre into mmbf)
#~ randomly arrange [select] sites
#~ take the cumulative cumulative sum of their harvest
#~ if somehwhere that cumulative sum is within 5% of the target harvest for that county, select that arrangment of random plots/harvested area
#~ if after 10,000 random simulation, nothing is chosen, begin increasing acceptable error 



randomize_sites <- function(location, ownrcd){
  final_results <- NULL
  ## track status when running lapply()
  print(location)
  
  # loop through each cycle
  for(i in 1:4){
    
    print(i)
    #filter data for sites that are in area and are harvested during rxcycle 1
    data_filt <- plot_targets %>% 
      filter(county == location & owngrpcd == ownrcd & rxcycle == i & merch_yield_mmbf > 0)
    
    #if no plots meet these conditions, stop function
    if(nrow(data_filt) == 0){
      results <- NULL
    } else {
      
      #define target mmbf for this area (for use in error calculation)
      target <- data_filt$target[1]
      
      # this is to help define the exponential decay function for selecting random numbers
      ratio <- target/sum(data_filt$merch_yield_mmbf)
      
      #### for some counties, too small a ratio (i.e expnential decay function) doesn't give large enough harvest restults, so set minimum ratio to 50
      if(ratio < 50){
        ratio <- 50
      } else {
        ratio <- ratio
      }
      
      ### if a site was previously selected, use the same assigned harvested acres as the first time around
      if(!is.null(final_results)) {
        joined <- left_join(data_filt, final_results[,c("biosum_cond_id", "random_harvest_assign")], by = "biosum_cond_id")
      } else {
        joined <- data_filt 
      }
      
      
      
      ### randomly select sites until at selected sites are within some error of the target value
      results <- NULL
      j <- 1
      while(is.null(results)) {
        
        ## randomly assign the number of acres to be harvest
        #~ first step: create vector (of same length as filtered data) filled with random numbers
        
        random_harvest_acres <- data.frame(random_harvest_assign = rexp(nrow(joined),1/ratio))
        
        ## bind this column to data and get total random harvest
        acres_assigned <- bind_cols(joined, random_harvest_acres)
        
        ### if a site was previously selected, use the same assigned harvested acres as the first time around
        if(any(is.na(acres_assigned$random_harvest_assign))) {
          total_acres <- acres_assigned %>% 
            mutate(random_harvest_assign = if_else(is.na(random_harvest_assign), random_harvest_assign1, random_harvest_assign))
        } else {
          total_acres <- acres_assigned
        }
        
        
        
        total_harvest <- total_acres %>% 
          mutate(random_harvest_assign = if_else(random_harvest_assign > acres, acres, random_harvest_assign), 
                 total_yield = random_harvest_assign*merch_yield_mmbf)
        
        
        ## randomly arrange sites
        rand_data <- total_harvest[sample(1:nrow(total_harvest)),]
        
        
        
        #calculate cummulative sum 
        cum_sum <- rand_data %>% 
          mutate(cum_sum = cumsum(total_yield))
        
        #### assign error value of 1%; if we've gone through >2,000 while loops, increase the error by 1%
        ## this is to ensure that the loop will eventually finish if for some reason the numbers arn't working well
        if(j < 2000){
          error <- .01
        } else if(j %% 2000 == 0) {
          error <- .01 + (j/2000 * .01)
        }
        
        ## select at least 2 sites that are within 20% of target
        selected <- cum_sum %>% 
          filter(cum_sum <= target * (1 + error) & cum_sum >= target * (1 - error)) ### filter for total harvest values w/in some error of the target
        
        ## if at least two sites were successfully selected, get list of sites, otherwise repeat loop
        if(nrow(selected) >= 1){
          results <- cum_sum %>% 
            filter(cum_sum <= target * (1 + error))
          results$error <- error
        } else {
          results <- NULL
        }
        
        #count number of loops
        j <- j+1
      }
    }
    final_results <- bind_rows(final_results, results)
    if(nrow(final_results) == 0){
      final_results <- NULL
    }
    
  }
  
  return(final_results)
  
}



# random_site_public <- lapply(unique_counties, randomize_sites, ownrcd = 10)
# random_site_private <- lapply(unique_counties, randomize_sites, ownrcd = 40)
# 
# random_public <- bind_rows(random_site_public)
# random_private <- bind_rows(random_site_private)
# 
# ## check rough accuracy towards targets:
# paste("total harvest accuracy:",(sum(random_private$total_yield) + sum(random_public$total_yield))/1425/4)
# paste("private harvest accuracy:", sum(random_private$total_yield)/1197/4)
# paste("public harvest accuracy:",sum(random_public$total_yield)/203/4)
# 
# selected_sites <- bind_rows(random_private, random_public)

write_csv(selected_sites, "baseline_sites.csv")


######################################
######## Carbon Discounting ##########
######################################

#####################
#### data cleaning ##
#####################


# filter down to just one plot
plot_all <- all_data %>% 
  mutate(time = rxcycle) 

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
    select(biosum_cond_id, ID, acres, rxpackage, cum_discount_carb, cum_discount_cost)
  
  return(final_cumulative)
}


## create function that applies the discount function  ^ to each plot+package 


discount_all <- function(df) {
  
  
  # pull out unique biosum ids 
  uniq_biosum_ids <- df %>% 
    distinct(biosum_cond_id) 
  
  uniq_biosum_ids <- uniq_biosum_ids[["biosum_cond_id"]]
  
  # loop through everything
  final_total <- NULL
  final_plot <- NULL

  number_completed <- 0
  total_to_complete <- length(uniq_biosum_ids)
  
  for (i in 1:length(uniq_biosum_ids)){
    
    # select plot
    id <- uniq_biosum_ids[i]
    
    plot <- plot_all %>% 
      filter(biosum_cond_id == paste(id))
    
    # get all packages that pertain to this plot, loop through these, applying the discounting function to each
    uniq_packages <- unique(plot$rxpackage)
    
    for (j in 1:length(uniq_packages)){
      
      pkg <- uniq_packages[j]
      
      plot_package <- plot %>% 
        filter(rxpackage == pkg) 
      
      discounted <- add_discounting(plot_package) 
      
      final_plot <- rbind(final_plot, discounted)
      
      
    }
    
    final_total <- rbind(final_total, final_plot)
    final_plot <- NULL
    
    
    ### progress tracker
    number_completed = number_completed + 1
    if (number_completed %% 100 == 0 || number_completed == total_to_complete) {
      print(sprintf("Percentage completion: %.2f%%", (number_completed / length(uniq_biosum_ids)) * 100))
    }
    
  }
  
  return(final_total)
  
}


### discount all packages
## this will take ~20 minutes
all_discounted <- discount_all(plot_all)

##########################
### subtract baseline ####
##########################

### next steps:
#~ incorperate baseline
#     baseline for non-selected plots == grow only
#     baseline for selected plots == (acres_assign)*carbon[for base package] + (acres-acres_assign)*carbon[for baseline]


### first, wrangle data:
## get grow only for each plot
grown_only <- all_discounted %>% 
  filter(rxpackage == "031")
  

## get relevent data from selected_sites
selected_data <- selected_sites %>% 
  select(biosum_cond_id, ID, acres, rxpackage, random_harvest_assign)

## get discounted carbon values for each of these selected sites
selected_disc <- merge(selected_data, all_discounted) %>% 
  rename("rxpackage_sel" = "rxpackage") %>% # rename columns to distinguish them before joining
  rename("discount_carb_sel" = "cum_discount_carb") %>% 
  rename("cost_baseline_rx" = "cum_discount_cost")


all_base <- left_join(grown_only, selected_disc, by = c("biosum_cond_id","acres"))


## calculate baseline
#~ first calculate the % of plot that is grow only vs radmonly selected acres
#~ then multiply this by the calculated discounted carbon value for each package

baseline_total <- all_base %>% 
  mutate(random_harvest_assign = replace_na(random_harvest_assign, 0),
         discount_carb_sel = replace_na(discount_carb_sel,0),
         pct_grow_only = ((acres-random_harvest_assign)/acres),
         pct_select = (random_harvest_assign/acres),
         base_disc_carb = pct_grow_only*cum_discount_carb+pct_select*discount_carb_sel)
         
## joing togeter and calculate relative carbon
incorp_base <- left_join(all_discounted, baseline_total[,c("biosum_cond_id","base_disc_carb")])

relative_carb <- incorp_base %>% 
  mutate(relative_carb = cum_discount_carb-base_disc_carb)



### final step:
## we now have final discounted values for each package for this plot, now select the package with the lowest CPU

optimal <- relative_carb %>% 
  mutate(total_carbon = relative_carb* acres,
         total_cost = cum_discount_cost* acres,
         cpu = total_cost/total_carbon) %>% 
  filter(cpu > 0) %>% 
  group_by(biosum_cond_id) %>% 
  filter(cpu == min(cpu)) %>% 
  ungroup()






