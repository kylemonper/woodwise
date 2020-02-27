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
  select(biosum_plot_id, lat, lon, elev)

cond_sel <- cond_m %>%
  select(biosum_cond_id, biosum_plot_id)

cond_lat_lon <- left_join(cond_sel, plot_sel)


##############################
#### select columns/join ####
############################

## harvest cost per acre
cost_sel <- cost %>%
  mutate(complete_cpa = harvest_onsite_cpa + haul_chip_cpa + haul_merch_cpa) %>%
  select(biosum_cond_id, rxpackage, rxcycle, complete_cpa, haul_chip_cpa, haul_merch_cpa, harvest_onsite_cpa, chip_yield_gt, merch_yield_cf, merch_yield_gt, merch_val_dpa, chip_val_dpa) %>%
  arrange(biosum_cond_id, rxpackage, rxcycle)


## stand carbon
pre_carb_sel <- pre_carb %>%
  select(biosum_cond_id, fvs_variant, rxpackage, rxcycle, Total_Stand_Carbon)

post_carb_sel <- post_carb %>%
  select(biosum_cond_id, fvs_variant, rxpackage, rxcycle, Total_Stand_Carbon)



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


write_csv(plots_loc, "plot_loc.csv")
test <- read_csv("plot_loc.csv")


# name pre and post for future reference
pre_full$section <- "pre"
post_full$section <- "post"


## full df 
all_data <- bind_rows(pre_full,post_full)

all_data <- left_join(all_data, new_id) 

## clean environment of everything except the new full dataset
rm(list = setdiff(ls(),"all_data"))

write_csv(all_data, "all_data.csv")
all_data <- read_csv("all_data.csv")

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
  select(ID, lat, lon, NAME)
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
  ## multiple how much is harvested in county by the statewide harvest percentage 
  #~ (if privatemultiple by private pct, otherwise multiply by federal pct
  result <- selected_county %>% 
    mutate(target = if_else(owngrpcd == 40, mmbf_county * private_pct, mmbf_county * federal_pct))
  
  
}

unique_counties <- unique(plot_sel$county)
target_list <- lapply(unique_counties, set_target)
plot_targets <- bind_rows(target_list)


#### function for randomizing sites within each area based on some specifed harvest amount (detirmed by the error around the target harvest value for each area)
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
        
        
       ## ensure that we cant harvest more area that the plot has 
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

# write_csv(selected_sites, "baseline_sites.csv")


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

### clean harvest data ####
## remove na's, and duplicates from pre/post

clean_harvest_data <- function(column) {
  
  plot_all[,column] <-  if_else(is.na(plot_all$complete_cpa), 0, unlist(plot_all[,column]))
  plot_all[,column] <- if_else(unlist(plot_all[column]) > 0 & plot_all$section == "pre" ,0, unlist(plot_all[,column]))
  
  return(unlist(plot_all[,column]))
  
}


plot_all$Merch_Carbon_Removed <- clean_harvest_data("Merch_Carbon_Removed")
plot_all$chip_yield_gt <- clean_harvest_data("chip_yield_gt")
plot_all$merch_yield_gt <-  clean_harvest_data("merch_yield_gt")
plot_all$haul_chip_cpa <- clean_harvest_data("haul_chip_cpa")
plot_all$haul_merch_cpa <- clean_harvest_data("haul_merch_cpa")
plot_all$harvest_onsite_cpa <- clean_harvest_data("harvest_onsite_cpa")
plot_all$chip_val_dpa <- clean_harvest_data("chip_val_dpa")
plot_all$merch_val_dpa <- clean_harvest_data("merch_val_dpa")
plot_all$complete_cpa <- clean_harvest_data("complete_cpa") # keep complete_cpa last


############################################
#####  add discounting and optimize  #######
############################################

#### decay rates for merch and non-merch
merch_decay <- read_csv("softwood_lumber_decay.csv")
non_merch <- read_delim("chip_pathways.txt", delim = ",")


### create functions ####

## product decay
add_harvest_decay <- function(df) {
  
  ### if a harvest occured, start a time count of 1, if not == NA
  merch_time <- df %>% 
    mutate(cycle1_merch = if_else(merch_carbon > 0 & rxcycle == 1 & section == "post", 1, NA_real_),
           cycle2_merch = if_else(merch_carbon > 0 & rxcycle == 2 & section == "post", 1, NA_real_),
           cycle3_merch = if_else(merch_carbon > 0 & rxcycle == 3 & section == "post", 1, NA_real_),
           cycle4_merch = if_else(merch_carbon > 0 & rxcycle == 4 & section == "post", 1, NA_real_))
  
  ## next step: how to add +1 to each row to have "time since harvest"
  cum.na <- function(x){
    # set NA's to 0
    x[which(is.na(x))] <- 0
    # set cumulative sum twice to increase each element n+1 
    x <- cumsum(cumsum(x))
    # reset the 0 values to NA again
    x[which(x < 1)] <- NA_real_
    return(x)
  }
  
  
  merch <- merch_time %>% 
    mutate(cycle1_time = cum.na(cycle1_merch),
           cycle2_time = cum.na(cycle2_merch),
           cycle3_time = cum.na(cycle3_merch),
           cycle4_time = cum.na(cycle4_merch))
  
  
  ### add in decay for each cycle
  merch <- left_join(merch, merch_decay, by = c("cycle1_time" = "year")) %>% 
    rename("cycle1_rate" = "decay")
  merch <- left_join(merch, merch_decay, by = c("cycle2_time" = "year")) %>% 
    rename("cycle2_rate" = "decay")
  merch <- left_join(merch, merch_decay, by = c("cycle3_time" = "year")) %>% 
    rename("cycle3_rate" = "decay")
  merch <- left_join(merch, merch_decay, by = c("cycle4_time" = "year")) %>% 
    rename("cycle4_rate" = "decay")
  
  ## calculate decay based on decay rate * harvested for each cycle
  decay <- function(time, rate) {
    
    harvested <- merch$merch_carbon[which(merch[,time] == 1)]
    decay <- harvested * merch[,rate]
    if(nrow(decay) == 0) {
      decay <- as.vector(rep(NA_real_, nrow(merch)))
    }
    return(as_vector(decay))
  }
  
  
  merch_loss <- merch %>% 
    mutate(decay1 = decay("cycle1_time", "cycle1_rate"),
           decay2 = decay("cycle2_time", "cycle2_rate"),
           decay3 = decay("cycle3_time", "cycle3_rate"),
           decay4 = decay("cycle4_time", "cycle4_rate"))
  
  
  summed <- rowSums(merch_loss[,c("decay1","decay2","decay3","decay4")], na.rm = T)
  
  return(summed)
  
}

#### wood chip pathways + decay
add_chip_decay <- function(df, pathway, decay_pct){
  
  # fraction that goes to this pathway
  fraction <- df %>% 
    mutate(chip_carbon = chip_carbon * decay_pct)
  
  ### if a harvest occured, start a time count of 1, if not == NA
  chip_time <- fraction %>% 
    mutate(cycle1_chip = if_else(chip_carbon > 0 & rxcycle == 1 & section == "post", 1, NA_real_),
           cycle2_chip = if_else(chip_carbon > 0 & rxcycle == 2 & section == "post", 1, NA_real_),
           cycle3_chip = if_else(chip_carbon > 0 & rxcycle == 3 & section == "post", 1, NA_real_),
           cycle4_chip = if_else(chip_carbon > 0 & rxcycle == 4 & section == "post", 1, NA_real_))
  
  ## next step: how to add +1 to each row to have "time since harvest"
  cum.na <- function(x){
    # set NA's to 0
    x[which(is.na(x))] <- 0
    # set cumulative sum twice to increase each element n+1 
    x <- cumsum(cumsum(x))
    # reset the 0 values to NA again
    x[which(x < 1)] <- NA_real_
    return(x)
  }
  
  
  chip <- chip_time %>% 
    mutate(cycle1_time = cum.na(cycle1_chip),
           cycle2_time = cum.na(cycle2_chip),
           cycle3_time = cum.na(cycle3_chip),
           cycle4_time = cum.na(cycle4_chip))
  
  ## before joining in decay rate, select the path we want to include
  chip_decay <- non_merch %>% 
    filter(path == pathway) %>% 
    mutate(Year = Year + 1) %>% ### start at year 1 not 0
    select(Year, embodied)
  
  
  ### add in decay for each cycle
  chip <- left_join(chip, chip_decay, by = c("cycle1_time" = "Year")) %>% 
    rename("cycle1_rate" = "embodied")
  chip <- left_join(chip, chip_decay, by = c("cycle2_time" = "Year")) %>% 
    rename("cycle2_rate" = "embodied")
  chip <- left_join(chip, chip_decay, by = c("cycle3_time" = "Year")) %>% 
    rename("cycle3_rate" = "embodied")
  chip <- left_join(chip, chip_decay, by = c("cycle4_time" = "Year")) %>% 
    rename("cycle4_rate" = "embodied")
  
  ## calculate decay based on decay rate * harvested for each cycle
  decay <- function(time, rate) {
    
    harvested <- chip$chip_carbon[which(chip[,time] == 1)]
    decay <- harvested * chip[,rate]
    if(nrow(decay) == 0) {
      decay <- as.vector(rep(NA_real_, nrow(chip)))
    }
    return(as_vector(decay))
  }
  
  
  chip_loss <- chip %>% 
    mutate(decay1 = decay("cycle1_time", "cycle1_rate"),
           decay2 = decay("cycle2_time", "cycle2_rate"),
           decay3 = decay("cycle3_time", "cycle3_rate"),
           decay4 = decay("cycle4_time", "cycle4_rate"))
  
  
  summed <- rowSums(chip_loss[,c("decay1","decay2","decay3","decay4")], na.rm = T)
  
  return(summed)
}


## discount
add_discounting = function(df){
  pre_post <- df %>% 
    # make sure in the right order
    arrange(time) %>% 
    # find the difference between the the stand carbon in cycle 1 and 2, 2 and 3 etc
    mutate(diff = Total_Stand_Carbon - lag(Total_Stand_Carbon, default = first(Total_Stand_Carbon))) %>%
    # what happens each year
    mutate(each_year = diff/10) %>% 
    # convert green tons to C
    mutate(merch_carbon = merch_yield_gt * .325) %>% 
    mutate(chip_carbon = chip_yield_gt * .325)
  
  
  
  
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
  
  ## add in decay of merchantable wood
  plot_time$merch_decay <- add_harvest_decay(plot_time)
  ## add in decay of chip paths (decay/ biochar)
  plot_time$decay <- add_chip_decay(plot_time, "decay", decay_pct)
  plot_time$biochar <- add_chip_decay(plot_time, "biochar", char_pct)
  
  ## merch_decay is for merchantable, decay is for woodchips
  ## get yearly difference for merch and chips
  plot_merch_diff <- plot_time %>% 
    mutate(merch_diff = merch_decay - lag(merch_decay),
           merch_diff = replace_na(merch_diff, 0)) %>% 
    mutate(decay_diff = decay - lag(decay),
           decay_diff = replace_na(decay_diff, 0)) %>% 
    mutate(biochar_diff = biochar - lag(biochar),
           biochar_diff = replace_na(biochar_diff, 0)) 
  
  
  ## add a discounted column that is discounted by 0.05 
  # that is the cumulative sum for each year of discounted carbon
  
  plot_time_discounts <- plot_merch_diff %>% 
    # discounted carbon for each year
    # mutate(discount_carb = each_year/((1+0.05)^time)) %>% 
    # cumulative discounted carbon
    mutate(cum_discount_carb = cumsum(each_year/((1+0.05)^time))) %>% 
    mutate(cum_discount_merch = cumsum(merch_diff/((1+0.05)^time))) %>% 
    mutate(cum_discount_decay = cumsum(decay_diff/((1+0.05)^time))) %>% 
    mutate(cum_discount_biochar = cumsum(biochar_diff/((1+0.05)^time))) %>% 
    mutate(total_discount_carb = cum_discount_carb + cum_discount_merch + cum_discount_biochar + cum_discount_decay) %>% 
    # discounted cost
    mutate(discount_haul_merch = haul_merch_cpa/((1+0.05)^time)) %>% 
    mutate(discount_haul_chip = haul_chip_cpa/((1+0.05)^time)) %>% 
    mutate(discount_harvest = harvest_onsite_cpa/((1+.05)^time)) %>% 
    mutate(discount_cost = discount_haul_chip + discount_haul_merch + discount_harvest) %>% 
    mutate(discount_cost = replace_na(discount_cost,0)) %>% 
    mutate(cum_discount_cost = cumsum(discount_cost)) %>% 
    #discouneted revenue
    mutate(discount_merch_dpa = merch_val_dpa/((1+0.05)^time)) %>% 
    mutate(discount_chip_dpa = chip_val_dpa/((1+0.05)^time)) %>% 
    mutate(discount_val = discount_merch_dpa + discount_chip_dpa) %>% 
    mutate(discount_val = replace_na(discount_val, 0)) %>% 
    mutate(cum_discount_val = cumsum(discount_val))
  
  ## the information we want to end up with for each distinct plot and package
  final_cumulative <- plot_time_discounts %>% 
    filter(time == 31) %>% 
    select(biosum_cond_id, ID, acres, rxpackage, cum_discount_carb, cum_discount_merch, cum_discount_cost, discount_haul_chip , discount_haul_merch, discount_harvest, total_discount_carb, cum_discount_decay, cum_discount_biochar, cum_discount_val)
  
  return(final_cumulative)
}

## create function that applies the discount function  ^ to each plot+package 
discount_all <- function(df) {
  
  # pull out unique biosum ids
  uniq_biosum_ids <- unique(df$ID)
  
  # loop through everything
  final_total <- NULL
  final_plot <- NULL

  number_completed <- 0
  total_to_complete <- length(uniq_biosum_ids)
  
  for (i in 1:length(uniq_biosum_ids)){
    
    # select plot
    id <- uniq_biosum_ids[i]
    
    plot <- plot_all %>% 
      filter(ID == paste(id))
    
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
    if (number_completed %% 200 == 0 || number_completed == total_to_complete) {
      print(sprintf("Percentage completion: %.2f%%", (number_completed / length(uniq_biosum_ids)) * 100))
    }
    
  }
  
  return(final_total)
  
}

decay_pct <- 1
char_pct <- 0

# test <- plot_all %>% 
#   filter(ID == 11)
# 
# tmp_disc <- discount_all(test)



###########################
##### Run discount_all ####
###########################

decay_pct <- 1
char_pct <- 0
### discount all packages
## this will take ~2 hours
# Sys.time()
# all_discounted_FullDecay <- discount_all(plot_all)
# Sys.time()
# write_csv(all_discounted_FullDecay, "all_discounted_FullDecay.csv")

all_discounted_FullDecay <- read_csv("all_discounted_FullDecay.csv")
all_discounted_FullDecay$biosum_cond_id <- as.numeric(all_discounted_FullDecay$biosum_cond_id)

############################
# add in cost of THP and NTMP
############################
###
###       THP
###
############################

thp <- read_csv("CALFIRE_THPS.csv")
ntmp <- read_csv("CALFIRE_NTMPS.csv")

#### we need to rethink the weighted sum: this is waaaaaaay over estimating the cost

##THP
thp_grouped <- thp %>% 
  mutate(size = if_else(ACRES < 500, "small",
                        if_else(ACRES > 500 & ACRES < 2500, "medium", "large")))

thp_total <- thp_grouped %>% 
  group_by(size) %>% 
  summarise(total = sum(ACRES),
            avg_size = median(ACRES)) %>% 
  ungroup() %>% 
  mutate(cost = if_else(size == "small", 40000,
                        if_else(size == "medium", 80000, 120000)),
         avg_cpa = cost/avg_size)

thp_avg_cpa <- weighted.mean(thp_total$avg_cpa, thp_total$total)

### NTMP
ntmp_grouped <- ntmp %>% 
  mutate(size = if_else(ACRES < 500, "small", "large"))

ntmp_total <- ntmp_grouped %>% 
  group_by(size) %>% 
  summarise(total = sum(ACRES),
            avg_size = mean(ACRES)) %>% 
  ungroup() %>% 
  mutate(cost = if_else(size == "small", 48000, 124000),
         avg_cpa = cost/avg_size)

ntmp_avg_cpa <- weighted.mean(ntmp_total$avg_cpa, ntmp_total$total)


#####
## try more simple average for both:
thp_simple_average <- (40000/250 + 80000/1250 + 120000/2000)/3

ntmp_simple_average <- thp_simple_average*1.2 # 20% more than thp

##  agin for the simple
thp_simple_df <- data.frame(time = seq(1,31,6), thp_cpa = rep(thp_simple_average,6))

thp_cost_simp <- thp_simple_df %>% 
  mutate(disc_thp = thp_simple_average/((1+0.05)^time))

total_thp_simp <- sum(thp_cost_simp$disc_thp)


### get group ownercode
tmp_join <- left_join(all_discounted_FullDecay, all_data[,c("owngrpcd", "ID")]) %>% 
  distinct()

### if private and cc, add cost of thp, if private and thin, add ntmp
#~ then add this to the cum_discount_cost
all_discounted_FullDecay <- tmp_join %>% 
  mutate(plan_cost = if_else(owngrpcd == 40 & rxpackage %in% c("032", "033"), total_thp_simp*acres, 
                             if_else(owngrpcd == 40 & !rxpackage %in% c("032", "033"), ntmp_simple_average*acres, 0)),
         cum_discount_cost = cum_discount_cost + plan_cost)
  

##########################
### subtract baseline ####
##########################

### next steps:
#~ incorperate baseline
#     baseline for non-selected plots == grow only
#     baseline for selected plots == (acres_assign)*carbon[for base package] + (acres-acres_assign)*carbon[for baseline]
selected_sites <- read_csv("baseline_sites.csv")
selected_sites$biosum_cond_id <- as.numeric(selected_sites$biosum_cond_id)

### first, wrangle data:
## get grow only for each plot
grow_only <- all_discounted_FullDecay %>% 
  filter(rxpackage == "031") %>% 
  select(biosum_cond_id, ID, acres, rxpackage, cum_discount_carb) %>% 
  rename("grow_only_carb" = "cum_discount_carb")
  

## get relevent data from selected_sites
selected_data <- selected_sites %>% 
  select(biosum_cond_id, ID, acres, rxpackage, random_harvest_assign)

## get discounted carbon values for each of these selected sites

selected_disc <- left_join(selected_data, all_discounted_FullDecay, by = c("biosum_cond_id","acres", "ID", "rxpackage")) %>% 
  rename("rxpackage_sel" = "rxpackage") %>% # rename columns to distinguish them before joining
  rename("discount_carb_sel" = "cum_discount_carb") %>% 
  rename("cost_baseline_rx" = "cum_discount_cost") %>% 
  rename("discount_merch_sel" = "cum_discount_merch") %>% 
  rename("total_carb_sel" = "total_discount_carb") %>% 
  distinct()


all_base <- left_join(grow_only, selected_disc, by = c("acres", "ID")) %>% 
  distinct()



## calculate baseline
#~ first calculate the % of plot that is grow only vs radmonly selected acres
#~ then multiply this by the calculated discounted carbon value for each package

baseline_total <- all_base %>% 
  mutate(random_harvest_assign = replace_na(random_harvest_assign, 0),
         total_carb_sel = replace_na(total_carb_sel,0),
         cost_baseline_rx = replace_na(cost_baseline_rx,0),
         cum_discount_val = replace_na(cum_discount_val,0),
         pct_grow_only = ((acres-random_harvest_assign)/acres),
         pct_select = (random_harvest_assign/acres),
         base_disc_carb = (pct_grow_only*grow_only_carb)+(pct_select*total_carb_sel), 
         base_disc_cost = (pct_select*cost_baseline_rx),
         base_disc_val = (pct_select*cum_discount_val))
         
## joing togeter and calculate relative carbon
incorp_base <- left_join(all_discounted_FullDecay, baseline_total[,c("base_disc_carb","base_disc_cost","base_disc_val", "ID")]) %>% 
  distinct()

test <- incorp_base %>% 
  group_by(ID) %>% 
  tally()


relative_carb <- incorp_base %>% 
  mutate(relative_carb = total_discount_carb-base_disc_carb,
         relative_cost = cum_discount_cost-base_disc_cost,
         relative_val = cum_discount_val-base_disc_val) %>% 
  mutate(total_carbon = relative_carb * acres,
         total_cost = relative_cost* acres,
         total_val = relative_val*acres,
         cpu = total_cost/total_carbon,
         cpu_rev = (total_cost-total_val)/total_carbon)

### last write: 2/25/2020
#write_csv(relative_carb, "relativ_carb.csv")
relative_carb <- read_csv("relativ_carb.csv")

#####################
##### Optimize #####
###################


### final step:
## we now have final discounted values for each package for this plot, now select the package with the lowest CPU

price <- 200

## new method for selecting optimal (based on value of carbon)
  optimal <- relative_carb %>% 
    filter(total_carbon > 0 & rxpackage != "031") %>% 
    mutate(value = (price * total_carbon) - total_cost) %>% 
    group_by(ID) %>% 
    filter(value > 0 &
             value == max(value))

opt_tie_break <- optimal %>% 
  group_by(ID) %>% 
  sample_n(1) %>% 
  ungroup()

## original method for selecting optimal
optimal_og <- relative_carb  %>% 
  filter(total_carbon > 0) %>% 
  group_by(ID) %>% 
  filter(cpu == min(cpu)) %>% 
  ungroup()

opt_tie_break_og <- optimal_og %>% 
  group_by(ID) %>% 
  sample_n(1) %>% 
  ungroup()



optimal_noCC <- relative_carb %>% 
  filter(total_carbon > 0 ) %>%     
  filter(!rxpackage %in% c("032", "033")) %>% 
  mutate(value = (price * total_carbon) - total_cost) %>% 
  group_by(ID) %>% 
  filter(value > 0 &
           value == max(value))

opt_tie_break_nocc <- optimal_noCC %>% 
  group_by(ID) %>% 
  sample_n(1) %>% 
  ungroup()


###################################
############ RESULTS ##############
###################################

###### MCC #######
## get cumsum
cumsum <- opt_tie_break %>% 
  arrange(cpu) %>% 
  filter(cpu > -100 & cpu < 200) %>% 
  mutate(cumsum_carb = cumsum(total_carbon))


## get cumsum
cumsum_og <- opt_tie_break_og %>% 
  arrange(cpu) %>% 
  filter(cpu > -100 & cpu < 200) %>% 
  mutate(cumsum_carb = cumsum(total_carbon))
  

cumsum_noCC <- opt_tie_break_nocc %>% 
  arrange(cpu) %>% 
  filter(cpu > -100 & cpu < 200) %>% 
  mutate(cumsum_carb = cumsum(total_carbon))


library(scales) # for comma in x axis

ggplot(cumsum, aes(cumsum_carb, cpu)) +
  geom_point(aes(color = cpu)) +
  scale_colour_gradient2(low = "forestgreen", mid = "yellow", high = "red", midpoint = 50) +
  scale_x_continuous(limits = c(0, 60000000),label=comma) +
  scale_y_continuous(limits = c(-150,220), expand = c(0,0)) +
  theme_minimal(base_size = 24) +
  theme(legend.position = "none") +
  labs(
    x = "Tons of Carbon",
    y = "$/Ton of Carbon",
    title = "MCC (w/ CC)"
  )

ggplot(cumsum_noCC, aes(cumsum_carb, cpu)) +
  geom_point(aes(color = cpu)) +
  scale_colour_gradient2(low = "forestgreen", mid = "yellow", high = "red", midpoint = 50) +
  scale_x_continuous(limits = c(0, 60000000),label=comma) +
  scale_y_continuous(limits = c(-150,220), expand = c(0,0)) +
  theme_minimal(base_size = 24) +
  theme(legend.position = "none") +
  labs(
    x = "Tons of Carbon",
    y = "$/Ton of Carbon",
    title = "MCC (w/o CC)"
  )

### repeat for using rev
optimal_rev <- relative_carb  %>% 
  filter(total_carbon > 0 & rxpackage != "031") %>% 
  mutate(value = (price * total_carbon) - (total_cost - total_val)) %>% 
  group_by(ID) %>% 
  filter(value > 0 &
           value == max(value))

opt_tie_break_rev <- optimal_rev %>% 
  group_by(ID) %>% 
  sample_n(1) %>% 
  ungroup()

cumsum_rev <- opt_tie_break_rev %>% 
  arrange(cpu_rev) %>% 
  mutate(cumsum_rev = cumsum(total_carbon))

test <- opt_tie_break_rev %>% 
  filter(owngrpcd == 40)
  
ggplot(cumsum_rev, aes(cumsum_rev, cpu_rev)) +
  geom_point(aes(color = cpu), size = 2.5) +
  scale_colour_gradient2(low = "forestgreen", mid = "yellow", high = "red") +
  scale_x_continuous(limits = c(-1000, 95000000), expand = c(0,0),label=comma) +
  scale_y_continuous(limits = c(-1200,1500), expand = c(0,0)) +
  theme_minimal(base_size = 24) +
  theme(legend.position = "none") +
  labs(
    x = "Tons of Carbon",
    y = "$/Ton of Carbon"
  )


##### Package counts ####
package_count <- optimal %>% 
  group_by(rxpackage) %>% 
  tally()

package_count$rxpackage <- factor(package_count$rxpackage, levels = package_count$rxpackage[order(-package_count$n)])

ggplot(package_count, aes(as.factor(rxpackage), n)) +
  geom_col() +
  labs(
    title = "counts of optimal packages",
    x = "treatment package", 
    y = "count"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



##### get plot locations for mapping #####
counties <- read_csv("plot_county.csv") %>% 
  select(ID, lat, lon, NAME)
cpu_locs <- left_join(opt_tie_break, counties)
#write_csv(cpu_locs, "cpu_locs.csv")
# calculate the most common package per county and plot this



#### calculate average cpu for each (not including optimal) treatments

tmp <- relative_carb %>% 
  filter(cpu > -400 & cpu < 400)

ggplot(tmp, aes(x = reorder(rxpackage,-cpu,mean), y = cpu)) +
  geom_boxplot(outlier.shape = NA)


##### how much would be abated at the given price of 15

carb_15 <- cumsum %>% 
  filter(cpu <= 15)

abate_25 <- cumsum %>% 
  filter(cumsum_carb <= 25000000) 

### this function approximates taking the integral of points w/in an x-y coordinate system
total_cost_25mt <- pracma::trapz(abate_25$cumsum_carb, abate_25$cpu)

cpu_locs <- left_join(optimal, counties)


#### Summary table #####
summary_cc <- cpu_locs %>% 
  group_by(NAME, owngrpcd) %>% 
  summarise(
    average_cpa = mean(relative_cost),
    sd_cpa = sd(relative_cost),
    avg_carbon_pa = mean(relative_carb),
    sd_carbon = sd(relative_carb)
  )

rx_count <- cpu_locs %>% 
  group_by(NAME, owngrpcd, rxpackage) %>% 
  tally() %>% 
  top_n(3, n)

count_wide <- rx_count %>% 
  spread(rxpackage, n)


final_sum <- left_join(count_wide, summary_cc)

#### again for no CC
optimal_noCC <- relative_carb %>% 
  filter(total_carbon > 0 ) %>%     
  filter(!rxpackage %in% c("032", "033")) %>% 
  mutate(value = (price * total_carbon) - total_cost) %>% 
  group_by(ID) %>% 
  filter(value > 0 &
           value == max(value))


cpu_locs_nocc <- left_join(optimal_noCC, counties)


summary_nocc <- cpu_locs_nocc %>% 
  group_by(NAME, owngrpcd) %>% 
  summarise(
    average_cpa = mean(relative_cost),
    sd_cpa = sd(relative_cost),
    avg_carbon_pa = mean(relative_carb),
    sd_carbon = sd(relative_carb)
  )

rx_count_nocc <- cpu_locs_nocc %>% 
  group_by(NAME, owngrpcd, rxpackage) %>% 
  tally() %>% 
  top_n(3, n)


count_wide <- rx_count_nocc %>% 
  spread(rxpackage, n)

final_summary_nocc <- left_join(count_wide, summary_nocc)





write_csv(final_sum, "summary_table_cc.csv")
write_csv(final_summary_nocc, "summary_table_noCC.csv")



plot_count_nocc <- cpu_locs_nocc %>% 
  select(NAME, owngrpcd, ID) %>% 
  distinct() %>% 
  group_by(NAME, owngrpcd) %>% 
  tally()
write_csv(plot_count_nocc, "plot_count_nocc.csv")


plot_count_nocc <- cpu_locs %>% 
  select(NAME, owngrpcd, ID) %>% 
  distinct() %>% 
  group_by(NAME, owngrpcd) %>% 
  tally()
write_csv(plot_count_nocc, "plot_count_cc.csv")





###### if revenue compare lack of net revenue as cost


