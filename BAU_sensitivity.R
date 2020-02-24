############################
### Baseline Sensitivity ###
############################
library(tidyverse)


## ensure that plot ids are read in as entire number (vs being auto-converted to scientific notation)
options(scipen = 999)

all_data <- read_csv("all_data.csv")
all_discounted_FullDecay <- read_csv("all_discounted_FullDecay.csv")



###########################
## wrangle data ########
#########################

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





###########################################
### function for getting relative carb ###
##########################################
get_optimal_sites <- function(baseline_sites){
  ### first, wrangle data:
  ## get grow only for each plot
  grow_only <- all_discounted_FullDecay %>% 
    filter(rxpackage == "031") %>% 
    select(biosum_cond_id, ID, acres, rxpackage, cum_discount_carb) %>% 
    rename("grow_only_carb" = "cum_discount_carb")
  
  
  ## get relevent data from selected_sites
  selected_data <- baseline_sites %>% 
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
  
  
  relative_carb <- incorp_base %>% 
    mutate(relative_carb = total_discount_carb-base_disc_carb,
           relative_cost = cum_discount_cost-base_disc_cost,
           relative_val = cum_discount_val-base_disc_val) %>% 
    mutate(total_carbon = relative_carb * acres,
           total_cost = relative_cost* acres,
           total_val = relative_val*acres,
           cpu = total_cost/total_carbon,
           cpu_rev = (total_cost-total_val)/total_carbon)
  
  price <- 200
  
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
  
}


###############################
####### run selection  ########
###############################

all_selection <- list()

for (i in 1:10) {
  
  print(i)
  
  random_site_public <- lapply(unique_counties, randomize_sites, ownrcd = 10)
  random_site_private <- lapply(unique_counties, randomize_sites, ownrcd = 40)
  
  random_public <- bind_rows(random_site_public)
  random_private <- bind_rows(random_site_private)
  
  
  selected_sites <- bind_rows(random_private, random_public)

  optimal <- get_optimal_sites(selected_sites)
  optimal$run <- i
  
  all_selection[[i]] <- optimal
  
}


res <- bind_rows(all_section)

filt <- filter(res, run == 1)

p <- ggplot() +
  geom_line(data = filt, aes(time, data), color = 1, linetype = 1)

for (i in 2:10) {
  
  filt <- filter(res, run == i)
  
  p <- p + geom_line(data = filt, aes(time, data), linetype = i, color = i)
  
}

p




