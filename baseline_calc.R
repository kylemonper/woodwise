##### calculating baseline amount of forest carbon #####
###
###
## known harvest amounts in 2012 (in mmbf):
## private/tribal : ~1,197
## state : ~ 27.9
## federal ~ 203
## blm: .4
## total: ~ 1,425,000 thousand board feet
##

library(tidyverse)
library(RODBC)
library(datapasta)
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
  mutate(merch_yield_bf = merch_yield_cf*acres*12)


#### what kind of numers are we working with??? ####
## calculate total harvest (in bf) are harvested during cycle 1 by package
tot_priv_bf <- data.frame(matrix(NA, nrow = length(unique(private$rxpackage)), ncol = 2))
names(tot_priv_bf) <- c("rxpackage", "harv_bf")
for(i in 1:length(unique(private$rxpackage))){
  
  package <- unique(private$rxpackage)[i]
 
  cc <- filter(private, rxpackage == package)
  
  cc <- filter(cc, rxcycle == 1)
  
  total_private_harv_bf <- sum(cc$merch_yield_bf)
  
  tot_priv_bf$rxpackage[i] <-  package
  tot_priv_bf$harv_bf[i] <- total_private_harv_bf
  
}

##### ^^^ obviously too much...

#### next need to choose which plots to pick (based on 2012 harvest numbers by county ?) #####


#### workflow: ####
## -randomly select plots within each county that together match 2012 harvest numbers (can we get better numbers: BEN?)
##    - if selected: "apply" clearcut to privately owned and !!!some package!!! to federal (how do we choose which package) to be used as the baseline
##  
##    - if not selected: baseline = grow only

##############################
####### data wrangling #######
##############################
# read in spatial joined plot w/ counties
# read in harvest data from 2012
# join, convert to mmb, and select appropriate data for pricate and federal


counties <- read_csv("plot_county.csv") %>% 
  select(ID, NAME)

harvest_2012 <- read_csv("CA_harvest_2012.csv")


### joing with plot_all (from carbon_2.R after running through line 144)
plot_county <- left_join(all_data, counties)

plot_county <- left_join(plot_county, harv)

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
  select(biosum_cond_id, acres, owngrpcd, rxcycle, rxpackage, NAME, mmbf_county, merch_yield_mmbf) %>% 
  rename("county" = "NAME")
### get rid of pre and post
plot_sel <- distinct(plot_sel)






###############################
####### begin analysis ########
###############################

## define percent allocation of harvest to federal and private (based on 2012 harvest report)
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
        if(sum(acres_assigned$random_harvest_assign, na.rm = T)  == 0) {
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



random_site_public <- lapply(unique_counties, randomize_sites, ownrcd = 10)



random_site_private <- lapply(unique_counties, randomize_sites, ownrcd = 40)


random_public <- bind_rows(random_site_public)
random_private <- bind_rows(random_site_private)

selected_sites <- bind_rows(random_private, random_public)






