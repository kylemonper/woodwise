##### calculating baseline amount of forest carbon #####
###
###
## known harvest amounts in 2012 (in mmbf):
## private/tribal : ~1,193
## state : ~ 27.9
## federal ~ 203.3
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

### remove rows where area == NA (this means they were not harvested in 2012)

plot_actual <- filter(plot_area, !is.na(area))

## convert merch_yield_cf/acre to mmbf (total per site)
plot_bf <- mutate(plot_actual, merch_yield_mmbf = round(merch_yield_cf * acres * 12 / 1000000,2))


#### select only private plots, clearcut package, and first cycle
private_cc <- plot_bf %>% 
  filter(rxpackage == "032" & owngrpcd == 40 & rxcycle == 1) %>% 
  select(biosum_cond_id, rxpackage, NAME, area, mmbf_county, mmbf_area, merch_yield_mmbf)

#### federal plots, !!!!some package!!!! and first cycle
federal <- plot_bf %>% 
  filter(rxpackage == "002" & owngrpcd == 10 & rxcycle == 1)  %>% 
  select(biosum_cond_id, rxpackage, NAME, area, mmbf_county, mmbf_area, merch_yield_mmbf)






###############################
####### begin analysis ########
###############################

## define percent allocation of harvest to federal and private (based on 2012 harvest report)
#~ !!!! this is a major assumption that this same ration applies accross all counties/ areas

private_pct <- .837
federal_pct <- 1 - private_pct

private_cc$mmbf_target <- private_cc$mmbf_area * private_pct
federal$mmbf_target <- federal$mmbf_area * federal_pct


#### private by 'area' ####

#### function for randomizing sites within each area based on some specifed harvest range (detirmed by the error around the target harvest value for each area)
## added the last argument (min_sites) specifically for federal sites in th san joaquine
randomize_sites <- function(data, location, error, min_sites = 2){
  
  #filter data for sites that are in area and are harvested during rxcycle 1
  data_sel <- data %>% 
    filter(area == location & merch_yield_mmbf > 0)
  
  #define target mmbf for this area
  target <- data_sel$mmbf_target[1]
  
  ### randomLY select sites until at least 2 sites together harvest levels are with 20% of target
  results <- NULL
  
  while(is.null(results)) {
    
    #randomly arrange sites
    rand_data <- data_sel[sample(1:nrow(data_sel)),]
    
    #calculate cummulative sum 
    cum_sum <- rand_data %>% 
      mutate(cum_sum = cumsum(merch_yield_mmbf))
    
    ## select at least 2 sites that are within 20% of target
    selected <- cum_sum %>% 
      slice(min_sites:nrow(rand_nc)) %>% ## by slicing at two, we are ensuring that at least two plots are chosen !!!do we want this !!!????
      filter(cum_sum <= target * (1 + error) & cum_sum >= target * (1 - error)) ### filter for total harvest values w/in 20% of target
    
    ## if at least two sites were successfully selected, get list of sites, otherwise repeat loop
    if(nrow(selected) >= min_sites){
      results <- cum_sum %>% 
        filter(cum_sum <= target * 1.2)
    } else {
      results <- NULL
    }
    
  }
  
  return(results)
}


##### select private sites ######
private_nc <- randomize_sites(private_cc, "North Coast", .1)
private_sac <- randomize_sites(private_cc, "Sacramento", .1)
private_int <- randomize_sites(private_cc, "Northern Interior", .1)
private_sj <- randomize_sites(private_cc, "San Joaquin", .2) #any less error does not work because the target is too low w/in this region

private_sites <- bind_rows(private_nc, private_sac, private_int, private_sj)

## how close are we to our target
total_private_harvest <- sum(private_sites$merch_yield_mmbf)

paste("accuracy of private:", round(total_private_harvest/1193,4)*100)




##### select federal sites ######
federal_nc <- randomize_sites(federal, "North Coast", .1)
federal_sac <- randomize_sites(federal, "Sacramento", .15)
federal_int <- randomize_sites(federal, "Northern Interior", .1)
federal_sj <- randomize_sites(federal, "San Joaquin", .1, 1) 

federal_sites <- bind_rows(federal_nc, federal_sac, federal_int, federal_sj)

## how close are we to our target
total_federal_harvest <- sum(federal_sites$merch_yield_mmbf)

paste("accuracy of federal:", round(total_federal_harvest/203,4)*100)
















