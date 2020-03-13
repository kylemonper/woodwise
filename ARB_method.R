#######
## ARB methodology


### all data from carbon_2.R
all_data <- read_csv("all_data.csv")


#### decay rates for merch and non-merch
merch_decay <- read_csv("softwood_lumber_decay.csv")
non_merch <- read_delim("chip_pathways.txt", delim = ",")

### CP values for each plot (from common_practice.R)
cp_plot <- read_csv("carb_base.csv") 

thp <- read_csv("CALFIRE_THPS.csv")
ntmp <- read_csv("CALFIRE_NTMPS.csv")

all_data_cp <- left_join(all_data, cp_plot)



# filter down to just one plot
plot_all <- all_data_cp %>% 
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
  plot_all[,column] <- if_else(unlist(plot_all[column]) > 0 & plot_all$section == "post" ,0, unlist(plot_all[,column]))
  
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




## product decay
add_harvest_decay <- function(df) {
  
  ### if a harvest occured, start a time count of 1, if not == NA
  merch_time <- df %>% 
    mutate(cycle1_merch = if_else(merch_carbon > 0 & rxcycle == 1 & section == "pre", 1, NA_real_),
           cycle2_merch = if_else(merch_carbon > 0 & rxcycle == 2 & section == "pre", 1, NA_real_),
           cycle3_merch = if_else(merch_carbon > 0 & rxcycle == 3 & section == "pre", 1, NA_real_),
           cycle4_merch = if_else(merch_carbon > 0 & rxcycle == 4 & section == "pre", 1, NA_real_))
  
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
    mutate(cycle1_chip = if_else(chip_carbon > 0 & rxcycle == 1 & section == "pre", 1, NA_real_),
           cycle2_chip = if_else(chip_carbon > 0 & rxcycle == 2 & section == "pre", 1, NA_real_),
           cycle3_chip = if_else(chip_carbon > 0 & rxcycle == 3 & section == "pre", 1, NA_real_),
           cycle4_chip = if_else(chip_carbon > 0 & rxcycle == 4 & section == "pre", 1, NA_real_))
  
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
add_discounting_CARB = function(df){
  carbon <- df %>% 
    # convert green tons to C
    mutate(merch_carbon = merch_yield_gt * .325) %>% 
    mutate(chip_carbon = chip_yield_gt * .325)
  
  
  
  new_time <- data.frame(time = c(-1,0,1,9,10,11,19,20,21,29,30,31)) %>% 
    left_join(carbon, by = "time")
  
  new_time <- replace_na(new_time, list(merch_carbon = 0, chip_carbon = 0, Aboveground_Total_Live = 0))
  
  
  for (i in c(1,4,7,10)) {
    
    new_time$Aboveground_Total_Live[i] <- new_time$merch_carbon[i+1] + new_time$chip_carbon[i+1] + new_time$Aboveground_Total_Live[i+1]
    
  }
  
  
  
  pre_post <- new_time %>% 
    arrange(time) %>% 
    # find the difference between the the stand carbon in cycle 1 and 2, 2 and 3 etc
    mutate(diff = Aboveground_Total_Live - lag(Aboveground_Total_Live, default = first(Aboveground_Total_Live)))
  
  
  ###### calculate yearly difference
  
  
  for (i in 2:(nrow(pre_post)-1)) {
    
    difference <- pre_post$Aboveground_Total_Live[i] - pre_post$Aboveground_Total_Live[i-1]
    time_since <- pre_post$time[i] - pre_post$time[i-1]
    
    pre_post$each_year[i] <- difference/time_since
    
  }
  ## do last year manually
  pre_post$each_year[12] <- pre_post$diff[12]
  
  
  # now create a new dataframe with all of the times and make sure time is an integer
  tmp <- tibble(time = -1:31)
  
  # now merge together the dataframes
  plot_time <- left_join(tmp, pre_post, by = "time") 
  
  # now need to fill in the empty total stand carbon columns 
  for (i in 1:nrow(plot_time)){
    plot_time$each_year = ifelse(plot_time$time <=8 & plot_time$time >=2, 
                                 plot_time$each_year[11],
                                 plot_time$each_year)
    plot_time$each_year = ifelse(plot_time$time <=18 & plot_time$time >=12, 
                                 plot_time$each_year[21],
                                 plot_time$each_year)
    plot_time$each_year = ifelse(plot_time$time <=28 & plot_time$time >=22, 
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
  
  #### set value of chips to zero
  plot_merch_diff$chip_val_dpa <- 0
  
  no_na <- replace_na(plot_merch_diff, list(haul_merch_cpa = 0, haul_chip_cpa = 0, harvest_onsite_cpa = 0, each_year = 0))
  
  # adjust timimg so that when discounting we dont use a -1
  new_time <- no_na %>% 
    mutate(new_time = time + 1)
  
  ### find discounted value of cp_et
  discount_cp <- new_time$carbon_metric_tons[2]/((1+dc_rate)^32)
  
  plot_time_discounts <- new_time %>% 
    # discounted carbon for each year
    # mutate(discount_carb = each_year/((1+0.05)^time)) %>% 
    # cumulative discounted carbon
    mutate(cum_discount_carb = cumsum(each_year/((1+dc_rate)^new_time))) %>% ### do we want to add in above_ground from year -1 here?
    mutate(cum_discount_merch = cumsum(merch_diff/((1+dc_rate)^new_time))) %>% 
    mutate(cum_discount_decay = cumsum(decay_diff/((1+dc_rate)^new_time))) %>% 
    mutate(cum_discount_biochar = cumsum(biochar_diff/((1+dc_rate)^new_time))) %>% 
    mutate(total_discount_carb = cum_discount_carb + cum_discount_merch + cum_discount_biochar + cum_discount_decay) %>%
    mutate(total_relative_carb = total_discount_carb - discount_cp) %>% 
    # discounted cost
    mutate(discount_haul_merch = haul_merch_cpa/((1+dc_rate)^new_time)) %>% 
    mutate(discount_haul_chip = haul_chip_cpa/((1+dc_rate)^new_time)) %>% 
    mutate(discount_harvest = harvest_onsite_cpa/((1+dc_rate)^new_time)) %>% 
    mutate(discount_cost = discount_haul_chip + discount_haul_merch + discount_harvest) %>% 
    mutate(discount_cost = replace_na(discount_cost,0)) %>% 
    mutate(cum_discount_cost = cumsum(discount_cost),
           cum_disc_haul_merch = cumsum(discount_haul_merch),
           cum_disc_haul_chip = cumsum(discount_haul_chip),
           cum_disc_harvest = cumsum(discount_harvest))%>% 
    #discouneted revenue
    mutate(discount_merch_dpa = merch_val_dpa/((1+dc_rate)^new_time)) %>% 
    mutate(discount_chip_dpa = chip_val_dpa/((1+dc_rate)^new_time)) %>% 
    mutate(discount_val = discount_merch_dpa + discount_chip_dpa) %>% 
    mutate(discount_val = replace_na(discount_val, 0)) %>% 
    mutate(cum_discount_val = cumsum(discount_val))
  
  ## the information we want to end up with for each distinct plot and package
  final_cumulative <- plot_time_discounts %>% 
    filter(time == 31) %>% 
    select(biosum_cond_id, ID, acres, rxpackage, total_relative_carb, cum_discount_carb, cum_discount_merch, cum_discount_cost, cum_disc_haul_chip , cum_disc_haul_merch, cum_disc_harvest , total_discount_carb, cum_discount_decay, cum_discount_biochar, cum_discount_val)
  
  
  return(final_cumulative)
}

## create function that applies the discount function  ^ to each plot+package 
discount_all_CARB <- function(df) {
  
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
      
      discounted <- add_discounting_CARB(plot_package) 
      
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



##### specify 
### -- % into wood chips vs biochar
### -- discount rate
char_pct <- 0
decay_pct <- 1
dc_rate <- .05



Sys.time()
all_discounted_CARB_05 <- discount_all_CARB(plot_all)
Sys.time()
write_csv(all_discounted_CARB_05, "all_discounted_CARB_dc_05.csv")


dc_rate <- 0

Sys.time()
all_discounted_CARB_00 <- discount_all_CARB(plot_all)
Sys.time()
write_csv(all_discounted_CARB_00, "all_discounted_CARB_dc_00.csv")



###### add thp costs
thp <- read_csv("CALFIRE_THPS.csv")
ntmp <- read_csv("CALFIRE_NTMPS.csv")


add_thp <- function(df) {
  
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
    tmp_join <- left_join(df, all_data[,c("owngrpcd", "ID")]) %>% 
      distinct()
    
    ### if private and cc, add cost of thp, if private and thin, add ntmp
    #~ then add this to the cum_discount_cost
    df <- tmp_join %>% 
      mutate(plan_cost = if_else(owngrpcd == 40 & rxpackage %in% c("032", "033"), total_thp_simp*acres, 
                                 if_else(owngrpcd == 40 & !rxpackage %in% c("032", "033"), ntmp_simple_average*acres, 0)),
             cum_discount_cost = cum_discount_cost + plan_cost)
    
    res <- df %>% 
      mutate(total_carbon = total_relative_carb * acres,
             total_cost = cum_discount_cost * acres,
             cpu = total_cost/total_carbon,
             total_rev = (cum_discount_cost-cum_discount_val) * acres,
             cpu_rev = total_rev/total_carbon)
    
    
    
  }

carb_thp_05 <- add_thp(all_discounted_CARB_05)
carb_thp_00 <- add_thp(all_discounted_CARB_00)



write_csv(carb_thp_05, "carb_cpu_05.csv")
write_csv(carb_thp_00, "carb_cpu_00.csv")

tmp <- carb_thp_00 %>% 
  filter(ID == 1)

##### how much would be abated at the given price of 15


carb_15 <- cumsum %>% 
  filter(cpu <= 15)

abate_25 <- cumsum %>% 
  filter(cumsum_carb <= 25000000) 

### this function approximates taking the integral of points w/in an x-y coordinate system
total_cost_25mt <- pracma::trapz(abate_25$cumsum_carb, abate_25$cpu)

