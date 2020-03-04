add_discounting_old <- function(df, id, rx) {
  
  plot_all <- df %>% 
  mutate(time = rxcycle) %>% 
    filter(ID == id & rxpackage == rx)

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

df <- plot_all 


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
  
 no_na <- replace_na(plot_merch_diff, list(haul_merch_cpa = 0, haul_chip_cpa = 0, harvest_onsite_cpa = 0))
  
  plot_time_discounts <- no_na %>% 
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
    mutate(cum_discount_cost = cumsum(discount_cost),
           cum_disc_haul_merch = cumsum(discount_haul_merch),
           cum_disc_haul_chip = cumsum(discount_haul_chip),
           cum_disc_harvest = cumsum(discount_harvest))%>% 
    #discouneted revenue
    mutate(discount_merch_dpa = merch_val_dpa/((1+0.05)^time)) %>% 
    mutate(discount_chip_dpa = chip_val_dpa/((1+0.05)^time)) %>% 
    mutate(discount_val = discount_merch_dpa + discount_chip_dpa) %>% 
    mutate(discount_val = replace_na(discount_val, 0)) %>% 
    mutate(cum_discount_val = cumsum(discount_val))
}
  
  
  
#   
# test <- add_discounting_old(all_data, 34, "018")
# base <- add_discounting_old(all_data, 34, "031")
#   
# ggplot(test, aes(x = time, y = cum_discount_carb)) +
#   geom_line(color = "green") +
#   geom_line(aes(x = time, y = cum_discount_merch), color = "red") +
#   geom_line(aes(x = time, y = cum_discount_decay), color = "brown") +
#   geom_line(aes(x = time, y = total_discount_carb), linetype = "dashed") +
#   geom_line(data = base, aes(time, total_discount_carb), color = "blue") +
#   labs(title = "old method")
# 
# df_18 <- add_discounting_new_long(all_data, 34, "018")
# df_go <- add_discounting_new_long(all_data, 34, "031")
# 
# 
# 
# 
# ggplot(df_18, aes(x = time, y = cum_discount_carb)) +
#   geom_line(color = "green") +
#   geom_line(aes(x = time, y = cum_discount_merch), color = "red") +
#   geom_line(aes(x = time, y = cum_discount_decay), color = "brown") +
#   geom_line(aes(x = time, y = total_discount_carb), linetype = "dashed") +
#   geom_line(data = df_go, aes(x = time, y = total_discount_carb), color = "blue") +
#   labs(title = "new method")










