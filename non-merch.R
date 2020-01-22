non_merch <- read_delim("chip_pathways.txt", delim = ",")
merch_decay <- read_csv("softwood_lumber_decay.csv")

short <- filter(non_merch, Year < 30)



plot_pkg <- plot_all %>% 
  filter(ID == 2, rxpackage == "004")

df <- plot_pkg

  pre_post <- df %>% 
    # make sure in the right order
    arrange(time) %>% 
    # find the difference between the the stand carbon in cycle 1 and 2, 2 and 3 etc
    mutate(diff = Total_Stand_Carbon - lag(Total_Stand_Carbon, default = first(Total_Stand_Carbon))) %>%
    # what happens each year
    mutate(each_year = diff/10) %>% 
    # convert green tons to C
    mutate(merch_carbon = merch_yield_gt * .5)
  
  
  
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
  
  ## add in devay of merchantable wood
  plot_time$merch_decay <- add_harvest_decay(plot_time)
  
  ## get yearly difference for merch
  plot_merch_diff <- plot_time %>% 
    mutate(merch_diff = merch_decay - lag(merch_decay),
           merch_diff = replace_na(merch_diff, 0))
    
  
  
  ## add a discounted column that is discounted by 0.05 
  # that is the cumulative sum for each year of discounted carbon
  
  plot_time_discounts <- plot_merch_diff %>% 
    # discounted carbon for each year
    # mutate(discount_carb = each_year/((1+0.05)^time)) %>% 
    # cumulative discounted carbon
    mutate(cum_discount_carb = cumsum(each_year/((1+0.05)^time))) %>% 
    mutate(cum_discount_merch = cumsum(merch_diff/((1+0.05)^time))) %>% 
    mutate(total_discount_carb = cum_discount_carb + cum_discount_merch) %>% 
    # discounted cost
    mutate(discount_cost = complete_cpa/((1+0.05)^time)) %>% 
    mutate(discount_cost = replace_na(discount_cost,0)) %>% 
    mutate(cum_discount_cost = cumsum(discount_cost)) 
  
  ## the information we want to end up with for each distinct plot and package
  final_cumulative <- plot_time_discounts %>% 
    filter(time == 31) %>% 
    select(biosum_cond_id, ID, acres, rxpackage, cum_discount_carb, cum_discount_cost, total_discount_carb)
  
  
  

add_harvest_decay <- function(df) {
  tmp <- df %>% 
    select(time, section, rxcycle, Total_Stand_Carbon, diff, each_year, merch_carbon)
  
  ### if a harvest occured, start a time count of 1, if not == NA
  merch_time <- tmp %>% 
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
