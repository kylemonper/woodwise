####### incorperating products ########
#
#   where in the workflow do we inorperate prodcut decay?
#
#



df <- plot_all %>% 
  filter(biosum_cond_id %in% unique(plot_all$biosum_cond_id)[1] & rxpackage == "001") 


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