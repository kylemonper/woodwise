#################################################
### This code reads in relevent pre and post
### tables along with the optimization selections
### for calculating change in carbon sequestration 
### over the course of the 40 year treatments

library(tidyverse)
library(RODBC)

## ensure that plot ids are read in as entire number (vs being auto-converted to scientific notation)
options(scipen = 999)

################################
## read in data cleaned data ##
#############################

pre_full <- read_csv("pre_harv_full.csv")
post_full <- read_csv("post_harv_full.csv")

####################
#carbon discounting
####################

# test 1

# filter down to just one plot
plot_pre <- pre_full %>% 
  filter(biosum_cond_id==1200506050501500909686420) %>% 
  filter(rxpackage=="001") %>% 
  filter(complete_cpa!=0) %>% # need to change this
  select(-tot_all) %>% 
  mutate(time = rxcycle)

# make a loop to match cycle to year

for (i in length(plot_pre$time)){
  
  plot_pre$time = ifelse(plot_pre$time==1, 0, plot_pre$time) 
  plot_pre$time = ifelse(plot_pre$time==2, 10, plot_pre$time) 
  plot_pre$time = ifelse(plot_pre$time==3, 20, plot_pre$time) 
  plot_pre$time = ifelse(plot_pre$time==4, 30, plot_pre$time) 
  
}

#### for post
plot_post <- post_full %>% 
  filter(biosum_cond_id==1200506050501500909686420) %>% 
  filter(rxpackage=="001") %>% 
  filter(complete_cpa!=0) %>% ## need to change this
  select(-tot_all) %>% 
  mutate(time = rxcycle) 

# make a loop to match cycle to year
for (i in length(plot_post$time)){
  
  plot_post$time = ifelse(plot_post$time==1, 1, plot_post$time) 
  plot_post$time = ifelse(plot_post$time==2, 11, plot_post$time) 
  plot_post$time = ifelse(plot_post$time==3, 21, plot_post$time) 
  plot_post$time = ifelse(plot_post$time==4, 31, plot_post$time) 
  
}

#stack pre and post on top of each other
pre_post <- rbind(plot_pre, plot_post)

#add columns that show to each year carbon increments
# ** we want to look at from cycle 1--> 10, 11 --> 20, 21--> 30 so ignore the #
# changes from 0-->1 etc bc that's merchantable **** #

pre_post <- pre_post %>% 
  # make sure in the right order
  arrange(time) %>% 
  # find the difference between the the stand carbon in cycle 1 and 2, 2 and 3 etc
  mutate(diff = Total_Stand_Carbon - lag(Total_Stand_Carbon, default = first(Total_Stand_Carbon))) %>%
  # what happens each year
  mutate(each_year = diff/10)

# now create a new dataframe with all of the times and make sure time is an integer
plot_all <- tibble(time = 0:31)

# now merge together the dataframes
plot_all <- left_join(plot_all, pre_post, by = "time") %>% 
  mutate(stand_carbon_exp = Total_Stand_Carbon)

# now need to fill in the empty total stand carbon columns 
for (i in 2:nrow(plot_all)){
  plot_all$stand_carbon_exp = ifelse(plot_all$time <=9, 
                                       plot_all$stand_carbon_exp[i-1] - 1.3,
                                       plot_all$stand_carbon_exp)
}







