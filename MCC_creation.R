#### MCCs #######
library(tidyverse)


carb_cpu_05 <- read_csv("carb_cpu_05.csv")
carb_cpu_00 <- read_csv("carb_cpu_00.csv")

relative_og_05 <- read_csv("relative_carb_og_05.csv")
relative_og_00 <- read_csv("relative_carb_og_00.csv")

select_opt <- function(df) {
  optimal <- df %>% 
    filter(total_carbon > 0 & rxpackage != "031" & cum_discount_cost != 0) %>% 
    mutate(value = (200 * total_carbon) - total_cost) %>% 
    group_by(ID) %>% 
    filter(value > 0 &
             value == max(value))
  
  opt_tie_break <- optimal %>% 
    group_by(ID) %>% 
    sample_n(1) %>% 
    ungroup()
  
  cumsum <- opt_tie_break %>% 
    arrange(cpu) %>% 
    filter(cpu > -100 & cpu < 200) %>% 
    mutate(cumsum_carb = cumsum(total_carbon))
}

carb_opt_05 <- select_opt(carb_cpu_05)
og_opt_05 <- select_opt(relative_og_05)




carb_opt_05$baseline_method <- "CARB"
og_opt_05$baseline_method <- "BAU"

both <- bind_rows(carb_opt_05, og_opt_05)

library(scales)

ggplot(both, aes(cumsum_carb, cpu, group = baseline_method, color = baseline_method)) +
  geom_line(size = 2) +
  #geom_point(aes(color = cpu)) +
  #scale_colour_gradient2(low = "forestgreen", mid = "yellow", high = "red", midpoint = 50) +
  scale_x_continuous(limits = c(0, 25000000),label=comma) +
  scale_y_continuous(limits = c(-120,220), expand = c(0,0)) +
  theme_minimal(base_size = 24) +
  #theme(legend.position = "none") +
  labs(
    x = "Tons of Carbon",
    y = "$/Ton of Carbon"
  )



##### how much would be abated at the given price of 15

carb_15 <- carb_opt_05 %>% 
  filter(cpu <= 15)

abate_25 <- carb_opt_05 %>% 
  filter(cumsum_carb <= 430000) 

### this function approximates taking the integral of points w/in an x-y coordinate system
total_cost_25mt <- pracma::trapz(abate_25$cumsum_carb, abate_25$cpu)




##### expliring negative costs w/in og cc scenarios

cc <- relative_og_05 %>% 
  filter(rxpackage %in% c("032", "033"))


cc_neg <- cc %>% 
  filter(cpu < 0 & total_carbon > 0)





library(tidyverse)


carb_cpu_05 <- read_csv("carb_cpu_05.csv")
carb_cpu_00 <- read_csv("carb_cpu_00.csv")

relative_og_05 <- read_csv("relative_carb_og_05.csv")
relative_og_00 <- read_csv("relative_carb_og_00.csv")

select_opt <- function(df) {
  optimal <- df %>% 
    filter(total_carbon > 0) %>% 
    mutate(value = (200 * total_carbon) - total_cost) %>% 
    group_by(ID) %>% 
    filter(value > 0 &
             value == max(value))
  
  opt_tie_break <- optimal %>% 
    group_by(ID) %>% 
    sample_n(1) %>% 
    ungroup()
  
  cumsum <- opt_tie_break %>% 
    arrange(cpu) %>% 
    filter(cpu > -100 & cpu < 200) %>% 
    mutate(cumsum_carb = cumsum(total_carbon))
}

carb_opt_05 <- select_opt(carb_cpu_05)
og_opt_05 <- select_opt(relative_og_05)




carb_opt_05$baseline_method <- "CARB"
og_opt_05$baseline_method <- "BAU"

both <- bind_rows(carb_opt_05, og_opt_05)

library(scales)

ggplot(both, aes(cumsum_carb, cpu, group = baseline_method, color = baseline_method)) +
  geom_line(size = 2) +
  #geom_point(aes(color = cpu)) +
  #scale_colour_gradient2(low = "forestgreen", mid = "yellow", high = "red", midpoint = 50) +
  scale_x_continuous(limits = c(0, 25000000),label=comma) +
  scale_y_continuous(limits = c(-120,220), expand = c(0,0)) +
  theme_minimal(base_size = 24) +
  #theme(legend.position = "none") +
  labs(
    x = "Tons of Carbon",
    y = "$/Ton of Carbon"
  )






