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




carb_opt_05$base_method <- "CARB"
og_opt_05$base_method <- "BAU"

both <- bind_rows(carb_opt_05, og_opt_05)

library(scales)

ggplot(both, aes(cumsum_carb, cpu, group = base_method)) +
  geom_point(aes(color = cpu)) +
  scale_colour_gradient2(low = "forestgreen", mid = "yellow", high = "red", midpoint = 50) +
  scale_x_continuous(limits = c(0, 25000000),label=comma) +
  scale_y_continuous(limits = c(-150,220), expand = c(0,0)) +
  theme_minimal(base_size = 24) +
  theme(legend.position = "none") +
  labs(
    x = "Tons of Carbon",
    y = "$/Ton of Carbon"
  )
