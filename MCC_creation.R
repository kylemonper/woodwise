#### MCCs #######
library(tidyverse)
library(ggthemes)
library(scales)


carb_cpu_05 <- read_csv("carb_cpu_05.csv")
carb_cpu_00 <- read_csv("carb_cpu_00.csv")

relative_og_05 <- read_csv("relative_carb_og_05.csv")
relative_og_00 <- read_csv("relative_carb_og_00.csv")

select_opt <- function(df, grow_only) {
  if(grow_only == TRUE) {
    optimal <- df %>% 
      filter(total_carbon > 0 & rxpackage != "031" & cum_discount_cost != 0) %>% 
      mutate(value = (200 * total_carbon) - total_cost) %>% 
      group_by(ID) %>% 
      filter(value > 0 &
               value == max(value))
  } else{
    optimal <- df %>% 
      filter(total_carbon > 0) %>% 
      mutate(value = (200 * total_carbon) - total_cost) %>% 
      group_by(ID) %>% 
      filter(value > 0 &
               value == max(value))
  }  
  
  opt_tie_break <- optimal %>% 
    group_by(ID) %>% 
    sample_n(1) %>% 
    ungroup()
  
  cumsum <- opt_tie_break %>% 
    arrange(cpu) %>% 
    filter(cpu > -100 & cpu < 200) %>% 
    mutate(cumsum_carb = cumsum(total_carbon))
}

### calc with grow only
carb_opt_05 <- select_opt(carb_cpu_05, grow_only = F)
og_opt_05 <- select_opt(relative_og_05, grow_only = F)


carb_opt_05$baseline_method <- "CARB"
og_opt_05$baseline_method <- "Business as Usual (BAU)"

both <- bind_rows(carb_opt_05, og_opt_05)
both$grow_type <- "Include grow only"

### calc w/o grow only
carb_opt_05_wo <- select_opt(carb_cpu_05, grow_only = T)
og_opt_05_wo <- select_opt(relative_og_05, grow_only = T)


carb_opt_05_wo$baseline_method <- "CARB"
og_opt_05_wo$baseline_method <- "Business as Usual (BAU)"

both_wo <- bind_rows(carb_opt_05_wo, og_opt_05_wo)
both_wo$grow_type <- "no grow only"

## combine
all <- bind_rows(both, both_wo)

#####################
######  GRAPH   #####
#####################

### Just BAU (no grow only)
ggplot(og_opt_05_wo, aes(cumsum_carb, cpu, color = baseline_method)) +
  geom_hline(yintercept = 0, alpha = .5) +
  geom_line(size = 2) +
  scale_x_continuous(limits = c(0, 2000000),label=comma) +
  scale_y_continuous(limits = c(-120,220), expand = c(0,0)) +
  scale_colour_manual(name = "Baseline Method", values=c("skyblue1")) +
  theme_solarized(base_size = 24) +
  #theme(legend.position = "none") +
  labs(
    x = "Tons of Carbon",
    y = "$/Ton of Carbon"
  ) 

## both baselines w/o grow only
ggplot(both_wo, aes(cumsum_carb, cpu, color = baseline_method)) +
  geom_hline(yintercept = 0, alpha = .5) +
  geom_line(size = 2) +
  scale_x_continuous(limits = c(0, 25000000),label=comma) +
  scale_y_continuous(limits = c(-120,220), expand = c(0,0)) +
  scale_colour_manual(name = "Baseline Method", values=c(CARB = "lightsalmon", `Business as Usual (BAU)` = "skyblue1")) +
  theme_solarized(base_size = 24) +
  #theme(legend.position = "none") +
  labs(
    x = "Tons of Carbon",
    y = "$/Ton of Carbon"
  ) 
                    

#### all MCCs
ggplot(data = all, aes(cumsum_carb, cpu, color = baseline_method, linetype = grow_type)) +
  geom_hline(yintercept = 0, alpha = .5) +
  geom_line(size = 2) +
  scale_x_continuous(limits = c(0, 25000000),label=comma) +
  scale_y_continuous(limits = c(-120,220), expand = c(0,0)) +
  scale_color_manual(name = "Baseline Method",
                     values = c(CARB = "lightsalmon", `Business as Usual (BAU)` = "skyblue1")) +
  scale_linetype_manual(name = "Management",
                        values = c("no grow only" = 1, "Include grow only" = 3)) +
  theme_solarized(base_size = 24) +
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


