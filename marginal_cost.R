##### testing the optimization method

cpt <- data.frame(
  ID = seq(1,10,1),
  rx1 = rdunif(10,0,100),
  rx2 = rdunif(10,0,100),
  rx3 = rdunif(10,0,100)
)

gather <- cpt %>% 
  gather(rx, cost, -ID) %>% 
  group_by(ID) %>% 
  filter(cost == min(cost))




pre_rx1 <- pre_full %>% 
  filter(rxcycle == 1) %>% 
  select(biosum_cond_id, rxpackage, rxcycle, harvest_onsite_cpa, tot_all)


post_rx4 <- post_full %>% 
  filter(rxcycle == 4) %>% 
  select(biosum_cond_id, rxpackage, rxcycle, harvest_onsite_cpa, tot_all) 



pre_post <- rbind(pre_rx1, post_rx4)


wide <- pre_post %>% 
  spread(rxcycle, tot_all) %>% 
  mutate(change = `4` - `1`)
