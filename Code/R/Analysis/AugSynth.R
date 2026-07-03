################## Estimate Augmented SC Estimator ########################
library(pacman)
p_load(tidyverse, tidycensus, magrittr, data.table, usmap, lubridate, synthdid, parallel, fixest, broom, augsynth)

### Load Data
data = read_csv("Data/Cleaned/NVSS_Mortality_MainRegReady.csv")

### Get Control Units
control = data %>% 
  filter(!state_fips %in% c(41,53)) %>% 
  .$state_fips %>% unique()

reg_est = tibble()
for (y in c("joshi", "spencer")) {
  
  for (treat in c(41, 53)) {
    if (treat==41) treattime = c(1,2)
    if (treat==53) treattime = c(2,3)
    
    for (trtime in treattime) {
        
        ## Weighted  
        reg =  data %>%
          as.data.frame() %>%
          filter(state_fips %in% control | state_fips==treat) %>%
          mutate(treat= ifelse(state_fips==treat & time >= trtime + 36, 1,0),
                 od_rate = get(y)/pop*100000) %>%
          single_augsynth(od_rate ~ treat, state_fips, time, trtime+36, data=.)
        
        reg = summary(reg)
        reg_est = reg$average_att %>%
          as.data.frame()  %>% 
          mutate(placebounit = treat, outcome=y, treatedunit=treat, trtime=trtime) %>% 
          bind_rows(reg_est)

      }
    }
  }
  

save(reg_est,  file=paste0("Estimate/Robustness/AugSynth_Main.RData"))