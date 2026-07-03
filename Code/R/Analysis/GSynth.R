################## Run Generalized SC ########################
library(pacman)
p_load(tidyverse, tidycensus, magrittr, data.table, usmap, lubridate, synthdid, parallel, fixest, broom, augsynth, fect)

### Load Data
data = read_csv("Data/Cleaned/NVSS_Mortality_MainRegReady.csv")

### Get Control Units
control = data %>% 
  filter(!state_fips %in% c(41,53)) %>% 
  .$state_fips %>% unique()

reg_est = tibble()
for (y in c("joshi")) {
  
  for (treat in c(41, 53)) {
    if (treat==41) treattime = c(2)
    if (treat==53) treattime = c(3)
    
    for (trtime in treattime) {
      
      ### Factor Completion
      reg = data %>%
        as.data.frame() %>%
        filter(state_fips %in% control | state_fips==treat) %>%
        mutate(treat= ifelse(state_fips==treat & time >= trtime + 36, 1,0),
               od_rate = get(y)/pop*100000) %>%
          fect(od_rate ~ treat,
               data = .,
               index = c("state_fips","time"), 
               CV = TRUE, method = "both", force = "two-way",
               se=T, vartype = "jackknife",parallel = T,
               seed=12345)
      
      reg_est =  reg$est.avg %>% 
        as.data.frame()  %>% 
        mutate(placebounit = treat, outcome=y, treatedunit=treat, trtime=trtime, model="Factor Completion") %>% 
        bind_rows(reg_est)
      
      ### GSynth
      reg = data %>%
        as.data.frame() %>%
        filter(state_fips %in% control | state_fips==treat) %>%
        mutate(treat= ifelse(state_fips==treat & time >= trtime + 36, 1,0),
               od_rate = get(y)/pop*100000) %>%
        fect(od_rate ~ treat,
             data = .,
             index = c("state_fips","time"), 
             CV = TRUE, method = "ife", force = "two-way",
             se=T, vartype = "jackknife",parallel = T,
             seed=12345)
      
      reg_est =  reg$est.avg %>% 
        as.data.frame()  %>% 
        mutate(placebounit = treat, outcome=y, treatedunit=treat, trtime=trtime, model="GSynth") %>% 
        bind_rows(reg_est)
    }
  }
}

save(reg_est, file="Estimate/Robustness/GSynth.RData")

