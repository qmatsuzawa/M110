################## Estimate TWFE Estimate ########################
library(pacman)
p_load(tidyverse, tidycensus, magrittr, data.table, usmap, lubridate, synthdid, parallel, fixest, broom, augsynth)

### Load Data
data = read_csv("Data/Cleaned/NVSS_Mortality_MainRegReady.csv")

### Get Control Units
control = data %>% 
  filter(!state_fips %in% c(41,53)) %>% 
  .$state_fips %>% unique()

### Loop Over
i = 1
main_w = tibble()
main_uw = tibble()

for (y in c("joshi", "spencer")) {
for (treat in c(41, 53)) {
  if (treat==41) treattime = c(1,2)
  if (treat==53) treattime = c(2,3)
  
  for (trtime in treattime) {
    ## Weighted  
    reg = data %>%
      as.data.frame() %>%
      filter(state_fips %in% control | state_fips==treat) %>%
      mutate(treat= ifelse(state_fips==treat & time >= trtime + 36, 1,0),
             od_rate = get(y)/pop*100000) %>%
      feols(od_rate ~ treat | state_fips + time, weight=~pop, cluster=~state_fips)
    main_w = tidy(reg) %>% 
      mutate(placebounit = treat, outcome=y, treatedunit=treat, trtime=trtime) %>% 
      bind_rows(main_w)
    
    ## Unweighted
    reg = data %>%
      as.data.frame() %>%
      filter(state_fips %in% control | state_fips==treat) %>%
      mutate(treat= ifelse(state_fips==treat & time >= trtime + 36, 1,0),
             od_rate = get(y)/pop*100000) %>%
      feols(od_rate ~ treat | state_fips + time, cluster=~state_fips)
    main_uw = tidy(reg) %>% 
      mutate(placebounit = treat, outcome=y, treatedunit=treat, trtime=trtime) %>% 
      bind_rows(main_uw)
    
    ### Run Placebos
    for (i in control) {
      ## Weighted  
      reg = data %>%
        as.data.frame() %>%
        filter(state_fips %in% control) %>%
        mutate(treat= ifelse(state_fips==i & time >= trtime + 36, 1,0),
               od_rate = get(y)/pop*100000) %>%
        feols(od_rate ~ treat | state_fips + time, weight=~pop, cluster=~state_fips)
      main_w = tidy(reg) %>% 
        mutate(placebounit = i, outcome=y, treatedunit=treat, trtime=trtime) %>% 
        bind_rows(main_w)
      
      ## Unweighted
      reg = data %>%
        as.data.frame() %>%
        filter(state_fips %in% control) %>%
        mutate(treat= ifelse(state_fips==i & time >= trtime + 36, 1,0),
               od_rate = get(y)/pop*100000) %>%
        feols(od_rate ~ treat | state_fips + time, cluster=~state_fips)
      main_uw = tidy(reg) %>% 
        mutate(placebounit = i, outcome=y, treatedunit=treat, trtime=trtime) %>% 
        bind_rows(main_uw)
    }
  }
  }
}


save(main_w, main_uw, file=paste0("Estimate/Robustness/DiD_Main.RData"))

