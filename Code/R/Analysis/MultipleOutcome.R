################## Estimate SC using multiple outcome ########################
library(pacman)
p_load(tidyverse, fixest, broom, augsynth)

### Load Data
data = read_csv("Data/Cleaned/NVSS_Mortality_MainRegReady.csv")

### Get Control Units
control = data %>% 
  filter(!state_fips %in% c(41,53)) %>% 
  .$state_fips %>% unique()

reg_est = tibble()
for (treat in c(41, 53)) {
  if (treat==41) treattime = c(1,2)
  if (treat==53) treattime = c(2,3)
  
  for (trtime in treattime) {
    
    ## Weighted  
    reg =  data %>%
      as.data.frame() %>%
      filter(state_fips %in% control | state_fips==treat) %>%
      mutate(treat= ifelse(state_fips==treat & time >= trtime + 36, 1,0),
             spencer = spencer/pop*100000, 
             joshi = joshi/pop*100000) %>%
      augsynth(spencer+joshi ~ treat, 
                        state_fips, 
                        time, 
                        trtime+36, data=.,
               progfunc = "None", scm = T, 
               combine_method = "avg_concat", fixedeff = F, nu=1
               )
    
    reg = summary(reg)
    reg_est = reg$average_att %>%
      as.data.frame()  %>% 
      mutate(placebounit = treat, treatedunit=treat, trtime=trtime) %>% 
      bind_rows(reg_est)
    
  }
}


save(reg_est,  file=paste0("Estimate/Robustness/AugSynth_MultipleOutcome.RData"))



########### By Time #############
reg_est = tibble()
for (treat in c(41, 53)) {
  if (treat==41) treattime = c(1,2)
  if (treat==53) treattime = c(2,3)
  
  for (yr in c(2021:2023)) {
  for (trtime in treattime) {
    
    ## Weighted  
    reg =  data %>%
      as.data.frame() %>%
      filter(state_fips %in% control | state_fips==treat) %>%
      filter(time < trtime+36 | year==yr) %>%
      mutate(treat= ifelse(state_fips==treat & time >= trtime + 36, 1,0),
             spencer = spencer/pop*100000, 
             joshi = joshi/pop*100000) %>%
      augsynth(spencer+joshi ~ treat, 
               state_fips, 
               time, 
               trtime+36, data=.,
               progfunc = "None", scm = T, 
               combine_method = "avg_concat", fixedeff = F, nu=1
      )
    
    reg = summary(reg)
    reg_est = reg$average_att %>%
      as.data.frame()  %>% 
      mutate(placebounit = treat, treatedunit=treat, trtime=trtime, timeperiod=yr) %>% 
      bind_rows(reg_est)
    
  }
}
}


save(reg_est,  file=paste0("Estimate/Robustness/AugSynth_MultipleOutcome_Yearly.RData"))