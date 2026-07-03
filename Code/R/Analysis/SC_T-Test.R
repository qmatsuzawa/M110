################## Estimate T-Stat (CI) for SC ########################
library(pacman)
p_load(tidyverse, magrittr, scinference, data.table, usmap, lubridate, synthdid, parallel, fixest, broom, augsynth)

### Load Data
data = read_csv("Data/Cleaned/NVSS_Mortality_MainRegReady.csv") %>%
  mutate(spencer=spencer/pop*100000,
         joshi=joshi/pop*100000)

### Get Control Units
control = data %>% 
  filter(!state_fips %in% c(41,53)) %>% 
  .$state_fips %>% unique()


### Loopover & RUn
sc_ttest = tibble()
for (k in c(3)) {
for (y in c("spencer", "joshi")) {
for (end in c(48, 51, 72)) {
for (treated in c(41, 53)) {
  loopover = c(1,2)
  if (treated==53) loopover = loopover+1
  for (treatmo in loopover) {
  
    ### T-Test -- Setup
    Y1 = data %>% 
      filter(state_fips==treated & time<=end) %>% 
      arrange(time) %>%
      mutate(y=get(y)) %>% 
      .$y
    Y0 = data %>% 
      filter(state_fips %in% control & time<=end) %>%
      mutate(y=get(y)) %>%
      arrange(time) %>% 
      pivot_wider(id_cols=time,names_from=state_fips, values_from=y) %>%
      select(-time) %>%
      as.matrix()
    
    T0 = treatmo + 36 - 1
    T1 = length(Y1) - T0
    
    ## Run!!!
    sc_t = scinference(Y1, Y0, T1, T0, inference_method="ttest", K=k)
    sc_ttest = sc_t %>% 
      as.data.frame() %>%
      mutate(dt=y, treated=treated, start=treatmo, end=end, K=k) %>%
      bind_rows(sc_ttest)
  }
}
}
}
}

sc_ttest %>% 
  filter((dt=="joshi" & treated==41 & start==2 & end==51) | 
    (dt=="spencer" & treated==41 & start==1 & end==48)) %>%
  View()

save(sc_ttest, file="Estimate/SC_Main/Robustness/SC_TTest.RData")
