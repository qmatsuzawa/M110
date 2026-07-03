######### Run Synthetic DD Estimate ###########
library(pacman)
p_load(tidyverse, tidycensus, magrittr, data.table, usmap, lubridate, synthdid, parallel)

### Load Data
data = read_csv("Data/Cleaned/NVSS_Mortality_MainRegReady.csv")

### Get Control Units
control = data %>% 
  filter(!state_fips %in% c(41,53)) %>% 
  .$state_fips %>% unique()

### Loop Over
i = 1
est = tibble()
for (y in c("joshi", "spencer")) {
for (treat in c(41, 53)) {
  if (treat==41) treattime = c(1,2)
  if (treat==53) treattime = c(2,3)
  
  for (trtime in treattime) {
    setup =  data %>%
      as.data.frame() %>%
      filter(state_fips %in% control | state_fips==treat) %>%
      mutate(treat= ifelse(state_fips==treat & time >= trtime + 36, 1,0),
             od_rate = get(y)/pop*100000)  %>%
      panel.matrices(. ,
                     unit="state_fips", 
                     time="time",
                     outcome="od_rate",
                     treatment="treat")
    
    tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
    # save(tau.hat, file=paste0("Estimate/SDiD/Replication/", d, "/Synth_", treat, "_start", trtime, "_", last, ".RData"))
    est = tibble(beta = as.numeric(tau.hat), 
                 outcome=y, treatedunit=treat, trtime=trtime) %>%
      bind_rows(est)
    }
  }
}

saveRDS(est, "Estimate/Robustness/SDiD_Main.rds")



######### Run Synthetic Placebo Estimates ###########

### Load Data
data = read_csv("Data/Cleaned/NVSS_Mortality_MainRegReady.csv")

### Get Control Units
control = data %>% 
  filter(!state_fips %in% c(41,53)) %>% 
  .$state_fips %>% unique()

### Loop Over
i = 1
placebos = tibble()
### Loop Over
for (y in c("joshi", "spencer")) {
  treattime = c(1,2,3)  
  for (trtime in treattime) {
      placebo_wts = tibble()
      for (i in control) {
        setup =  data %>%
          as.data.frame() %>%
          filter(state_fips %in% control) %>%
          mutate(treat= ifelse(state_fips==i & time >= trtime + 36, 1,0),
                 od_rate = get(y)/pop*100000)  %>%
          panel.matrices(. ,
                         unit="state_fips", 
                         time="time",
                         outcome="od_rate",
                         treatment="treat")
        
        tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
        placebos = tibble(beta = as.numeric(tau.hat), placebounit=i,
                          outcome=y, trtime=trtime) %>%
          bind_rows(placebos)
      }
  }
}

saveRDS(placebos, "Estimate/Robustness/SDiD_Placebos.rds")

