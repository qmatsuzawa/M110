################## Estimate SC Prediction Intervals ########################
library(pacman)
p_load(tidyverse, magrittr, scinference, data.table, usmap, lubridate, synthdid, parallel, fixest, broom, augsynth, scpi)
set.seed(12345)


### Load Data
data = read_csv("Data/Cleaned/NVSS_Mortality_MainRegReady.csv") %>%
  mutate(spencer=spencer/pop*100000,
         joshi=joshi/pop*100000)

### Get Control Units
control = data %>% 
  filter(!state_fips %in% c(41,53)) %>% 
  .$state_fips %>% unique()


### Loopover & RUn
scpi_result = tibble()
for (alpha in c(5, 10)) {
for (y in c("spencer", "joshi")) {
  for (treated in c(41, 53)) {
    loopover = c(1,2)
    if (treated==53) loopover = loopover+1
    for (treatmo in loopover) {
      
      ### Run SCPI
      setup = data %>% 
        filter(state_fips %in% c(treated, control)) %>% 
        scdata(df = ., id.var = "state_fips", time.var = "time",
               outcome.var = y, period.pre = (1:(treatmo-1+36)),
               period.post = ((treatmo+36):72), unit.tr = treated,
               unit.co = control,
               constant = TRUE, cointegrated.data = TRUE)
      result = scpi(setup, u.alpha=alpha/100, e.alpha=alpha/100)
      
      ### Get Results
      y_actual = result$data$Y.post
      y_synth = result$est.results$Y.post.fit
      gaussian = result$inference.results$CI.all.gaussian
      att = y_actual - y_synth 
      conf.low = y_actual - gaussian[,2] 
      conf.high = y_actual - gaussian[,1]
      
      scpi_result = tibble(time = c((treatmo+36):72),
                           att = as.vector(att),
                           cilow= as.vector(conf.low),
                           cihigh= as.vector(conf.high),
                           start = treatmo,
                           treatunit=treated,
                           outcome=y,
                           pval = alpha
                           ) %>%
        bind_rows(scpi_result)
      
    }
  }
}
}

scpi_result %>% 
  group_by(start, treatunit, outcome, pval) %>%
  summarize(att = mean(att),
            cilow=mean(cilow),
            cihigh=mean(cihigh))

scpi_result %>%
  filter(treatunit==41) %>%
  filter((outcome=="joshi" & time <=51) | (outcome=="spencer" & time <=48)) %>%
  group_by(start, treatunit, outcome, pval) %>%
  summarize(att = mean(att),
            cilow=mean(cilow),
            cihigh=mean(cihigh))

save(scpi_result, file="Estimate/SC_Main/Robustness/SCPI.RData")
