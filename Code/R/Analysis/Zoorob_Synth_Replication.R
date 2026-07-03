########### Zoorob Replication for SC #############

# seed for replicability
seed_number = 02138
set.seed(seed_number)
#
library(pacman)
p_load(data.table, tidyverse, ggplot2, geofacet, fixest, ggpubr, gsynth, fect)

################# Get Data (This is from rep file) #########################

##### seizure panel -- Use 2025 Data #####
dat = fread("Data/Raw/NFLIS/nflis_state_half_panel_2025.csv")
dat[, `:=`(ST=state_abb, Year=year)]
dat[, State := state.name[match(ST,state.abb)]]
dat[ST == "DC", State := "District of Columbia"]
dat = dat[!(ST %in% c("GU", "PR", "VI", "XX"))]

## mort h1
m1 = fread("fentanyl_oregon_replication_dataverse/data/wonder/janjune_alldrugod_Multiple Cause of Death, 2008-2020.csv")
m1 = m1[!is.na(Year)]
m1[, half := 1]
### add 2021 h1 data
m121 = fread("fentanyl_oregon_replication_dataverse/data/wonder/janjune_2021_alldrugod_Multiple Cause of Death, 2018-2021, Single Race.csv")
m121[, half:=1]
m1 = rbind(m1[,c("State","Year","Deaths","half")],
           m121[,c("State","Year","Deaths","half")])

## mhort h2
m2 = fread("fentanyl_oregon_replication_dataverse/data/wonder/julydec_alldrugod_Multiple Cause of Death, 2008-2020.csv")
m2[, half := 2]
### add 2021 h2 data
m221 = fread("fentanyl_oregon_replication_dataverse/data/wonder/julydec_2021_alldrugod_Multiple Cause of Death, 2018-2021, Single Race.csv")
m221[, half:=2]
m2 = rbind(m2[,c("State","Year","Deaths","half")],
           m221[,c("State","Year","Deaths","half")])

### merge
dat = merge(dat[Year>=2008], rbind(m1,m2)[,c("State","Year","Deaths","half")], all.x=T,
            by = c("State", "Year", "half"))
dat[, Year_half := paste(Year,half,sep="_")]
## interval var for time passage 
dat[, trend := as.numeric(factor(Year_half))]
## date version
dat[, date_trend := as.Date(ifelse(half==1, (paste(Year,"01","01",sep="-")),
                                   (paste(Year,"06","01",sep="-")))
) ]
##
## suppressed NAs (only 2) --> North Dakota 2010_2/2013_1 --> replace with random <10 number
nrow(dat[is.na(Deaths) & Year <= 2021])
dat[is.na(Deaths) & Year <= 2021, Deaths := round(runif(nrow(dat[is.na(Deaths) & Year <= 2021]), 0,9))]

### read in population
pop = fread("fentanyl_oregon_replication_dataverse/data/wonder/annual_alldrugod_Multiple Cause of Death, 2008-2020.csv")
pop21 = fread("fentanyl_oregon_replication_dataverse/data/wonder/annual_alldrugod_Multiple Cause of Death, 2018-2021_Single Race.csv")
pop = rbind(pop[!is.na(Year),c("State", "Year", "Population")],
            pop21[Year==2021, c("State", "Year", "Population")])[order(State, Year)]
## merge pop
dat = merge(dat, pop, all.x=T, by = c("State", "Year"))

## 2022, h1 2023 mort data + pop data
### read 2022/2023 half mort data
mort_22_h1 = fread("fentanyl_oregon_replication_dataverse/data/wonder/janjune_alldrugod_Provisional_Multiple Cause of Death, 2022.txt")
mort_22_h1[, half := 1]
mort_22_h2 = fread("fentanyl_oregon_replication_dataverse/data/wonder/julydec_alldrugod_Provisional_Multiple Cause of Death, 2022.txt")
mort_22_h2[, half := 2]
mort_22 = rbind(mort_22_h1, mort_22_h2)
mort_22 = mort_22[`Year Code` == 2022]

### read pop data
# pop data
pop = fread("fentanyl_oregon_replication_dataverse/data/wonder_spencer2023/pop_2018_sep2023.txt")
pop = pop[, c("Residence State", "Residence State Code", "Year", "Year Code", "Population")]
pop$Year = as.numeric(pop$Year)
### merge with mort data, create od_death_rate_half var
# merge pop data in
mort_22 = merge(mort_22[,c("Occurrence State", "Year Code", "half", "Deaths")],
                pop[`Year Code`==2022,c("Residence State", "Population")],
                all.x=T, by.x = c("Occurrence State"), by.y = c("Residence State"))
mort_22$State = mort_22$`Occurrence State`
mort_22$Year = mort_22$`Year Code`

#### New -- Add 2023 Data #####
mort_23 = fread("Data/Cleaned/NVSS_Mortality_MainRegReady.csv") %>%
  filter(year==2023) %>%
  mutate(`Occurrence State`=str_to_title(state_name),
         half = ifelse(monthdth < 7, 1,2)) %>% 
  group_by(`Occurrence State`, half) %>%
  summarize(Deaths = sum(joshi))
mort_23 = merge(mort_23[,c("Occurrence State", "half", "Deaths")],
                pop[`Year Code`==2023,c("Residence State", "Population")],
                all.x=T, by.x = c("Occurrence State"), by.y = c("Residence State"))
mort_23$State = mort_22$`Occurrence State`
mort_23$Year = 2023

## Bind 2022 & 2023 Data
mort_22_23 = rbindlist(list(mort_22[, c("State","Year", "half", "Population", "Deaths")],
                            mort_23[, c("State","Year", "half", "Population", "Deaths")]))

## dplyr row patch
dat = dplyr::rows_patch(dat, mort_22_23, by = c("State", "Year", "half"))

# mortality data rate calculation
dat[,od_death_rate_half := Deaths/Population*100000]
# fent_rate_half: normalize fentanyl seizures by population
#dat[,fent_rate_half := fent_count/Population*100000] 
dat[, fent_rate_half := pc_fent]
## nflis total number of seizures
dat[, nflis_fentanyl_percent_total := fent_count / alldrugs_seizure_count * 100 ]

## drug decriminalization indicator
dat$treatment = 0
dat[ST == "OR", treatment := as.numeric(trend >= mean(dat[Year == 2021 & half == 1]$trend))]
dat[ST == "WA", treatment := as.numeric(trend >= mean(dat[Year == 2021 & half == 1]$trend))]
## oregon drug decrim indicator
dat$treatment_oregon = 0
dat[ST == "OR", treatment_oregon := as.numeric(trend >= mean(dat[Year == 2021 & half == 1]$trend))]
dat$treatment_washington = 0
dat[ST == "WA", treatment_washington := as.numeric(trend >= mean(dat[Year == 2021 & half == 1]$trend))]

## Get changepoint
changepoint = read_csv("fentanyl_oregon_replication_dataverse/data/changepoint_state_amoc_nflis_fentanyl_percent.csv")
dat = merge(dat, changepoint, by="ST")
# changepoint %>% filter(ChangePoint>=33) 


###################### Replication -- Oregon ##########################
reg_est_att = tibble()
reg_est_es = tibble()

for (startyr in c(2008, 2014)) {
  for (endyr in c(2022, 2023)) {
  oregon_mc_nocontrol_22_fect <-  fect(od_death_rate_half ~ treatment_oregon,
                                       data = dat[Year<=endyr & Year>=startyr & ST != "WA"],
                                       index = c("ST","trend"), 
                                       CV = TRUE, method = "both", force = "two-way",
                                       se=T, vartype = "jackknife",parallel = T,
                                       seed=seed_number)
  
  oregon_mc_22_fect <-  fect(od_death_rate_half ~ treatment_oregon + nflis_fentanyl_percent_total,
                             data = dat[Year<=endyr & Year>=startyr &  ST != "WA"],
                             index = c("ST","trend"), 
                             CV = TRUE, method = "both", force = "two-way",
                             se=T, vartype = "jackknife",parallel = T,
                             seed=seed_number)
  
  oregon_mc_22_fect2 <-  fect(od_death_rate_half ~ treatment_oregon + fent_rate_half,
                              data = dat[Year<=endyr & Year>=startyr &  ST != "WA"],
                              index = c("ST","trend"), 
                              CV = TRUE, method = "both", force = "two-way",
                              se=T, vartype = "jackknife",parallel = T,
                              seed=seed_number)
  
  
  oregon_mc_22_fect3 <-  fect(od_death_rate_half ~ treatment_oregon + fent_all_opioid_share,
                              data = dat[Year<=endyr & Year>=startyr &  ST != "WA"],
                              index = c("ST","trend"), 
                              CV = TRUE, method = "both", force = "two-way",
                              se=T, vartype = "jackknife",parallel = T,
                              seed=seed_number)
  
  oregon_mc_22_fect4 <-  fect(od_death_rate_half ~ treatment_oregon,
                              data = dat[Year<=endyr & Year>=startyr &  ST != "WA" & ChangePoint>=31],
                              index = c("ST","trend"), 
                              CV = TRUE, method = "both", force = "two-way",
                              se=T, vartype = "jackknife",parallel = T,
                              seed=seed_number)
  
  
  
  reg_est_att = rbind(
    oregon_mc_nocontrol_22_fect$est.avg %>% as.data.frame() %>% mutate(control="No", rmse=oregon_mc_nocontrol_22_fect$rmse),
    oregon_mc_22_fect$est.avg %>% as.data.frame() %>% mutate(control="Fent Rate", rmse=oregon_mc_22_fect$rmse),
    oregon_mc_22_fect2$est.avg %>% as.data.frame() %>% mutate(control="Fent Count", rmse=oregon_mc_22_fect2$rmse),
    oregon_mc_22_fect3$est.avg %>% as.data.frame() %>% mutate(control="Fent Ratio", rmse=oregon_mc_22_fect3$rmse),
    oregon_mc_22_fect4$est.avg %>% as.data.frame() %>% mutate(control="2019Sample", rmse=oregon_mc_22_fect3$rmse)
  ) %>% 
    mutate(samplestart = startyr, sampleend = endyr) %>%
    bind_rows(reg_est_att)
  
  reg_est_es = rbind(
    oregon_mc_nocontrol_22_fect$est.att %>% as.data.frame() %>% mutate(control="No", rmse=oregon_mc_nocontrol_22_fect$rmse),
    oregon_mc_22_fect$est.att %>% as.data.frame() %>% mutate(control="Fent Rate", rmse=oregon_mc_22_fect$rmse),
    oregon_mc_22_fect2$est.att %>% as.data.frame() %>% mutate(control="Fent Count", rmse=oregon_mc_22_fect2$rmse),
    oregon_mc_22_fect3$est.att %>% as.data.frame() %>% mutate(control="Fent Ratio", rmse=oregon_mc_22_fect3$rmse),
    oregon_mc_22_fect4$est.att %>% as.data.frame() %>% mutate(control="2019Sample", rmse=oregon_mc_22_fect3$rmse)
    
  ) %>% 
    mutate(samplestart = startyr, sampleend = endyr) %>%
    bind_rows(reg_est_es)
  
  }
}

save(reg_est_att, reg_est_es, file="Estimate/Zoorob/Gsynth_Controls_Replication_Oregon.RData")




###################### Replication -- Washington ##########################
reg_est_att = tibble()
reg_est_es = tibble()

for (startyr in c(2008, 2014)) {
  for (endyr in c(2022, 2023)) {
  oregon_mc_nocontrol_22_fect <-  fect(od_death_rate_half ~ treatment_washington,
                                       data = dat[Year<=endyr & Year>=startyr & ST != "OR"],
                                       index = c("ST","trend"), 
                                       CV = TRUE, method = "both", force = "two-way",
                                       se=T, vartype = "jackknife",parallel = T,
                                       seed=seed_number)
  
  oregon_mc_22_fect <-  fect(od_death_rate_half ~ treatment_washington + nflis_fentanyl_percent_total,
                             data = dat[Year<=endyr & Year>=startyr &  ST != "OR"],
                             index = c("ST","trend"), 
                             CV = TRUE, method = "both", force = "two-way",
                             se=T, vartype = "jackknife",parallel = T,
                             seed=seed_number)
  
  oregon_mc_22_fect2 <-  fect(od_death_rate_half ~ treatment_washington + fent_rate_half,
                              data = dat[Year<=endyr & Year>=startyr &  ST != "OR"],
                              index = c("ST","trend"), 
                              CV = TRUE, method = "both", force = "two-way",
                              se=T, vartype = "jackknife",parallel = T,
                              seed=seed_number)
  
  
  oregon_mc_22_fect3 <-  fect(od_death_rate_half ~ treatment_washington + fent_all_opioid_share,
                              data = dat[Year<=endyr & Year>=startyr &  ST != "OR"],
                              index = c("ST","trend"), 
                              CV = TRUE, method = "both", force = "two-way",
                              se=T, vartype = "jackknife",parallel = T,
                              seed=seed_number)
  
  oregon_mc_22_fect4 <-  fect(od_death_rate_half ~ treatment_washington,
                              data = dat[Year<=endyr & Year>=startyr &  ST != "OR" & ChangePoint>=31],
                              index = c("ST","trend"), 
                              CV = TRUE, method = "both", force = "two-way",
                              se=T, vartype = "jackknife",parallel = T,
                              seed=seed_number)
  
  
  reg_est_att = rbind(
    oregon_mc_nocontrol_22_fect$est.avg %>% as.data.frame() %>% mutate(control="No", rmse=oregon_mc_nocontrol_22_fect$rmse),
    oregon_mc_22_fect$est.avg %>% as.data.frame() %>% mutate(control="Fent Rate", rmse=oregon_mc_22_fect$rmse),
    oregon_mc_22_fect2$est.avg %>% as.data.frame() %>% mutate(control="Fent Count", rmse=oregon_mc_22_fect2$rmse),
    oregon_mc_22_fect3$est.avg %>% as.data.frame() %>% mutate(control="Fent Ratio", rmse=oregon_mc_22_fect3$rmse),
    oregon_mc_22_fect4$est.avg %>% as.data.frame() %>% mutate(control="2019Sample", rmse=oregon_mc_22_fect3$rmse)
  ) %>% 
    mutate(samplestart = startyr, sampleend = endyr) %>%
    bind_rows(reg_est_att)
  
  reg_est_es = rbind(
    oregon_mc_nocontrol_22_fect$est.att %>% as.data.frame() %>% mutate(control="No", rmse=oregon_mc_nocontrol_22_fect$rmse),
    oregon_mc_22_fect$est.att %>% as.data.frame() %>% mutate(control="Fent Rate", rmse=oregon_mc_22_fect$rmse),
    oregon_mc_22_fect2$est.att %>% as.data.frame() %>% mutate(control="Fent Count", rmse=oregon_mc_22_fect2$rmse),
    oregon_mc_22_fect3$est.att %>% as.data.frame() %>% mutate(control="Fent Ratio", rmse=oregon_mc_22_fect3$rmse),
    oregon_mc_22_fect4$est.att %>% as.data.frame() %>% mutate(control="2019Sample", rmse=oregon_mc_22_fect3$rmse)
  ) %>% 
    mutate(samplestart = startyr, sampleend = endyr) %>%
    bind_rows(reg_est_es)
  
}
}

save(reg_est_att, reg_est_es, file="Estimate/Zoorob/Gsynth_Controls_Replication_Washington.RData")



