########### Clean NVSS Mortality Data: Restricted-Use Version Step 2 #########
library(pacman)
p_load(tidyverse, tidycensus, magrittr, data.table, usmap, lubridate)

rm(list=ls())

#### Get Pop Data
acs = readRDS(file="Data/OtherData/ACS.rds")
pop = fread("SpencerCode/project/build/input/pop/pop.txt", fill=TRUE)
pop %<>% 
  mutate(state_fips=`Residence State Code`,
         year = Year,
         pop_spencer = Population) %>%
  filter(!is.na(state_fips)) %>%
  select(state_fips, year, pop_spencer)

### Create Balanced Panel
balanced = tibble(state_fips=c(as.numeric(fips(state.name)),11),
                  id=1) %>%
  left_join(tibble(id=1, 
                   year=rep(c(2018:2023), each=12),
                   month=rep(c(1:12), 6),
                   yrmo = as.Date(paste0(year,"-", month, "-01"))),
            by = "id", relationship = "many-to-many") %>%
  select(-"id")


### Get Main Data
data = fread("Data/NCHSRestricted/Overdoses_20182023.csv")

### Merge Pop
data %<>% 
  left_join(acs, by=c("state_fips", "year")) %>%
  left_join(pop, by=c("state_fips", "year")) 

fwrite(data, "Data/Cleaned/NVSS_Mortality_MainRegReady.csv")


