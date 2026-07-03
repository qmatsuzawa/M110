########### Clean NVSS Mortality Data: Public-Use Version #########
library(pacman)
p_load(tidyverse, tidycensus, magrittr, data.table, usmap, lubridate)

#### Get Pop Data
acs = readRDS(file="Data/OtherData/ACS.rds")

### Create Balanced Panel
data = expand.grid(state_fips=c(as.numeric(fips(state.name)),11),
                   year=c(2018:2023),
                   month=c(1:12)
                   ) %>%
  mutate(time = (year-2018)*12 + month)


### Load + Clean + Merge (w/ pop) JAMA Data
set.seed(12345)
jama = fread("Data/Raw/Jama_v2.txt", fill=TRUE)
jama %<>%
  mutate(state_fips = as.numeric(fips(`Occurrence State`)),
         `Month Code` = paste0(`Month Code`, "/01"),
         yrmo = as.Date(`Month Code`, "%Y/%m/%d"),
         year = year(yrmo),
         month = month(yrmo),
         joshi = as.numeric(Deaths),
  ) %>%
  select(c("state_fips", "year", "month", "joshi"))


### Load + Clean + Merge (w/ pop) Spencer Data
spencer = fread("Data/Raw/Spencer_v2.txt", fill=TRUE)
spencer %<>%
  mutate(state_fips = as.numeric(fips(`Occurrence State`)),
         `Month Code` = paste0(`Month Code`, "/01"),
         yrmo = as.Date(`Month Code`, "%Y/%m/%d"),
         year = year(yrmo),
         month = month(yrmo),
         spencer = as.numeric(Deaths),
  ) %>%
  select(c("state_fips", "year", "month", "spencer"))

#### Merge Data
data %<>% 
  left_join(jama, by=c("state_fips", "year", "month")) %>%
  left_join(spencer, by=c("state_fips", "year", "month"))

#### Create Random Imputed Value
data %<>% 
  mutate(random_j = sample(c(1:9), nrow(.), replace=TRUE),
         random_s = sample(c(1:9), nrow(.), replace=TRUE),
         random_j = ifelse(random_s > random_j, random_s, random_j), ### Make sure joshi >= spencer
         joshi = ifelse(is.na(joshi), random_j, joshi),
         spencer = ifelse(is.na(spencer), random_s, spencer)
         ) %>%
  select(-c("random_j", "random_s"))

### Merge Pop
data %<>% 
  left_join(acs, by=c("state_fips", "year")) 


### Save
fwrite(data, "Data/Cleaned/NVSS_Mortality_MainRegReady.csv")
