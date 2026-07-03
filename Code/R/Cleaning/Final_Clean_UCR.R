########## Finalize UCR Data ############
library(pacman)
p_load(tidyverse, fastverse, fixest, broom, arrow, usmap)

data = read_parquet("Data/Cleaned/Crime/M110_Drugs.parquet")
data %<>%
  filter(year %in% c(2018:2024)) %>%
  filter(str_detect(offense_code, "drug possess") & 
           offense_code!="drug possess - drug total" &
           offense_code!="drug possess - marijuana") %>%
  mutate(ori9 = ifelse(is.na(ori9), paste0(ori,"00"), ori9)) %>%
  group_by(ori9) %>%
  mutate(n=n())

data$offense_code %>% table()

data %<>%
  filter(n==7*12*3) %>%
  group_by(state, year, monthno) %>%
  summarize(pop=sum(population/3),
            drug_all=sum(total_arrests),
            drug_synthetic=sum(ifelse(offense_code=="drug possess - synthetic - narcotics", total_arrests,0)),
            drug_other=sum(ifelse(offense_code=="drug possess - other drug", total_arrests,0)),
            drug_opium_coke=sum(ifelse(offense_code=="drug possess - opium and cocaine and derivatives including heroin", total_arrests,0)),
            )

data %<>% mutate(state_fips = as.numeric(fips(state)))

write_csv(data, "Data/Cleaned/Crime/UCR_Total_Drug.csv")