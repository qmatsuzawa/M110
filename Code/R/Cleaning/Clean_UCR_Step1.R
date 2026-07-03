####### Clean UCR Data: Measure 110 ########
library(pacman)
p_load(tidyverse, fastverse, arrow, parallel)

months = tibble(month=str_to_lower(month.name),
                monthno = c(1:12)) %>%
  as.data.table()

yr = 2022

get_ucr = function(yr) {
  data = read_parquet(paste0("Raw/UCR/arrests_parquet_1974_2024_month/",  
                             "arrests_monthly_", yr, ".parquet"))
  data %<>%
    as.data.table() %>%
    .[number_of_months_reported==12] %>% ### Keep agencies that report constantly
    .[,.(ori9, ori, year, month, state, number_of_months_reported, 
         population, agency_type, fips_state_county_code, 
         offense_code, total_arrests)] %>%
    .[str_detect(offense_code, "drug")] %>%
    merge(., months, by="month")
  
  return(data)
}

data = mclapply(c(2018:2024), get_ucr)
data %<>% rbindlist()
write_parquet(data, "Cleaned/UCR/M110_Drugs.parquet")

  