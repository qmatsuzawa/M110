########## Create Appendix Table: SC w/ Confidence Intervals ##########
library(pacman)
p_load(readstata13, tidyverse, magrittr, usmap, kableExtra)
theme_set(theme_classic())

## Create fips => name
crosswalk = tibble(state = state.abb, `_Co_Number`=as.numeric(fips(state)))

## Get Pre-Treatment SD
data = read_csv("Data/Cleaned/NVSS_Mortality_MainRegReady.csv") %>%
  mutate(joshi = joshi/pop*100000,
         spencer=spencer/pop*100000) %>%
  filter(year < 2021 ) %>%
  group_by(state_fips) %>%
  summarize(spencer = sd(spencer),
            joshi =sd(joshi))

### Load SC T-Test Result
load("Estimate/SC_Main/Robustness/SC_TTest.RData")

### Load SCPI Result
load(file="Estimate/SC_Main/Robustness/SCPI.RData")

########### Define Functions ##################

create_output_ttest  = function(state, starttime, endtime, dtname, Kno) {
  result = sc_ttest %>% 
    filter(treated==state & start==starttime & end==endtime & dt==dtname & K==Kno)
  
  lower = result$lb %>% round(2)
  upper = result$ub %>% round(2)
  att = result$att
  sd = result$se

  pval = pt(abs(att/sd), df=Kno-1, lower.tail = F)*2
  att = round(att, 3)
  if (pval < 0.1) att = paste0(att, "*") 
  if (pval < 0.05) att = paste0(att, "*") 
  if (pval < 0.01) att = paste0(att, "*") 
  pval = round(pval,3)
  
  return(c(att, paste0("[", lower, ",", upper, "]")))
}

create_output_scpi  = function(state, starttime, endtime, dtname) {
  result = scpi_result %>%
    filter(treatunit==state & start==starttime & outcome==dtname)
  
  result %<>% 
    filter(time <= endtime) %>%
    group_by(pval) %>%
    summarize(att=mean(att),
              lb=mean(cilow),
              ub=mean(cihigh)) %>% 
    ungroup() %>%
    mutate(p = ifelse(pval==5 & (lb>0 | ub<0),1,
                      ifelse(pval==10 & (lb>0 | ub<0), 1,0)),
           p = sum(p),
           p = ifelse(p==2, 0.02, ifelse(p==1, 0.07, 0.15))) %>%
    filter(pval==5)
  
  lower = result$lb %>% round(2)
  upper = result$ub %>% round(2)
  att = result$att
  pval = result$p
  att = round(att, 3)
  if (pval < 0.1) att = paste0(att, "*") 
  if (pval < 0.05) att = paste0(att, "*") 
  if (pval < 0.01) att = paste0(att, "*") 
  pval = round(pval,3)
  
  return(c(att, paste0("[", lower, ",", upper, "]")))
}

################  Table 1A. Replication ####################
table1 = cbind(
  c("ATT", ""),
  create_output_scpi(41, 1, 48, "spencer"),
  create_output_scpi(41, 2, 51, "joshi"),
  create_output_scpi(53, 2, 48, "spencer"),
  create_output_scpi(53, 3, 51, "joshi")
)


table2 = cbind(
    c("ATT", ""),
    create_output_ttest(41, 1, 48, "spencer",3),
    create_output_ttest(41, 2, 51, "joshi", 3),
    create_output_ttest(53, 2, 48, "spencer",3),
    create_output_ttest(53, 3, 51, "joshi",3)
  )

table = rbind(
  table1, 
  table2,
  c("Specification", rep(c("Spencer", "Joshi"), 2))
) %>% as.data.frame()


table[1,1] = "& \\multicolumn{2}{c}{Oregon} & \\multicolumn{2}{c}{Washington} \\\\ \\cmidrule(lr){2-3} \\cmidrule(lr){4-5}  \\\\ & \\multicolumn{4}{c}{Panel I: Prediction Intervals}  \\\\ \\cmidrule(l){2-5} ATT"
table[3,1] = " \\\\ & \\multicolumn{4}{c}{Panel II: Chernozhukov et al. (2018) T-Test Procedure}  \\\\ \\cmidrule(l){2-5} ATT"
# table = convert_tex(table, title="\\label{tab:ttestreplication} Synthetic Control Estimates with Prediction \\& Confidence Intervals,  Replication")
# writeLines(table, "Estimate/Tables/Replication_TTest.tex")
write_csv(table, "Estimate/Tables/Replication_TTest.csv")



##############################
table1 = cbind(
  c("ATT", ""),
  create_output_scpi(41, 1, 72, "spencer"),
  create_output_scpi(41, 2, 72, "joshi"),
  create_output_scpi(53, 2, 72, "spencer"),
  create_output_scpi(53, 3, 72, "joshi")
)


table2 = cbind(
  c("ATT", ""),
  create_output_ttest(41, 1, 72, "spencer",3),
  create_output_ttest(41, 2, 72, "joshi", 3),
  create_output_ttest(53, 2, 72, "spencer",3),
  create_output_ttest(53, 3, 72, "joshi",3)
)

table = rbind(
  table1, 
  table2,
  c("Specification", rep(c("Spencer", "Joshi"), 2))
) %>% as.data.frame()


table[1,1] = "& \\multicolumn{2}{c}{Oregon} & \\multicolumn{2}{c}{Washington} \\\\ \\cmidrule(lr){2-3} \\cmidrule(lr){4-5}  \\\\ & \\multicolumn{4}{c}{Panel I: Prediction Intervals}  \\\\ \\cmidrule(l){2-5} ATT"
table[3,1] = " \\\\ & \\multicolumn{4}{c}{Panel II: Chernozhukov et al. (2018) T-Test Procedure}  \\\\ \\cmidrule(l){2-5} ATT"
# table = convert_tex(table, title="\\label{tab:ttestextension} Synthetic Control Estimates with Prediction \\& Confidence Intervals, Full Period")
# writeLines(table, "Estimate/Tables/Extension_TTest.tex")
write_csv(table, "Estimate/Tables/Extension_TTest.csv")


