######## Create Table 2: Sensitivity to Fentanyl Control ###########
library(pacman)
p_load(readstata13, tidyverse, magrittr, usmap, kableExtra)
theme_set(theme_classic())

## Create fips => name
crosswalk = tibble(state = state.abb, `_Co_Number`=as.numeric(fips(state)))

########### Define Functions ##################
create_output = function(state, start, end, dt, contvar=NA) {
  ### Load Data
  treat = 36 + start
  if (!is.na(contvar)) {
    main = read.dta13(paste0("Estimate/SC_Main/Robustness/Synth_", dt, "_", state, "_", contvar, ".dta"))
  } else {
    main = read.dta13(paste0("Estimate/SC_Main/ReplicationExtention/Synth_", dt, "_", state, "_start", start, ".dta"))
  }
  colnames(main)[c(3,4,5)] = c("y", "y_synth", "time")
  main %<>% filter(time <= end)
  if (!is.na(contvar)) {
    placebos = read_csv(paste0("Estimate/SC_Main/Robustness/Placebos_", dt, "_", state, "_", contvar, ".csv"),
                        show_col_types = FALSE)
    } else {
      placebos = read_csv(paste0("Estimate/SC_Main/ReplicationExtention/Placebos_", dt, state, "_start", start, ".csv"),
                          show_col_types = FALSE)
  }

  colnames(placebos)[c(4,5,6)] = c("y", "y_synth", "time")
  placebos %<>% filter(time <= end)
  
  ### Placebos + ATT
  placebos %<>% 
    bind_rows(main %>% mutate(unit=state, trtime=treat)) %>%
    mutate(error = (y-y_synth)^2,
           pre = ifelse(time < treat, error, NA),
           post = ifelse(time >= treat, error, NA)
    ) %>% 
    group_by(unit) %>%
    summarize(pre=mean(pre, na.rm=T),
              post=mean(post, na.rm=T)) %>% 
    mutate(rate=post/pre) %>%
    arrange(desc(rate)) %>%
    mutate(n=row_number(),
           pval = n/nrow(.))
  
  
  pval = placebos %>%
    filter(unit==state) %>%
    .$pval %>%
    round(3)
  
  att = main %>%
    filter(time>=treat) %>%
    mutate(diff = y - y_synth) %>%
    .$diff %>% 
    mean() 
  
  att = att*12
  att = round(att, 3)
  if (pval < 0.1) att = paste0(att, "*") 
  if (pval < 0.05) att = paste0(att, "*") 
  if (pval < 0.01) att = paste0(att, "*") 
  
  return(c(att, paste0("{", pval, "}")))
}


######## Create Tables ##########

### Panels I & II
load("Estimate/Zoorob/Gsynth_Controls_Replication_Washington.RData")
main = reg_est_att %>% mutate(st="WA")
load("Estimate/Zoorob/Gsynth_Controls_Replication_Oregon.RData")
main = reg_est_att %>% mutate(st="OR")  %>% bind_rows(main)

main1 = main %>% 
  filter(samplestart==2008 & sampleend==2022) %>%
  mutate(att = round(ATT.avg*2, 3),
         att = ifelse(p.value >= 0.1, att,
                      ifelse(p.value >=0.05, paste0(att, "*"),
                             ifelse(p.value >=0.01, paste0(att, "**"), paste0(att, "***") ))),
         pval =format(round(p.value,3),3),
         pval = paste0("{", pval, "}"))
panel1 = rbind(c("\\hline \\\\ & \\multicolumn{10}{c}{Panel I: Matrix Completion SC Using Biannual Data, 2008-2022} \\\\ \\cmidrule(lr){2-11} ATT", main1$att), 
               c("P-Val", main1$pval))

main2 = main %>% 
  filter(samplestart==2008 & sampleend==2023) %>%
  mutate(att = round(ATT.avg*2, 3),
         att = ifelse(p.value >= 0.1, att,
                      ifelse(p.value >=0.5, paste0(att, "*"),
                             ifelse(p.value >=0.01, paste0(att, "**"), paste0(att, "***") ))),
         pval = format(round(p.value,3),3),
         pval = paste0("{", pval, "}"))
panel2 = rbind(c("\\\\ & \\multicolumn{10}{c}{Panel II: Matrix Completion SC Using Biannual Data, 2008-2023} \\\\ \\cmidrule(lr){2-11} ATT", main2$att), 
               c("P-Val", main2$pval))


panel3 = cbind(c("\\\\ & \\multicolumn{10}{c}{Panel III: Traditional SC Using Monthly Data, 2018-2023} \\\\ \\cmidrule(lr){2-11} ATT", 
                 "P-Val"),
               create_output(41, 2, 72, "joshi"),
               create_output(41, 2, 72, "joshi", "Control_fent_pct"),
               create_output(41, 2, 72, "joshi", "Control_fent_pc"),
               create_output(41, 2, 72, "joshi", "Control_fent_ratio"),
               create_output(41, 2, 72, "joshi", "Breakpoint_31"),
               create_output(53, 3, 72, "joshi"),
               create_output(53, 3, 72, "joshi", "Control_fent_pct"),
               create_output(53, 3, 72, "joshi", "Control_fent_pc"),
               create_output(53, 3, 72, "joshi", "Control_fent_ratio"),
               create_output(53, 3, 72, "joshi", "Breakpoint_31"))

table = rbind(
  c(" & \\multicolumn{5}{c}{Oregon} & \\multicolumn{5}{c}{Washington} \\\\ \\cmidrule(lr){2-6} \\cmidrule(lr){7-11}", 
    paste0("(", c(1:10), ")")),
  panel1, panel2, panel3, 
  c("\\\\ Fentanyl Control", rep(c("None", "DrugRatio", "PC", "OpioidRatio", "None"),2)),
  c("Donor Pool Restriction?", rep(c("No", "No", "No", "No", "Yes"),2))
) %>%
  as.data.frame()

# table %<>% 
#   mutate(across(V2:V11, ~str_replace_all(., c("\\{"="\\\\{", "\\}"="\\\\}"))))
# table = convert_tex(table, title="\\label{tab:zoorobrep} Exploring Sensitivity to Alternative Fentanyl Controls") %>%
#   kable_styling(latex_options = "scale_down")
# writeLines(table, "Estimate/Tables/Zoorob_Replication.tex")
write_csv(table, "Estimate/Tables/Zoorob_Replication.csv")






######## For Appendix ##########

### Panels I & II
load("Estimate/Zoorob/Gsynth_Controls_Replication_Washington.RData")
main = reg_est_att %>% mutate(st="WA")
load("Estimate/Zoorob/Gsynth_Controls_Replication_Oregon.RData")
main = reg_est_att %>% mutate(st="OR")  %>% bind_rows(main)

main1 = main %>% 
  filter(samplestart==2014 & sampleend==2022) %>%
  mutate(att = round(ATT.avg*2, 3),
         att = ifelse(p.value >= 0.1, att,
                      ifelse(p.value >=0.5, paste0(att, "*"),
                             ifelse(p.value >=0.01, paste0(att, "**"), paste0(att, "***") ))),
         pval = format(round(p.value,3),3),
         pval = paste0("{", pval, "}"))
panel1 = rbind(c("\\hline \\\\ & \\multicolumn{10}{c}{Panel I: Matrix Completion SC Using Biannual Data, 2008-2022} \\\\ \\cmidrule(lr){2-11} ATT", main1$att), 
               c("P-Val", main1$pval))

main2 = main %>% 
  filter(samplestart==2014 & sampleend==2023) %>%
  mutate(att = round(ATT.avg*2, 3),
         att = ifelse(p.value >= 0.1, att,
                      ifelse(p.value >=0.5, paste0(att, "*"),
                             ifelse(p.value >=0.01, paste0(att, "**"), paste0(att, "***") ))),
         pval = sprintf("%.3f",round(p.value,3)),
         pval = paste0("{", pval, "}"))
panel2 = rbind(c("\\\\ & \\multicolumn{10}{c}{Panel II:  Matrix Completion SC Using Biannual Data, 2008-2023} \\\\ \\cmidrule(lr){2-11} ATT", main2$att), 
               c("P-Val", main2$pval))


table = rbind(
  c(" & \\multicolumn{5}{c}{Oregon} & \\multicolumn{5}{c}{Washington} \\\\ \\cmidrule(lr){2-6} \\cmidrule(lr){7-11}", 
    paste0("(", c(1:10), ")")),
  panel1, panel2, 
  c("\\\\ Fentanyl Control", rep(c("None", "DrugRatio", "PC", "OpioidRatio", "None"),2)),
  c("Donor Pool Restriction?", rep(c("No", "No", "No", "No", "Yes"),2))
) %>%
  as.data.frame()

# table %<>% 
#   mutate(across(V2:V11, ~str_replace_all(., c("\\{"="\\\\{", "\\}"="\\\\}"))))
# table = convert_tex(table, title="\\label{tab:zoorobrep_appendix}  Exploring Sensitivity to Alternative Fentanyl Controls -- Starting Sample in 2014") %>%
#   kable_styling(latex_options = "scale_down")
# writeLines(table, "Estimate/Tables/Zoorob_Replication_Appendix.tex")
write_csv(table, "Estimate/Tables/Zoorob_Replication_Appendix.csv")

