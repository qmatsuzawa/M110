######### Finalize Appendix Table for Alternative Estimators #############
library(pacman)
p_load(tidyverse, broom, magrittr, kableExtra)


last = 72
get_coeff_gsynth = function(y, treat, trtime2, m) {
  load(file="Estimate/Robustness/GSynth.RData")
  reg= reg_est %>% filter(outcome==y & treatedunit==treat, trtime==trtime2, model==m)
  att = reg$ATT.avg 
  pval = reg$p.value
  
  att = format(round(att, 3),3)
  if (pval < 0.1) att = paste0(att, "*") 
  if (pval < 0.05) att = paste0(att, "*")
  if (pval < 0.01) att = paste0(att, "*") 
  pval = sprintf("%.3f",round(pval,3))
  
  return(c(att, paste0("{", pval, "}")))
}


get_coeff_aug = function(y, treat, trtime2) {

  load("Estimate/Robustness/AugSynth_Main.RData")
  reg= reg_est %>% filter(outcome==y & treatedunit==treat, trtime==trtime2)
  att = reg$Estimate 
  pval = reg$p_val
  
  att = format(round(att, 3),3)
  if (pval < 0.1) att = paste0(att, "*") 
  if (pval < 0.05) att = paste0(att, "*")
  if (pval < 0.01) att = paste0(att, "*") 
  pval = format(round(pval, 3),3)
  
  return(c(att, paste0("{", pval, "}")))
}

get_coeff_sdid = function(y, treat, trtime2) {
  
  est = readRDS("Estimate/Robustness/SDiD_Main.rds") %>%
    filter(treatedunit==treat & trtime==trtime2 &  outcome==y )
  placebo_wts =  readRDS("Estimate/Robustness/SDiD_Placebos.rds") %>%
    filter(trtime==trtime2 & outcome==y)
  
  att =  est$beta
  se = sd(placebo_wts$beta)
  pval = pnorm(abs(att/se), lower.tail = F) * 2
  pval = format(round(pval, 3),3)
  att = format(round(att, 3),3)
  
  if (pval < 0.1) att = paste0(att, "*") 
  if (pval < 0.05) att = paste0(att, "*")
  if (pval < 0.01) att = paste0(att, "*") 
  
  return(c(att, paste0("{", pval, "}")))
}

get_coeff_did = function(y, treat, trtime2,  weighted) {
  load(file="Estimate/Robustness/DiD_Main.RData")
  
  if (weighted==T) main = main_w
  if (weighted==F) main = main_uw
  main %<>% filter(outcome==y & treatedunit==treat & trtime==trtime2)
  main %<>% arrange(desc(abs(estimate)))
  att =  main %>% filter(placebounit==treat) %>% .$estimate 
  se = main %>% filter(placebounit!=treat) %>% .$estimate
  se = sd(se)
  pval = pnorm(abs(att/se), lower.tail = F) * 2
  #pval = main %>% mutate(pval=row_number()/n()) %>% filter(placebounit==treat) %>% .$pval
  pval = round(pval, 3)
  att = format(round(att, 3),3)
  
  if (pval < 0.1) att = paste0(att, "*") 
  if (pval < 0.05) att = paste0(att, "*")
  if (pval < 0.01) att = paste0(att, "*") 
  pval = format(round(pval, 3),3)
  
  return(c(att, paste0("{", pval, "}")))
}

get_coeff_multy = function(y, treat, trtime2) {
  
  load("Estimate/Robustness/AugSynth_MultipleOutcome.RData")
  reg= reg_est %>% filter(Outcome==y & treatedunit==treat, trtime==trtime2)
  att = reg$Estimate 
  pval = reg$p_val
  
  att = format(round(att, 3),3)
  if (pval < 0.1) att = paste0(att, "*") 
  if (pval < 0.05) att = paste0(att, "*")
  if (pval < 0.01) att = paste0(att, "*") 
  
  pval = format(round(pval, 3),3)
  return(c(att, paste0("{", pval, "}")))
}

table1 = cbind(c("ATT", "P-Val"),
              get_coeff_aug("joshi", 41, 2),
              get_coeff_sdid("joshi", 41, 2),
              get_coeff_gsynth("joshi", 41, 2, "GSynth"),
              get_coeff_gsynth("joshi", 41, 2, "Factor Completion"),
              get_coeff_multy("joshi", 41, 2),
              get_coeff_did("joshi", 41, 2, T)
      )

table2 = cbind(c("ATT", "P-Val"),
               get_coeff_aug("joshi", 53, 3),
               get_coeff_sdid("joshi", 53, 3),
               get_coeff_gsynth("joshi", 53, 3, "GSynth"),
               get_coeff_gsynth("joshi", 53, 3, "Factor Completion"),
               get_coeff_multy("joshi", 53, 3),
               get_coeff_did("joshi", 53, 3, T)
)

table = rbind(
  c("", paste0("(", c(1:6), ")")),
  c("", "Aug Synth", "SDiD", "GSynth", "Matrix Completion", "Multi Outcome SC", "TWFE"),
  table1,
  table2
) %>% as.data.frame()


# table %<>% mutate(across(V2:V7, ~str_replace_all(., c("\\{"="\\\\{", "\\}"="\\\\}"))))
# table[3,1] = " \\hline & \\multicolumn{6}{c}{Panel A: Oregon}  \\\\ \\cmidrule(l){2-7} ATT Overall"
# table[5,1] = " & \\multicolumn{6}{c}{Panel B: Washington} \\\\ \\cmidrule(l){2-7} ATT Overall"
# 
# table = convert_tex(table, title="\\label{tab:robustmethod} Robustness: Other Estimator")
# writeLines(table, "Estimate/Tables/OtherMethod.tex")

write_csv(table, "Estimate/Tables/OtherMethod.csv")
