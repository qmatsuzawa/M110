############### Create Appendix Table: Robustness to COVID-19 ###################
library(pacman)
p_load(readstata13, tidyverse, magrittr, usmap, kableExtra)
theme_set(theme_classic())

## Create fips => name
crosswalk = tibble(state = state.abb, `_Co_Number`=as.numeric(fips(state)))

########### Define Functions ##################
create_output = function(state, start, end, dt) {
  ### Load Data
  treat = 36 + start
  main = read.dta13(paste0("Estimate/SC_Main/Robustness/Synth_", dt, "_", state, "_Control_COVID.dta"))
  colnames(main)[c(3,4,5)] = c("y", "y_synth", "time")
  main %<>% filter(time <= end)
  placebos = read_csv(paste0("Estimate/SC_Main/Robustness/Placebos_", dt, "_", state, "_COVID.csv"),
                      show_col_types = FALSE)
  colnames(placebos)[c(4,5,6)] = c("y", "y_synth", "time")
  placebos %<>% filter(time <= end)
  
  ### Placebos + ATT
  placebos %<>% 
    bind_rows(main %>% mutate(unit=state, trtime=treat)) %>%
    mutate(error = (y-y_synth)^2,
           pre = ifelse(time < treat, error, NA),
           post = ifelse(time >= treat, error, NA),
           att = ifelse(time >= treat, y-y_synth, NA),
    ) %>% 
    group_by(unit) %>%
    summarize(pre=mean(pre, na.rm=T),
              post=mean(post, na.rm=T),
              att=mean(att,na.rm=T)) %>% 
    mutate(rate=post/pre) %>%
    arrange(desc(rate)) %>%
    mutate(n=row_number(),
           pval = n/nrow(.))
  
  pval = placebos %>%
    filter(unit==state) %>%
    .$pval
  
  att = main %>%
    filter(time>=treat) %>%
    mutate(diff = y - y_synth) %>%
    .$diff %>% 
    mean() 
  
  att = round(att, 3)
  if (pval < 0.1) att = paste0(att, "*") 
  if (pval < 0.05) att = paste0(att, "*") 
  if (pval < 0.01) att = paste0(att, "*") 
  
  return(c(att, paste0("{", pval, "}")))
}




################  Table 1A. Rpelication: OR ####################
table = cbind(
  c("ATT", "P-Val"),
  create_output(41, 1, 71, "spencer"),
  create_output(41, 2, 71, "joshi"),
  create_output(53, 2, 71, "spencer"),
  create_output(53, 3, 71, "joshi")
)

table = rbind(
  c("", paste0("(", c(1:4), ")")),
  table,
  c("Data", rep(c("Spencer","Joshi"), 2))
) %>% as.data.frame()


# table %<>% mutate(across(V2:V5, ~str_replace_all(., c("\\{"="\\\\{", "\\}"="\\\\}"))))
# table[1,1] = " & \\multicolumn{2}{c}{Oregon} & \\multicolumn{2}{c}{Washington}  \\\\ \\cmidrule(l){2-3}  \\cmidrule(l){4-5}" 
# table[2,1] = paste0("\\hline ", table[2,1])
# table = convert_tex(table, title="\\label{tab:nocovid} Synthetic Control Estimate Excluding COVID-19")
# writeLines(table, "Estimate/Tables/Robustness_COVID.tex")
write_csv(table, "Estimate/Tables/Robustness_COVID.csv")