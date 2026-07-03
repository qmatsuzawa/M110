############### Create Appendix Table: Placebo Test for West Coast ###################
library(pacman)
p_load(readstata13, tidyverse, magrittr, usmap)
theme_set(theme_classic())

## Create fips => name
crosswalk = tibble(state = state.abb, `_Co_Number`=as.numeric(fips(state)))

########### Define Functions ##################
create_output = function(start, end, dt) {
  ### Load Data
  treat = 36 + start
  placebos = read_csv(paste0("Estimate/SC_Main/Robustness/Placebos_", dt, "_West_start", start, ".csv"),
                      show_col_types = FALSE)
  colnames(placebos)[c(4,5,6)] = c("y", "y_synth", "time")
  placebos %<>% filter(time <= end)
  
  ### Placebos + ATT
  placebos %<>% 
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
  
  ## Filter West Coast States
  placebos %<>%  
    filter(unit %in% c(6, 16, 32)) %>%
    arrange(unit)
  
  placebos %<>% 
    mutate(att = round(att, 3),
           att = ifelse(pval >= 0.1, att,
                        ifelse(pval >=0.5, paste0(att, "*"),
                               ifelse(pval >=0.01, paste0(att, "**"), paste0(att, "***") ))),
           pval =round(pval,3))
  
  tab = rbind(c("CA", "ID", "NV"),
              placebos$att,
              paste0("{", placebos$pval, "}")
              )              
  return(tab)
}



################  Table 1A. Rpelication: OR ####################
table = cbind(
  c("", "\\hline ATT", "P-Val"),
  create_output(1, 71, "spencer"),
  create_output(1, 71, "joshi")
) %>%
  as.data.frame()


# table %<>% 
#   mutate(across(V2:V7, ~str_replace_all(., c("\\{"="\\\\{", "\\}"="\\\\}"))))
# table[1,1] = " & \\multicolumn{3}{c}{Spencer (2023)} & \\multicolumn{3}{c}{Joshi et al. (2023)} \\\\ \\cmidrule(l){2-4} \\cmidrule(l){5-7}"
# table = convert_tex(table, title="\\label{tab:placebo_west} Placebo Test: West Coast State")
# writeLines(table, "Estimate/Tables/Placebo_WestCoast.tex")
write_csv(table, "Estimate/Tables/Placebo_WestCoast.csv")
