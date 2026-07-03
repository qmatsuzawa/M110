######### Create Appendix NFILS SC Table ##########
library(pacman)
p_load(readstata13, tidyverse, magrittr, usmap, kableExtra)
theme_set(theme_classic())

## Create fips => name
crosswalk = tibble(state = state.abb, `_Co_Number`=as.numeric(fips(state)))

########### Define Functions ##################
create_output = function(state, y) {
  
  if (y =="drug_pc") ylab = "Drugs Seized per 100,000"
  if (y =="fent_pc") ylab = "Fentanyl Seized per 100,000"
  if (y =="nonfent_pc") ylab = "Non-Fentanyl Seized per 100,000"
  if (y =="opioid_pc") ylab = "Non-Fentanyl Opioids Seized per 100,000"
  if (y =="fent_pct") ylab = "% Fentanyl to All Drugs"
  if (y =="fent_all_opioid_share") ylab = "% Fentanyl to Opioid "
  
  ### Load Data
  main = read.dta13(paste0("Estimate/Synth/NFIL/", "Synth_", y, "_", state, ".dta"))
  colnames(main)[c(3,4,5)] = c("y", "y_synth", "time")
  placebos = read_csv(paste0("Estimate/Synth/NFIL/", "Placebos_", y, "_", state, ".csv"),
                      show_col_types = FALSE)
  colnames(placebos)[c(4,5,6)] = c("y", "y_synth", "time")
  
  main %<>% filter(!is.na(time))
  
  ### Figures
  ggplot(main, aes(x=time)) +
    geom_line(aes(y=y, color="Actual"), linewidth=1.7) + 
    geom_line(aes(y=y_synth, color="Synthetic"), linetype="dashed", linewidth=1.7) + 
    scale_color_manual(values= c("red", "blue"),
                       name="") +
    scale_x_continuous(breaks=seq(3,23,4),
                       labels=seq(2012,2022,2)) +
    geom_vline(xintercept = 21, linetype="dashed", linewidth=1.3) +
    labs(x="Time", y=paste0(ylab)) +
    theme(legend.position = "bottom",
          text = element_text(size=20)) -> plot
  if (state==6) stname = "CA"
  if (state==41) stname = "OR"
  if (state==53) stname = "WA"
  ggsave(paste0("Figures/NFIL/", y, "_", stname, ".png"), plot, width=12, height=8)
  
  
  ### Placebos + ATT
  placebos %<>% 
    bind_rows(main %>% mutate(unit=state, trtime=21)) %>%
    mutate(error = (y-y_synth)^2,
           pre = ifelse(time < 21, error, NA),
           post = ifelse(time >= 21, error, NA)
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
    .$pval
  
  att = main %>%
    filter(time>=21) %>%
    mutate(diff = y - y_synth) %>%
    .$diff %>% 
    mean() 
  
  mean = main %>%
    filter(time>=21) %>%
    .$y_synth %>%
    mean() %>%
    round(2)
  
  att = round(att, 3)
  if (pval < 0.1) att = paste0(att, "*") 
  if (pval < 0.05) att = paste0(att, "*") 
  if (pval < 0.01) att = paste0(att, "*") 
  
  # return(c(att, paste0("{", pval, "}"),  mean))
  return(c(att, paste0("{", pval, "}")))
}


table = cbind(
  c("ATT", "P-Val"),
  create_output(41, "fent_pc"),
  create_output(41, "nonfent_pc"),
  create_output(41, "opioid_pc"),
  create_output(41, "drug_pc"),
  create_output(41, "fent_pct"),
  create_output(41, "fent_all_opioid_share"),
  create_output(53, "fent_pc"),
  create_output(53, "nonfent_pc"),
  create_output(53, "opioid_pc"),
  create_output(53, "drug_pc"),
  create_output(53, "fent_pct"),
  create_output(53, "fent_all_opioid_share")
)

table = rbind(
  c("","Fentanyl", "Non Fentanyl", "Opioid", "All Drugs", "Fentanyl-to-Drug", "Fentanyl-to-Opioid"),
  c("", paste0("(", c(1:6), ")")),
  table[, c(1:7)],
  table[, c(1, 8:13)]
) 
table %<>%  as.data.frame() 

# table[3,1] = "\\hline \\\\ & \\multicolumn{6}{c}{Panel I: Oregon} \\\\ \\cmidrule(lr){2-7} ATT"
# table[5,1] = "\\hline \\\\ & \\multicolumn{6}{c}{Panel II: Washington} \\\\ \\cmidrule(lr){2-7} ATT"
# table %<>% mutate(across(V2:V7, ~str_replace_all(., c("\\{"="\\\\{", "\\}"="\\\\}"))))
# table = convert_tex(table, title="\\label{tab:nfils} Synthetic Control Estimate Drugs Seized, NFILS")
# writeLines(table, "Estimate/Tables/NFILS_Seizure.tex")

write_csv(table, "Estimate/Tables/NFILS_Seizure.csv")