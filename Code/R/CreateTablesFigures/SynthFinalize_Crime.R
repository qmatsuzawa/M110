############## Create Figure: UCR Arrest ##################
library(pacman)
p_load(readstata13, tidyverse, magrittr, usmap)
theme_set(theme_classic())

## Create fips => name
crosswalk = tibble(state = state.abb, `_Co_Number`=as.numeric(fips(state)))

########### Define Functions ##################
create_output = function(state, start, y) {
  ### Load Data
  treat = 36 + start
  main = read.dta13(paste0("Estimate/SC_Main/Crime/Synth_", y, "_", state, "_start", start, ".dta"))
  colnames(main)[c(3,4,5)] = c("y", "y_synth", "time")
  placebos = read_csv(paste0("Estimate/SC_Main/Crime/Placebos_", y, "_", state, "_start", start, ".csv"),
                      show_col_types = FALSE)
  colnames(placebos)[c(4,5,6)] = c("y", "y_synth", "time")
  
  ### Figures
  ggplot(main, aes(x=time)) +
    geom_line(aes(y=y, color="Actual"), linewidth=1.7) + 
    geom_line(aes(y=y_synth, color="Synthetic"), linetype="dashed", linewidth=1.7) + 
    scale_color_manual(values= c("red", "blue"),
                       name="") +
    scale_x_continuous(breaks = c(1,13,25,37,49,61),
                       labels = c(2018:2023)) +
    geom_vline(xintercept = treat, linetype="dashed", linewidth=1.3) +
    labs(x="Time", y=paste0("Drug Arrests per 100,000")) +
    theme(legend.position = "bottom",
          text = element_text(size=20)) -> plot
  if (state==41) stname = "OR"
  if (state==53) stname = "WA"
  ggsave(paste0("Figures/Crime/DrugArrest_", y, "_", stname, "_", start, ".png"), plot, width=12, height=8)
  
  
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
  pval = round(pval, 3)
  
  return(c(att, paste0("{", pval, "}")))
}




############ Table 2: Extension #############
table = cbind(
  c("ATT", "P-Val"),
  create_output(41, 1, "drug_all"),
  create_output(41, 2, "drug_all"),
  create_output(53, 2, "drug_all"),
  create_output(53, 3, "drug_all")
)

# 
# table = rbind(
#   table,
#   c("State", rep("OR",2), rep("WA",2)),
#   c("Treatment", "Jan", "Feb", "Feb", "Mar")
# ) %>% as.data.frame()
# 
# write_csv(table, "Estimate/Tables/Crime.csv")
