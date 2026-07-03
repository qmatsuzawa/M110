################## Create Figure + Table: Replication & Extension ########################
library(pacman)
p_load(readstata13, tidyverse, magrittr, usmap, kableExtra)
theme_set(theme_classic())

## Create fips => name
crosswalk = tibble(state = state.abb, `_Co_Number`=as.numeric(fips(state)))

########### Define Functions ##################
create_output = function(state, start, end, dt) {
  ### Load Data
  treat = 36 + start
  
  if (dt=="JAMA") y = "joshi"
  if (dt=="Spencer") y="spencer"
  
  main = read.dta13(paste0("Estimate/SC_Main/ReplicationExtention/Synth_", y, "_", state, "_start", start, ".dta"))
  colnames(main)[c(3,4,5)] = c("y", "y_synth", "time")
  main %<>% filter(time <= end)
  placebos = read_csv(paste0("Estimate/SC_Main/ReplicationExtention/Placebos_", y, state,  "_start", start, ".csv"),
                      show_col_types = FALSE)
  colnames(placebos)[c(4,5,6)] = c("y", "y_synth", "time")
  placebos %<>% filter(time <= end)
  
  ### Figures
  ggplot(main, aes(x=time)) +
    geom_line(aes(y=y, color="Actual"), linewidth=1.7) + 
    geom_line(aes(y=y_synth, color="Synthetic"), linetype="dashed", linewidth=1.7) + 
    scale_color_manual(values= c("red", "blue"),
                       name="") +
    scale_x_continuous(breaks = c(1,13,25, 37, 49, 61),
                       labels = c(2018:2023)) +
    geom_vline(xintercept = treat, linetype="dashed", linewidth=1.3) +
    labs(x="Time", y="OD per 100,000") +
    theme(legend.position = "bottom",
          text = element_text(size=20)) -> plot
  # if (dt=="JAMA" & end >51) plot = plot + annotate('rect', xmin=36+start, xmax=51, ymin=-Inf, ymax=Inf, alpha=.5, fill='gray50')
  # if (dt=="Spencer" & end >48) plot = plot + annotate('rect', xmin=36+start, xmax=48, ymin=-Inf, ymax=Inf, alpha=.5, fill='gray50')
  if (state==6) stname = "CA"
  if (state==41) stname = "OR"
  if (state==53) stname = "WA"
  ggsave(paste0("Figures/Replication/", stname, "_", dt, "_", end, "_", start, ".png"), plot, width=12, height=8)
  
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
  
  return(c(att, paste0("{", pval, "}")))
}

create_output_21v22v23 = function(state, start, end, dt) {
  
  if (dt=="JAMA") y = "joshi"
  if (dt=="Spencer") y="spencer"
  
  ### Load Data
  treat = 36 + start
  main = read.dta13(paste0("Estimate/SC_Main/ReplicationExtention/Synth_", y, "_", state, "_start", start, ".dta"))
  colnames(main)[c(3,4,5)] = c("y", "y_synth", "time")
  main %<>% filter(time <= end)
  placebos = read_csv(paste0("Estimate/SC_Main/ReplicationExtention/Placebos_", y, state,  "_start", start, ".csv"),
                      show_col_types = FALSE)
  colnames(placebos)[c(4,5,6)] = c("y", "y_synth", "time")
  placebos %<>% filter(time <= end)
  
  ### Placebos + ATT
  placebos %<>% 
    bind_rows(main %>% mutate(unit=state, trtime=treat)) %>%
    mutate(error = (y-y_synth)^2,
           pre = ifelse(time < treat, error, NA),
           post1 = ifelse(time >= treat & time<=48, error, NA),
           post2 = ifelse(time >=49 & time<=60, error, NA),
           post3 = ifelse(time >=61 & time<=end, error, NA),
    ) %>% 
    group_by(unit) %>%
    summarize(pre=mean(pre, na.rm=T),
              post1=mean(post1, na.rm=T),
              post2=mean(post2, na.rm=T),
              post3=mean(post3, na.rm=T),
    ) %>% 
    mutate(rate1=post1/pre,
           rate2=post2/pre,
           rate3=post3/pre) %>%
    arrange(desc(rate1)) %>%
    mutate(n1=row_number(),
           pval1 = n1/nrow(.)) %>%
    arrange(desc(rate2)) %>%
    mutate(n2=row_number(),
           pval2 = n2/nrow(.)) %>%
    arrange(desc(rate3)) %>%
    mutate(n3=row_number(),
           pval3 = n3/nrow(.))
  
  
  pval1 = placebos %>%
    filter(unit==state) %>%
    .$pval1
  
  att1 = main %>%
    filter(time>=treat & time<=48) %>%
    mutate(diff = y - y_synth) %>%
    .$diff %>% 
    mean() 
  
  att1 = round(att1, 3)
  if (pval1 < 0.1) att1 = paste0(att1, "*") 
  if (pval1 < 0.05) att1 = paste0(att1, "*") 
  if (pval1 < 0.01) att1 = paste0(att1, "*") 
  
  
  pval2 = placebos %>%
    filter(unit==state) %>%
    .$pval2
  
  att2 = main %>%
    filter(time>=49 & time<=60)  %>%
    mutate(diff = y - y_synth) %>%
    .$diff %>% 
    mean() 
  
  att2 = round(att2, 3)
  if (pval2 < 0.1) att2 = paste0(att2, "*") 
  if (pval2 < 0.05) att2 = paste0(att2, "*") 
  if (pval2 < 0.01) att2 = paste0(att2, "*") 
  
  
  pval3 = placebos %>%
    filter(unit==state) %>%
    .$pval3
  
  att3 = main %>%
    filter(time>=61)  %>%
    mutate(diff = y - y_synth) %>%
    .$diff %>% 
    mean() 
  
  att3 = round(att3, 3)
  if (pval3 < 0.1) att3 = paste0(att3, "*") 
  if (pval3 < 0.05) att3 = paste0(att3, "*") 
  if (pval3 < 0.01) att3 = paste0(att3, "*") 
  
  return(c(att1, paste0("{", pval1, "}"),
           att2, paste0("{", pval2, "}"),
           att3, paste0("{", pval3, "}")
  ))
}


################  Table 1A. Rpelication: OR ####################
table1 = cbind(
  c("ATT", "P-Val"),
  create_output(41, 1, 48, "Spencer"),
  create_output(41, 2 ,48, "Spencer"),
  create_output(41, 1, 51, "Spencer"),
  create_output(41, 2, 51, "Spencer")
)

table2 = cbind(
  c("ATT", "P-Val"),
  create_output(41, 1, 48, "JAMA"),
  create_output(41, 2, 48, "JAMA"),
  create_output(41, 1, 51, "JAMA"),
  create_output(41, 2, 51, "JAMA")
)

table = rbind(
  table1,
  table2,
  c("Sample", "2018-21", "2018-21", "2018-22", "2018-22"),
  c("Matching Until", "Dec", "Jan", "Dec", "Jan")
)  %>% as.data.frame()

write_csv(table, "Estimate/Tables/Replication_OR.csv")

# table %<>% mutate(across(V2:V9, ~str_replace_all(., c("\\{"="\\\\{", "\\}"="\\\\}"))))
# table[1,1] = " & \\multicolumn{4}{c}{Panel I: Spencer (2023) Outcome}  \\\\ \\cmidrule(l){2-5} ATT"
# table[3,1] = " & \\multicolumn{4}{c}{Panel II: Joshi et al. (2023) Outcome}  \\\\ \\cmidrule(l){2-5} ATT"
# table = convert_tex(table, title="\\label{tab:replication_or} Replication: Synthetic Control Estimate for Oregon")
# writeLines(table, "Estimate/Tables/Replication_OR.tex")



################  Table 1B. Rpelication: WA ####################
table1 = cbind(
  c("ATT", "P-Val"),
  create_output(53, 2, 48, "Spencer"),
  create_output(53, 3,48, "Spencer"),
  create_output(53, 2, 51, "Spencer"),
  create_output(53, 3, 51, "Spencer")
)

table2 = cbind(
  c("ATT", "P-Val"),
  create_output(53, 2, 48, "JAMA"),
  create_output(53, 3, 48, "JAMA"),
  create_output(53, 2, 51, "JAMA"),
  create_output(53, 3, 51, "JAMA")
)


table = rbind(
  table1,
  table2,
  c("Sample", "2018-21", "2018-21", "2018-22", "2018-22"),
  c("Matching Until", "Dec", "Jan", "Dec", "Jan")
  ) %>% as.data.frame()


write_csv(table, "Estimate/Tables/Replication_WA.csv")

# table %<>% mutate(across(V2:V9, ~str_replace_all(., c("\\{"="\\\\{", "\\}"="\\\\}"))))
# table[1,1] = " & \\multicolumn{4}{c}{Panel I: Spencer (2023) Outcome}  \\\\ \\cmidrule(l){2-5} ATT"
# table[3,1] = " & \\multicolumn{4}{c}{Panel II: Joshi et al. (2023) Outcome}  \\\\ \\cmidrule(l){2-5} ATT"
# table = convert_tex(table, title="\\label{tab:replication_wa} Replication: Synthetic Control Estimate for Washington")
# writeLines(table, "Estimate/Tables/Replication_WA.tex")


############ Table 2: Extension #############
table = cbind(
  c("\\hline ATT Overall", "P-Val"),
  create_output(41, 2, 72, "JAMA"),
  create_output(53, 3, 72, "JAMA"),
  create_output(41, 1, 72, "Spencer"),
  create_output(53, 2, 72, "Spencer")
)

table2 = cbind(
  c("ATT 2021", "P-Val", "ATT 2022", "P-Val", "ATT 2023 ", "P-Val"),
  create_output_21v22v23(41, 2, 72, "JAMA"),
  create_output_21v22v23(53, 3, 72, "JAMA"),
  create_output_21v22v23(41, 1, 72, "Spencer"),
  create_output_21v22v23(53, 2, 72, "Spencer")
)

table = rbind(
  c("State", "OR", "WA", "OR", "WA"),
  table,
  c(rep("",5)),  table2[c(1,2),], 
  c(rep("",5)), table2[c(3,4),],
  c(rep("",5)), table2[c(5,6),]
) %>% as.data.frame()

write_csv(table, "Estimate/Tables/Extension_LongRunEffect.csv")

# table %<>% mutate(across(V2:V5, ~str_replace_all(., c("\\{"="\\\\{", "\\}"="\\\\}"))))
# table[1,1] = " & \\multicolumn{2}{c}{Joshi/Our Model} & \\multicolumn{2}{c}{Spencer Model} \\\\ \\cmidrule(l){2-3} \\cmidrule(l){4-5} State"
# table = convert_tex(table, title="\\label{tab:annualatt} Extension: Synthetic Control Estimate, Overall \\& Yearly ATT")
# writeLines(table, "Estimate/Tables/Extension_LongRunEffect.tex")
