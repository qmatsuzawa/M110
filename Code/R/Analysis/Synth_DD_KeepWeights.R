################## Estimate SC and DiD Forcing Same Weights ########################
library(pacman)
p_load(readstata13, tidyverse, magrittr, usmap, kableExtra, fixest, broom)
theme_set(theme_classic())

### Load Data
data = read_csv("Data/Cleaned/NVSS_Mortality_MainRegReady.csv") %>%
  mutate(spencer=spencer/pop*100000,
         joshi=joshi/pop*100000)

## Create fips => name
crosswalk = tibble(state = state.abb, `_Co_Number`=as.numeric(fips(state)))

## Controls 
control = data %>% filter(! state_fips %in% c(41, 53)) %>% .$state_fips %>% unique()

########### Define Functions ##################
create_output = function(state, start, end, dt) {
  ### Load Data
  treat = 36 + start
  weights = read.dta13(paste0("Estimate/SC_Main/ReplicationExtention/Synth_", dt, "_", state, "_start", start, ".dta"))
  colnames(weights)[c(1,2)] = c("state_fips", "weight")
  weights %<>% filter(!is.na(state_fips))
  
  placebo_wt = read_csv(paste0("Estimate/SC_Main/ReplicationExtention/Placebos_", dt, state, "_start", start, ".csv"),
                      show_col_types = FALSE)
  colnames(placebo_wt)[c(2,3)] = c("state_fips", "weight")
  placebo_wt %<>% filter(!is.na(state_fips))

  ## Other data
  otherdt = setdiff(c("spencer", "joshi"), dt)
  
  ### Create Synthetic Control
  main = data %>%
    filter(state_fips %in% c(control, state)) %>%
    left_join(weights, by="state_fips") %>%
    mutate(od_rate = get(otherdt),
           y = ifelse(state_fips==state, od_rate, 0),
           y_synth = od_rate*weight) %>%
    group_by(time) %>%
    summarize(y=sum(y, na.rm=T),
              y_synth = sum(y_synth, na.rm=T)) %>%
    ungroup() %>%
    filter(time<=end)
  
  ### Figures
  ggplot(main, aes(x=time)) +
    geom_line(aes(y=y, color="Actual"), linewidth=1.7) + 
    geom_line(aes(y=y_synth, color="Synthetic"), linetype="dashed", linewidth=1.7) + 
    scale_color_manual(values= c("red", "blue"),
                       name="") +
    scale_x_continuous(breaks = c(1,13,25, 37, 49),
                       labels = c(2018:2022)) +
    geom_vline(xintercept = treat, linetype="dashed", linewidth=1.3) +
    labs(x="Time", y="OD per 100,000") +
    theme(legend.position = "bottom",
          text = element_text(size=20)) -> plot
  
  if (state==41) stname = "OR"
  if (state==53) stname = "WA"
  ggsave(paste0("Figures/SameWeights_OD/", stname, "_", dt, "_", end, "_", start, ".png"), plot, width=12, height=8)

  
  ### Placebos + ATT
  placebos = data %>% 
    filter(state_fips %in% c(control)) %>%
    left_join(placebo_wt, by="state_fips", relationship ="many-to-many") %>%
    mutate(od_rate = get(otherdt),
           y = ifelse(state_fips==unit, od_rate, 0),
           y_synth = od_rate*weight) %>%
    group_by(unit, time) %>%
    summarize(y=sum(y, na.rm=T),
              y_synth = sum(y_synth, na.rm=T)) %>%
    group_by(unit) %>%
    mutate(time=row_number()) %>%
    filter(time<=end) %>%
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
  if (pval < 0.05) att = paste0(att, "**") 
  if (pval < 0.01) att = paste0(att, "**") 
  
  return(c(att, paste0("{", pval, "}")))
}

################  Table  ####################
table = cbind(
  c("ATT", "P-Val"),
  create_output(41, 1, 51, "spencer"),
  create_output(41, 2 ,51, "spencer"),
  create_output(53, 2, 51, "spencer"),
  create_output(53, 3, 51, "spencer"),
  create_output(41, 1, 51, "joshi"),
  create_output(41, 2, 51, "joshi"),
  create_output(53, 2, 51, "joshi"),
  create_output(53, 3, 51, "joshi")
)

table = cbind(
  c("ATT", "P-Val"),
  create_output(41, 1, 72, "spencer"),
  create_output(41, 2, 72, "spencer"),
  create_output(53, 2, 72, "spencer"),
  create_output(53, 3, 72, "spencer"),
  create_output(41, 1, 72, "joshi"),
  create_output(41, 2, 72, "joshi"),
  create_output(53, 2, 72, "joshi"),
  create_output(53, 3, 72, "joshi")
)

################# Synthetic Style Diff-in-Diff ####################
create_output = function(state, start, end, dt) {
  ### Load Data
  treat = 36 + start
  weights = read.dta13(paste0("Estimate/SC_Main/ReplicationExtention/Synth_", dt, "_", state, "_start", start, ".dta"))
  colnames(weights)[c(1,2)] = c("state_fips", "weight")
  weights %<>% filter(!is.na(state_fips))
  
  placebo_wt = read_csv(paste0("Estimate/SC_Main/ReplicationExtention/Placebos_", dt, state, "_start", start, ".csv"),
                        show_col_types = FALSE)
  colnames(placebo_wt)[c(2,3)] = c("state_fips", "weight")
  placebo_wt %<>% filter(!is.na(state_fips))
  
  ## Other data
  otherdt = setdiff(c("spencer", "joshi"), dt)
  
  ### Diff-in-Diff w/ SC Weights
  main = data %>%
    filter(state_fips %in% c(control, state)) %>%
    left_join(weights, by="state_fips") %>%
    mutate(weight = ifelse(state_fips==state, 1, weight),
           treat = ifelse(state_fips==state & time >= treat, 1,0),
           od_rate = get(otherdt),
    ) %>% 
    filter(weight >0) %>%
    filter(time <= end)
  
  fit = feols(od_rate ~ treat | state_fips + time, main, weight=~weight)
  dd_est = tidy(fit) %>% mutate(unit="main")
  for (p in control) {
    placebos = data %>% 
      filter(state_fips %in% c(control, state)) %>%
      left_join(placebo_wt %>% filter(unit==p), by="state_fips") %>%
      mutate(weight = ifelse(state_fips==p, 1, weight),
             treat = ifelse(state_fips==p & time >= treat, 1,0),
             od_rate = get(otherdt),
      ) %>% 
      filter(weight>0) %>%
      filter(time <= end)
    fit = feols(od_rate ~ treat | state_fips + time,  placebos, weight=~weight)
    dd_est = tidy(fit) %>% 
      mutate(unit=paste0(p)) %>%
      bind_rows(dd_est)
  }
  
  
  pval = dd_est %>%
    arrange(desc(abs(estimate))) %>%
    mutate(pval = row_number()/n()) %>%
    filter(unit=="main") %>%
    .$pval
  
  att = dd_est %>%
    filter(unit=="main") %>%
    .$estimate
  
  att = round(att, 3)
  if (pval < 0.1) att = paste0(att, "*") 
  if (pval < 0.05) att = paste0(att, "**") 
  if (pval < 0.01) att = paste0(att, "**") 
  
  return(c(att, paste0("{", pval, "}")))
}

table1 = cbind(
  c("ATT", "P-Val"),
  create_output(41, 1, 51, "spencer"),
  create_output(41, 2 ,51, "spencer"),
  create_output(53, 2, 51, "spencer"),
  create_output(53, 3, 51, "spencer"),
  create_output(41, 1, 51, "joshi"),
  create_output(41, 2, 51, "joshi"),
  create_output(53, 2, 51, "joshi"),
  create_output(53, 3, 51, "joshi")
)


table2 = cbind(
  c("ATT", "P-Val"),
  create_output(41, 1, 72, "joshi"),
  create_output(41, 2 ,72, "joshi"),
  create_output(41, 1, 72, "spencer"),
  create_output(41, 2, 72, "spencer"),
  create_output(53, 2, 72, "joshi"),
  create_output(53, 3, 72, "joshi"),
  create_output(53, 2, 72, "spencer"),
  create_output(53, 3, 72, "spencer")
)

table = rbind(
  c("", paste0("(", c(1:8), ")")),
  table2,
  c("Outcome", rep(c("Spencer", "Spencer", "Joshi", "Joshi"), 2)),
  c("Weight", rep(c("Joshi", "Joshi", "Spencer", "Spencer"),2)),
  c("Treatment", c("Jan", "Feb", "Jan", "Feb", "Feb", "Mar", "Feb", "Mar"))
) %>% as.data.frame()


table %<>% mutate(across(V2:V9, ~str_replace_all(., c("\\{"="\\\\{", "\\}"="\\\\}"))))
table[1,1] = " & \\multicolumn{4}{c}{Oregon} & \\multicolumn{4}{c}{Washington} \\\\  \\cmidrule(l){2-5}  \\cmidrule(l){6-9}"
table[2,1] = " \\hline ATT Overall"

table = convert_tex(table, title="\\label{tab:robustsameweight} Robustness: Forcing Same Weights \\& Estimating Difference-in-Differences")
writeLines(table, "Estimate/Tables/DD_SameWeights.tex")