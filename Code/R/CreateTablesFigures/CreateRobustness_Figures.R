######### Create Appendix Figures: Robustness to Dropping 1 state & Imputation #########
library(pacman)
p_load(tidyverse, tidycensus, magrittr, data.table, usmap, lubridate, readstata13)
theme_set(theme_classic())

########## Robustness: Drop One State #############

create_output = function(state, start, dt) {
  
treat = 36 + start
weights = read_csv(paste0("Estimate/SC_Main/Robustness/Dropone_", dt , "_",
                          state, "_start", start, ".csv"), show_col_types = FALSE)

colnames(weights)[c(2,3,4,5,6)] = c("state_fips", "weight", "y", "y_synth", "time")

est_dropped = weights %>%
  group_by(unit) %>%
  filter(time >= treat) %>%
  summarize(att=mean(y-y_synth))

est_dropped %<>%
  left_join(tibble(abb=state.abb, unit=as.numeric(fips(abb))), by="unit") %>%
  mutate(abb = ifelse(unit==11,"DC", abb))

print(paste0("Min: ", min(est_dropped$att), " Max: ", max(est_dropped$att)))
plot = ggplot(est_dropped, aes(att, reorder(abb, att))) + 
  geom_point(size=3) +
  labs(x="Estimated Treatment Effect", y="States Excluded") +
  theme(text=element_text(size=24))

if (state==41) stname = "OR"
if (state==53) stname = "WA"
ggsave(paste0("Figures/Robustness/DropOneState_", stname, "_", dt, "_", start, ".png"), plot, width=12, height=10)
}

create_output(41, 1, "spencer")
create_output(53, 2, "spencer")
create_output(41, 2, "joshi")
create_output(53, 3, "joshi")




############ Robustness Imputation ###############
create_output = function(state, start, dt) {

placebos = read_csv(paste0("Estimate/SC_Main/Robustness/Robust_Imputation_", dt, "_", state, "_start", start, ".csv"))
colnames(placebos)[c(4,5,6)] = c("y", "y_synth", "time")
placebos %<>% 
  mutate(unit=as.character(unit)) %>%
  mutate(mspe = ifelse(time < start + 36, (y-y_synth)^2, NA),
         att = ifelse(time >= start + 36, (y-y_synth), NA),
         spencer = ifelse(time >= start + 36 & time<=48, (y-y_synth), NA),
         joshi = ifelse(time >= start + 36 & time<=51, (y-y_synth), NA),
  ) %>%
  group_by(unit) %>%
  summarize(mspe = mean(mspe, na.rm=T),
            spencer = mean(spencer, na.rm=T),
            joshi = mean(joshi, na.rm=T)
  )

if (dt=="joshi" & state==41) att = 0.268
if (dt=="joshi" & state==53) att = 0.112
if (dt=="spencer" & state==41) att = 0.39
if (dt=="spencer" & state==53) att = 0.17

  
plot = ggplot(placebos %>% mutate(y = get(dt)), aes(y)) + 
  geom_density(fill="lightblue", linewidth=2) +
  geom_vline(xintercept = att, color="red", linetype="dashed", linewidth=2) +
  labs(x="Estimated Effect", y="Density") +
  theme(text=element_text(size=20))  +
  theme(text=element_text(size=24))

if (state==41) stname = "OR"
if (state==53) stname = "WA"
ggsave(paste0("Figures/Robustness/Distribution_", stname, "_", dt, "_", start, ".png"), plot, width=12, height=10)
}

create_output(41, 1,  "spencer")
create_output(53, 2, "spencer")
create_output(41, 2, "joshi")
create_output(53, 3, "joshi")
