########## Create Appendix Fig -- Time Trend for Different Mortality Definition #############
library(pacman)
p_load(tidyverse, magrittr, lubridate)
theme_set(theme_minimal())

out = "Figures/Descriptive/definition_figures_clean"
dir.create(out, recursive = TRUE, showWarnings = FALSE)

save_plot = function(plot, file) {
  ggsave(file.path(out, file), plot, width=12, height=8, units="in")
}

dat = read_csv("Data/Cleaned/NVSS_Mortality_MainRegReady.csv", show_col_types = FALSE)
month_var = ifelse("monthdth" %in% names(dat), "monthdth", "month")

dat %<>%
  mutate(date = as.Date(sprintf("%04d-%02d-01", year, .data[[month_var]])))

make_defs = function(df) {
  bind_rows(
    df %>% transmute(date, state_fips, definition="Joshi et al.", od=joshi),
    df %>% transmute(date, state_fips, definition="Spencer", od=spencer)
  ) %>%
    mutate(oregon = ifelse(state_fips==41, od, 0),
           washington = ifelse(state_fips==53, od, 0),
           oregon_washington = ifelse(state_fips %in% c(41,53), od, 0)) %>%
    group_by(date, definition) %>%
    summarize(national=sum(od, na.rm=TRUE),
              oregon=sum(oregon, na.rm=TRUE),
              washington=sum(washington, na.rm=TRUE),
              oregon_washington=sum(oregon_washington, na.rm=TRUE),
              .groups="drop")
}

defs = make_defs(dat)
cols = c("Joshi et al."="#7B2CBF", "Spencer"="#F4A261")
geo_labs = c(national="United States", oregon="Oregon",
             washington="Washington", oregon_washington="Oregon and Washington")

plot_defs = function(geo) {
  defs %>%
    select(date, definition, all_of(geo)) %>%
    rename(deaths=all_of(geo)) %>%
    ggplot(aes(date, deaths, color=definition)) +
    geom_line(linewidth=1.2) +
    scale_color_manual(values=cols, name="") +
    scale_y_continuous(labels=scales::comma) +
    labs(title=paste("Monthly Overdose Deaths:", geo_labs[[geo]]),
         x=NULL, y="Total overdose deaths") +
    theme(legend.position="bottom",
          plot.title=element_text(face="bold", size=13),
          axis.text.x=element_text(angle=45, hjust=1))
}

diffs = dat %>%
  mutate(diff = joshi - spencer,
         oregon = ifelse(state_fips==41, diff, 0),
         washington = ifelse(state_fips==53, diff, 0),
         oregon_washington = ifelse(state_fips %in% c(41,53), diff, 0)) %>%
  group_by(date) %>%
  summarize(national=sum(diff, na.rm=TRUE),
            oregon=sum(oregon, na.rm=TRUE),
            washington=sum(washington, na.rm=TRUE),
            oregon_washington=sum(oregon_washington, na.rm=TRUE),
            .groups="drop")

plot_diff = function(geo) {
  diffs %>%
    select(date, all_of(geo)) %>%
    rename(diff=all_of(geo)) %>%
    ggplot(aes(date, diff)) +
    geom_hline(yintercept=0, color="grey60") +
    geom_line(linewidth=1.2, color="#2A9D8F") +
    scale_y_continuous(labels=scales::comma) +
    labs(title=paste("Monthly Definition Difference:", geo_labs[[geo]]),
         x=NULL, y="Joshi et al. minus Spencer deaths") +
    theme(plot.title=element_text(face="bold", size=13),
          axis.text.x=element_text(angle=45, hjust=1))
}

for (geo in names(geo_labs)) {
  save_plot(plot_defs(geo), paste0("monthly_definition_", geo, ".png"))
  save_plot(plot_diff(geo), paste0("monthly_definition_difference_", geo, ".png"))
}

annual = dat %>%
  group_by(year) %>%
  summarize(`Joshi et al.`=sum(joshi, na.rm=TRUE),
            Spencer=sum(spencer, na.rm=TRUE),
            .groups="drop") %>%
  pivot_longer(c(`Joshi et al.`, Spencer), names_to="definition", values_to="deaths")

ggplot(annual, aes(year, deaths, color=definition)) +
  geom_line(linewidth=1.2) +
  geom_point(size=2.4) +
  scale_color_manual(values=cols, name="") +
  scale_x_continuous(breaks=sort(unique(annual$year))) +
  scale_y_continuous(labels=scales::comma) +
  labs(title="Annual National Overdose Deaths by Definition",
       x=NULL, y="Total overdose deaths") +
  theme(legend.position="bottom",
        plot.title=element_text(face="bold", size=13)) -> annual_plot

save_plot(annual_plot, "annual_national_definition_comparison.png")
