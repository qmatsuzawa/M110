############ Estimate Structural Change in NFLIS Fentanyl Measures ############

library(pacman)
p_load(
  tidyverse,
  lubridate,
  strucchange,
  maps,
  sf,
  ggrepel,
  scales
)

theme_set(theme_minimal())

nflis_raw_file <- "Data/Raw/NFLIS/raw_NFLIS_Drug_DQS_2026_06_23_18_09_50_literal_heroin.csv"
population_file <- "Data/Raw/population.rds"
figure_dir <- "Figures/Descriptive"

if (!file.exists(nflis_raw_file)) {
  stop("Missing NFLIS raw file: ", nflis_raw_file, call. = FALSE)
}

if (!file.exists(population_file)) {
  stop("Missing population file: ", population_file, call. = FALSE)
}

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

save_figure <- function(plot, filename, width = 8, height = 6) {
  ggsave(
    filename = file.path(figure_dir, filename),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    bg = "white"
  )
}

parse_nflis_state_export <- function(path) {
  raw_export <- read_csv(
    path,
    col_types = cols(
      .default = col_character(),
      YYYY = col_integer(),
      DRUG_REPORTS = col_double()
    )
  )

  required_columns <- c(
    "YYYY",
    "PERIOD",
    "DRUG_REPORTS",
    "DRUG_CATEGORY_DESCRIPTION",
    "BASE_DESCRIPTION",
    "SUBSTANCE_DESCRIPTION",
    "STATE"
  )
  missing_columns <- setdiff(required_columns, names(raw_export))

  if (length(missing_columns) > 0) {
    stop(
      "NFLIS raw file is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  raw_export %>%
    transmute(
      state_abb = str_to_upper(str_trim(STATE)),
      year = YYYY,
      half = case_when(
        str_detect(PERIOD, "^SA1") ~ 1L,
        str_detect(PERIOD, "^SA2") ~ 2L,
        TRUE ~ NA_integer_
      ),
      drug_category = str_trim(DRUG_CATEGORY_DESCRIPTION),
      base_description = str_trim(BASE_DESCRIPTION),
      substance_description = str_trim(SUBSTANCE_DESCRIPTION),
      drug_reports = coalesce(DRUG_REPORTS, 0),
      is_fentanyl = drug_category == "Fentanyl and Fentanyl-related",
      is_heroin = drug_category == "Heroin" | base_description == "Heroin"
    ) %>%
    filter(!is.na(year), !is.na(half), !is.na(state_abb), state_abb != "")
}

complete_state_half_panel <- function(panel, count_columns) {
  fill_values <- rep(list(0), length(count_columns))
  names(fill_values) <- count_columns

  panel %>%
    complete(
      state_abb = sort(unique(state_abb)),
      year = seq(min(year, na.rm = TRUE), max(year, na.rm = TRUE)),
      half = c(1L, 2L),
      fill = fill_values
    ) %>%
    arrange(state_abb, year, half)
}

extend_population_panel <- function(population_data, target_year) {
  growth_inputs <- population_data %>%
    arrange(state_abb, year) %>%
    group_by(state_abb) %>%
    mutate(annual_growth = population / lag(population) - 1) %>%
    summarise(
      state = state[which.max(year)],
      last_year = max(year, na.rm = TRUE),
      last_population = population[which.max(year)],
      last_growth = {
        observed_growth <- annual_growth[!is.na(annual_growth)]
        if (length(observed_growth) == 0) {
          0
        } else {
          tail(observed_growth, 1)
        }
      },
      .groups = "drop"
    )

  future_population <- growth_inputs %>%
    filter(last_year < target_year) %>%
    pmap_dfr(function(state_abb, state, last_year, last_population, last_growth) {
      future_years <- seq(last_year + 1, target_year)

      tibble(
        year = future_years,
        state_abb = state_abb,
        state = state,
        population = last_population * cumprod(rep(1 + last_growth, length(future_years)))
      )
    })

  bind_rows(population_data, future_population) %>%
    arrange(state_abb, year)
}

build_nflis_panel <- function(nflis_file, population_file) {
  state_universe <- c(state.abb, "DC")

  nflis_raw <- parse_nflis_state_export(nflis_file)

  state_half_counts <- nflis_raw %>%
    group_by(state_abb, year, half) %>%
    summarise(
      fent_count = sum(if_else(is_fentanyl, drug_reports, 0), na.rm = TRUE),
      nonfent_count = sum(if_else(!is_fentanyl, drug_reports, 0), na.rm = TRUE),
      narcotic_analgesic_count = sum(
        if_else(drug_category == "Narcotic Analgesics", drug_reports, 0),
        na.rm = TRUE
      ),
      heroin_count = sum(if_else(is_heroin, drug_reports, 0), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    complete_state_half_panel(c(
      "fent_count",
      "nonfent_count",
      "narcotic_analgesic_count",
      "heroin_count"
    ))

  population_panel <- readRDS(population_file) %>%
    ungroup()

  population_panel <- extend_population_panel(
    population_panel,
    max(state_half_counts$year, na.rm = TRUE)
  )

  state_half_counts %>%
    filter(state_abb %in% state_universe) %>%
    mutate(
      Date = as.Date(if_else(
        half == 1L,
        paste0(year, "-04-01"),
        paste0(year, "-10-01")
      )),
      alldrugs_seizure_count = fent_count + nonfent_count,
      opioid_seizure_count = fent_count + narcotic_analgesic_count + heroin_count,
      zoorob_ratio = if_else(
        alldrugs_seizure_count > 0,
        fent_count / alldrugs_seizure_count,
        NA_real_
      ),
      fent_all_opioid_share = if_else(
        opioid_seizure_count > 0,
        fent_count / opioid_seizure_count,
        0
      )
    ) %>%
    left_join(population_panel, by = c("year", "state_abb")) %>%
    mutate(
      pc_fent = if_else(!is.na(population), fent_count / population * 100000, NA_real_),
      pc_nonfent = if_else(!is.na(population), nonfent_count / population * 100000, NA_real_)
    ) %>%
    arrange(state_abb, Date)
}

validate_nflis_panel <- function(panel) {
  summary_row <- panel %>%
    summarise(
      states = n_distinct(state_abb),
      half_years = paste(sort(unique(half)), collapse = ", "),
      missing_population = sum(is.na(population)),
      missing_opioid_share = sum(is.na(fent_all_opioid_share))
    )

  if (summary_row$states != 51L) {
    stop("The NFLIS panel should contain 50 states plus DC.", call. = FALSE)
  }

  if (summary_row$half_years != "1, 2") {
    stop("The NFLIS panel should contain both half-year periods.", call. = FALSE)
  }

  if (summary_row$missing_population > 0) {
    stop("The NFLIS panel has rows without population data.", call. = FALSE)
  }

  if (summary_row$missing_opioid_share > 0) {
    stop("The NFLIS panel has missing fentanyl opioid-share values.", call. = FALSE)
  }
}

measure_keys <- c(
  "zoorob_ratio",
  "pc_fent",
  "pc_nonfent",
  "fent_all_opioid_share"
)

measure_labels <- c(
  zoorob_ratio = "Fentanyl to All-Drug Seizures",
  pc_fent = "Per-Capita Fentanyl Seizures",
  pc_nonfent = "Per-Capita Non-Fentanyl Seizures",
  fent_all_opioid_share = "Fentanyl Share of Opioid Seizures"
)

measure_axis_labels <- c(
  zoorob_ratio = "Ratio",
  pc_fent = "Seizures per 100,000",
  pc_nonfent = "Seizures per 100,000",
  fent_all_opioid_share = "Share"
)

western_states <- c(
  "AZ", "CO", "UT", "NV", "NM", "ID", "MT", "WY", "CA", "HI", "AK"
)

series_colors <- c(
  OR = "#0072B2",
  WA = "#C1121F",
  West = "#7A9CC6",
  `Rest of US` = "#4D4D4D"
)

series_linetypes <- c(
  OR = "solid",
  WA = "solid",
  West = "longdash",
  `Rest of US` = "dotted"
)

series_linewidths <- c(
  OR = 1.2,
  WA = 1.2,
  West = 0.8,
  `Rest of US` = 0.8
)

series_point_sizes <- c(
  OR = 1.6,
  WA = 1.6,
  West = 1,
  `Rest of US` = 1
)

build_group_series <- function(panel, measure_key) {
  bind_rows(
    panel %>%
      mutate(
        group = case_when(
          state_abb %in% c("OR", "WA") ~ state_abb,
          TRUE ~ "Rest of US"
        )
      ) %>%
      group_by(group, Date) %>%
      summarise(value = mean(.data[[measure_key]], na.rm = TRUE), .groups = "drop") %>%
      filter(group %in% c("OR", "WA", "Rest of US")),
    panel %>%
      filter(state_abb %in% western_states) %>%
      group_by(Date) %>%
      summarise(value = mean(.data[[measure_key]], na.rm = TRUE), .groups = "drop") %>%
      mutate(group = "West")
  ) %>%
    filter(!is.nan(value), !is.na(value)) %>%
    mutate(group = factor(group, levels = c("OR", "WA", "West", "Rest of US")))
}

plot_group_series <- function(panel, measure_key) {
  plot_data <- build_group_series(panel, measure_key)

  ggplot(plot_data, aes(x = Date, y = value, color = group, group = group)) +
    geom_line(aes(linetype = group, linewidth = group)) +
    geom_point(aes(size = group)) +
    scale_color_manual(values = series_colors, name = NULL, drop = FALSE) +
    scale_linetype_manual(values = series_linetypes, name = NULL, drop = FALSE) +
    scale_linewidth_manual(values = series_linewidths, guide = "none") +
    scale_size_manual(values = series_point_sizes, guide = "none") +
    labs(title = measure_labels[[measure_key]], x = NULL, y = measure_axis_labels[[measure_key]]) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 11),
      legend.position = "bottom",
      legend.key.width = grid::unit(1.3, "cm"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

estimate_single_break <- function(data, measure_key, from = 0.15, to = 0.85) {
  model_data <- data %>%
    arrange(Date) %>%
    filter(!is.na(.data[[measure_key]]))

  if (nrow(model_data) < 10) {
    return(tibble(
      measure = measure_key,
      qlr_stat = NA_real_,
      p_value = NA_real_,
      break_index = NA_integer_,
      break_date = as.Date(NA)
    ))
  }

  fstats_obj <- Fstats(
    as.formula(paste(measure_key, "~ 1")),
    data = model_data,
    from = from,
    to = to
  )
  qlr_test <- sctest(fstats_obj, type = "supF")
  break_index <- breakpoints(fstats_obj)$breakpoints[1]

  tibble(
    measure = measure_key,
    qlr_stat = as.numeric(qlr_test$statistic),
    p_value = as.numeric(qlr_test$p.value),
    break_index = as.integer(break_index),
    break_date = if (!is.na(break_index)) model_data$Date[break_index] else as.Date(NA)
  )
}

estimate_multiple_breaks <- function(data, measure_key, model_type = c("mean", "trend"), h = 0.10) {
  model_type <- match.arg(model_type)
  model_data <- data %>%
    arrange(Date) %>%
    filter(!is.na(.data[[measure_key]]))

  model_formula <- if (model_type == "mean") {
    as.formula(paste(measure_key, "~ 1"))
  } else {
    as.formula(paste(measure_key, "~ seq_along(Date)"))
  }

  break_indices <- breakpoints(model_formula, data = model_data, h = h)$breakpoints
  break_dates <- model_data$Date[break_indices]
  break_dates <- break_dates[!is.na(break_dates)]

  tibble(
    measure = measure_key,
    model_type = model_type,
    n_breaks = length(break_dates),
    break_dates = list(break_dates)
  )
}

plot_multiple_breaks <- function(break_table, title) {
  break_table %>%
    unnest(break_dates) %>%
    mutate(measure_label = recode(measure, !!!measure_labels)) %>%
    ggplot(aes(x = break_dates, y = measure_label, color = model_type, shape = model_type)) +
    geom_jitter(width = 50, height = 0.05, size = 2.6, alpha = 0.95) +
    facet_wrap(~state, ncol = 1, scales = "free_y") +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    scale_color_manual(values = c(mean = "red", trend = "blue"), name = "Model") +
    scale_shape_manual(values = c(mean = 16, trend = 17), name = "Model") +
    labs(title = title, x = "Break Date", y = NULL) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(size = 10, margin = margin(r = 4)),
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 13, face = "bold")
    )
}

plot_break_date_map <- function(map_shapes, measure_key) {
  plot_data <- map_shapes %>%
    filter(measure == measure_key)

  break_range <- range(plot_data$break_date, na.rm = TRUE)
  break_ticks <- as.Date(
    round(seq(as.numeric(break_range[1]), as.numeric(break_range[2]), length.out = 3)),
    origin = "1970-01-01"
  )

  ggplot(plot_data) +
    geom_sf(aes(fill = break_date), color = "white", linewidth = 0.2) +
    scale_fill_gradientn(
      colours = c("#2166AC", "#F7F7F7", "#B2182B"),
      na.value = "grey90",
      trans = "date",
      breaks = break_ticks,
      labels = label_date("%Y"),
      guide = guide_colorbar(
        title = "Break date",
        barwidth = grid::unit(40, "mm"),
        barheight = grid::unit(4, "mm"),
        title.position = "top",
        ticks = FALSE
      )
    ) +
    labs(
      title = paste("Estimated Structural Break by State:", measure_labels[[measure_key]]),
      x = NULL,
      y = NULL
    ) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      panel.grid = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )
}

nflis_panel <- build_nflis_panel(nflis_raw_file, population_file)
validate_nflis_panel(nflis_panel)

figure_panel <- nflis_panel %>%
  filter(year >= 2010, year <= 2023)

save_figure(
  plot_group_series(figure_panel, "zoorob_ratio"),
  "fig03_panel_a_fentanyl_share_all_drugs.png"
)

save_figure(
  plot_group_series(figure_panel, "pc_fent"),
  "fig03_panel_b_fentanyl_reports_per_capita.png"
)

save_figure(
  plot_group_series(figure_panel, "pc_nonfent"),
  "fig03_panel_c_nonfentanyl_reports_per_capita.png"
)

save_figure(
  plot_group_series(figure_panel, "fent_all_opioid_share"),
  "fig03_panel_d_fentanyl_share_opioid_reports.png"
)

single_break_table <- nflis_panel %>%
  group_by(state_abb) %>%
  group_modify(~ map_dfr(measure_keys, function(measure_key) {
    estimate_single_break(.x, measure_key)
  })) %>%
  ungroup()

state_shapes <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) %>%
  rename(region = ID)

state_crosswalk <- tibble(
  region = tolower(state.name),
  state_abb = state.abb
)

break_map_shapes <- state_shapes %>%
  left_join(state_crosswalk, by = "region") %>%
  left_join(single_break_table, by = "state_abb") %>%
  mutate(break_date = as.Date(break_date))

save_figure(
  plot_break_date_map(break_map_shapes, "zoorob_ratio"),
  "app_fig07_map_fentanyl_share_all_drugs.png"
)

save_figure(
  plot_break_date_map(break_map_shapes, "pc_fent"),
  "app_fig07_map_fentanyl_reports_per_capita.png"
)

save_figure(
  plot_break_date_map(break_map_shapes, "pc_nonfent"),
  "app_fig07_map_nonfentanyl_reports_per_capita.png"
)

save_figure(
  plot_break_date_map(break_map_shapes, "fent_all_opioid_share"),
  "app_fig07_map_fentanyl_share_opioid_reports.png"
)

multiple_break_table <- map_dfr(c("OR", "WA"), function(state_code) {
  state_data <- nflis_panel %>%
    filter(state_abb == state_code)

  map_dfr(measure_keys, function(measure_key) {
    bind_rows(
      estimate_multiple_breaks(state_data, measure_key, "mean", h = 0.10),
      estimate_multiple_breaks(state_data, measure_key, "trend", h = 0.10)
    )
  }) %>%
    mutate(state = state_code)
})

save_figure(
  plot_multiple_breaks(
    multiple_break_table %>% filter(state == "OR"),
    "Estimated Structural Break Dates: Oregon"
  ),
  "app_fig05_bai_perron_multiple_breaks_or.png"
)

save_figure(
  plot_multiple_breaks(
    multiple_break_table %>% filter(state == "WA"),
    "Estimated Structural Break Dates: Washington"
  ),
  "app_fig05_bai_perron_multiple_breaks_wa.png"
)

demeaned_panel <- nflis_panel %>%
  group_by(Date) %>%
  mutate(
    across(
      all_of(measure_keys),
      ~ .x - mean(.x, na.rm = TRUE),
      .names = "{.col}_dm"
    )
  ) %>%
  ungroup()

demeaned_measure_keys <- paste0(measure_keys, "_dm")
demeaned_measure_labels <- paste(measure_labels, "DM")
names(demeaned_measure_labels) <- demeaned_measure_keys
measure_labels <- c(measure_labels, demeaned_measure_labels)

demeaned_break_table <- map_dfr(c("OR", "WA"), function(state_code) {
  state_data <- demeaned_panel %>%
    filter(state_abb == state_code)

  map_dfr(demeaned_measure_keys, function(measure_key) {
    bind_rows(
      estimate_multiple_breaks(state_data, measure_key, "mean", h = 0.10),
      estimate_multiple_breaks(state_data, measure_key, "trend", h = 0.10)
    )
  }) %>%
    mutate(state = state_code)
})

save_figure(
  plot_multiple_breaks(
    demeaned_break_table %>% filter(state == "OR"),
    "Estimated Structural Break Dates, Demeaned Measures: Oregon"
  ),
  "app_fig06_bai_perron_demeaned_breaks_or.png"
)

save_figure(
  plot_multiple_breaks(
    demeaned_break_table %>% filter(state == "WA"),
    "Estimated Structural Break Dates, Demeaned Measures: Washington"
  ),
  "app_fig06_bai_perron_demeaned_breaks_wa.png"
)
