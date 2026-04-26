library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)
library(zoo)
library(tibble)

# =========================================================
# 1. Read and prepare data
# =========================================================

flow_dat <- read.csv("/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Processes_data/ERDC_water_hourlyflow.csv") %>%
  mutate(date = as.Date(date)) %>%
  filter(site != "davis") %>%
  mutate(
    Name = case_when(
      site == "ophir" ~ "ophir",
      site == "winters_usgs" ~ "winters",
      site == "winters_up" ~ "winters",
      site == "browns" ~ "browns",
      site == "browns_sub" ~ "browns",
      TRUE ~ NA_character_
    )
  )

ppt_dat <- read.csv("/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Raw_data/prism_cat_new.csv") %>%
  filter(Name != "davis") %>%
  mutate(
    date = as.Date(Date, format = "%Y-%m-%d"),
    lagPPT = lag(ppt..mm.),
    lag_C_PPT = ppt..mm. + lagPPT
  ) %>%
  dplyr::select(Name, date, ppt..mm., lag_C_PPT)

new_df_hydro <- flow_dat %>%
  left_join(ppt_dat, by = c("date", "Name"))

analysis_start <- as.Date("2024-09-25")
analysis_end   <- as.Date("2025-12-15")

hydro_subset <- new_df_hydro %>%
  transmute(
    site = as.character(site),
    date = as.Date(date),
    flow = as.numeric(flow),
    Name = Name,
    ppt..mm. = as.numeric(ppt..mm.),
    lag_C_PPT = as.numeric(lag_C_PPT),
    analysis_year = 1
  ) %>%
  filter(date >= analysis_start, date <= analysis_end) %>%
  arrange(site, date) %>%
  mutate(yr= year(date))

# =========================================================
# 2. Functional flow identification function
# =========================================================

identify_functional_flows <- function(df_site_year,
                                      fall_rise_ratio = 1.15,
                                      min_fall_peak_quantile = 0.5,
                                      peak_quantile = 0.98,
                                      feb_peak_quantile = 0.,
                                      recession_min_days = 7,
                                      dry_cv_threshold = 0.20,
                                      dry_quantile = 0.30,
                                      smoothing_k = 5,
                                      fall_months = c(8, 9, 10, 11),
                                      spring_months = c(2, 3, 4, 5, 6)) {
  
  df <- df_site_year %>%
    arrange(date) %>%
    mutate(
      flow = if_else(flow < 0, NA_real_, flow),
      lag1 = dplyr::lag(flow),
      pct_change = (flow - lag1) / pmax(lag1, 1e-8),
      flow_smooth = zoo::rollapply(
        flow,
        width = smoothing_k,
        FUN = function(x) mean(x, na.rm = TRUE),
        fill = NA_real_,
        align = "center",
        partial = TRUE
      ),
      roll7_slope = zoo::rollapply(
        flow_smooth,
        width = 7,
        align = "right",
        fill = NA_real_,
        FUN = function(x) {
          if (sum(!is.na(x)) < 5) return(NA_real_)
          coef(lm(x ~ seq_along(x)))[2]
        }
      ),
      roll7_cv = zoo::rollapply(
        flow,
        width = 7,
        align = "right",
        fill = NA_real_,
        FUN = function(x) {
          if (sum(!is.na(x)) < 5) return(NA_real_)
          sd(x, na.rm = TRUE) / max(mean(x, na.rm = TRUE), 1e-8)
        }
      ),
      gap_days = c(NA_integer_, as.integer(diff(date))),
      yr = lubridate::year(date),
      mo = lubridate::month(date)
    )
  
  n_obs <- nrow(df)
  
  if (n_obs < 30 || all(is.na(df$flow))) {
    return(df %>%
             mutate(
               component = NA_character_,
               peak_event_id = NA_integer_,
               fall_event_id = NA_integer_,
               spring_peak_flag = FALSE
             ))
  }
  
  site_i <- unique(df$site)[1]
  
  # ---------------------------------------------------------
  # A. Peak-flow events
  # General threshold + extra February allowance
  # ---------------------------------------------------------
  peak_thr <- stats::quantile(df$flow, probs = peak_quantile, na.rm = TRUE, names = FALSE)
  peak_flag_general <- !is.na(df$flow) & df$flow >= peak_thr
  
  # additional February peaks at all sites
  feb_idx <- which(df$mo == 2 & !is.na(df$flow))
  peak_flag_feb <- rep(FALSE, n_obs)
  
  if (length(feb_idx) >= 3) {
    feb_thr <- stats::quantile(df$flow[feb_idx], probs = feb_peak_quantile, na.rm = TRUE, names = FALSE)
    peak_flag_feb[feb_idx] <- df$flow[feb_idx] >= feb_thr
  }
  
  peak_flag <- peak_flag_general | peak_flag_feb
  
  peak_event_start <- peak_flag & !dplyr::lag(peak_flag, default = FALSE)
  peak_event_id <- if_else(peak_flag, cumsum(peak_event_start), NA_integer_)
  
  # ---------------------------------------------------------
  # B. Fall pulse events
  # Default fall window + site/date-specific overrides
  # ---------------------------------------------------------
  fall_window_default <- df$mo %in% fall_months
  
  fall_window_override <- rep(FALSE, n_obs)
  
  # winters_up: allow Nov-Dec 2024
  if (identical(site_i, "winters_up")) {
    fall_window_override <- fall_window_override |
      (df$yr == 2024 & df$mo %in% c(11, 12))
  }
  
  # ophir: allow Nov 2024
  if (identical(site_i, "ophir")) {
    fall_window_override <- fall_window_override |
      (df$yr == 2024 & df$mo %in% c(11,12))
  }
  
  fall_window <- fall_window_default | fall_window_override
  
  fall_thr <- stats::quantile(df$flow, probs = min_fall_peak_quantile, na.rm = TRUE, names = FALSE)
  
  fall_candidate_flag <- fall_window &
    !is.na(df$pct_change) &
    !is.na(df$flow) &
    df$pct_change >= (fall_rise_ratio - 1) &
    df$flow >= fall_thr
  
  fall_flag <- rep(FALSE, n_obs)
  fall_candidate_idx <- which(fall_candidate_flag)
  
  if (length(fall_candidate_idx) > 0) {
    for (i in fall_candidate_idx) {
      baseline_start <- max(1, i - 14)
      baseline_end   <- max(1, i - 1)
      
      baseline <- median(df$flow[baseline_start:baseline_end], na.rm = TRUE)
      if (!is.finite(baseline)) {
        baseline <- stats::quantile(df$flow, 0.25, na.rm = TRUE, names = FALSE)
      }
      
      j <- i
      while (j <= n_obs &&
             fall_window[j] &&
             !is.na(df$flow[j]) &&
             df$flow[j] > max(baseline * 1.1,
                              stats::quantile(df$flow, 0.35, na.rm = TRUE, names = FALSE))) {
        fall_flag[j] <- TRUE
        j <- j + 1
      }
    }
  }
  
  fall_event_start <- fall_flag & !dplyr::lag(fall_flag, default = FALSE)
  fall_event_id <- if_else(fall_flag, cumsum(fall_event_start), NA_integer_)
  
  # ---------------------------------------------------------
  # C. Spring peak and spring recession
  # ---------------------------------------------------------
  spring_window <- which(df$mo %in% spring_months)
  
  spring_peak <- if (length(spring_window) > 0 &&
                     any(!is.na(df$flow_smooth[spring_window]))) {
    spring_window[which.max(df$flow_smooth[spring_window])]
  } else if (any(!is.na(df$flow_smooth))) {
    which.max(df$flow_smooth)
  } else {
    NA_integer_
  }
  
  recession_start <- NA_integer_
  recession_end   <- NA_integer_
  
  if (!is.na(spring_peak) && spring_peak < (n_obs - recession_min_days)) {
    candidate_start <- seq(spring_peak, n_obs - recession_min_days)
    
    is_recession_start <- vapply(candidate_start, function(i) {
      idx <- i:min(i + recession_min_days - 1, n_obs)
      vals <- df$flow_smooth[idx]
      difs <- diff(vals)
      
      if (sum(!is.na(vals)) < 5) return(FALSE)
      if (any(diff(df$date[idx]) > 1)) return(FALSE)
      
      prop_declining <- mean(difs <= 0, na.rm = TRUE)
      mean_slope <- mean(difs, na.rm = TRUE)
      
      prop_declining >= 0.75 && mean_slope < 0
    }, logical(1))
    
    if (any(is_recession_start, na.rm = TRUE)) {
      recession_start <- candidate_start[which(is_recession_start)[1]]
    }
    
    if (!is.na(recession_start)) {
      low_flow_thr <- stats::quantile(df$flow, probs = dry_quantile, na.rm = TRUE, names = FALSE)
      
      end_candidates <- seq(recession_start, n_obs)[
        !is.na(df$roll7_cv[seq(recession_start, n_obs)]) &
          !is.na(df$flow[seq(recession_start, n_obs)]) &
          df$flow[seq(recession_start, n_obs)] <= low_flow_thr &
          df$roll7_cv[seq(recession_start, n_obs)] <= dry_cv_threshold
      ]
      
      if (length(end_candidates) > 0) {
        recession_end <- min(end_candidates)
      }
    }
  }
  
  spring_flag <- rep(FALSE, n_obs)
  if (!is.na(recession_start) && !is.na(recession_end) && recession_start <= recession_end) {
    spring_flag[recession_start:recession_end] <- TRUE
  }
  
  # ---------------------------------------------------------
  # D. Final month windows for assignment
  # ---------------------------------------------------------
  wet_month_flag    <- df$mo %in% c(11, 12, 1, 2, 3)
  spring_month_flag <- df$mo %in% c(3, 4, 5, 6, 7)
  dry_month_flag    <- df$mo %in% c(8, 9, 10, 11)
  
  dry_flag <- rep(FALSE, n_obs)
  if (!is.na(recession_end) && recession_end < n_obs) {
    dry_flag[(recession_end + 1):n_obs] <- TRUE
  }
  dry_flag <- dry_flag & dry_month_flag
  
  spring_flag <- spring_flag & spring_month_flag
  
  # ---------------------------------------------------------
  # E. Assign components with priority
  # ---------------------------------------------------------
  component <- rep(NA_character_, n_obs)
  
  component[wet_month_flag] <- "wet_season_baseflow"
  component[dry_month_flag] <- "dry_season_baseflow"
  
  component[spring_flag] <- "spring_recession_flow"
  component[peak_flag]   <- "peak_flow"
  component[fall_flag]   <- "fall_pulse_flow"
  
  low_flow_thr <- stats::quantile(df$flow, probs = dry_quantile, na.rm = TRUE, names = FALSE)
  
  component[is.na(component) & dry_month_flag] <- "dry_season_baseflow"
  component[is.na(component) & wet_month_flag] <- "wet_season_baseflow"
  component[is.na(component) & spring_month_flag] <- "spring_recession_flow"
  component[is.na(component) & !is.na(df$flow) & df$flow <= low_flow_thr] <- "dry_season_baseflow"
  component[is.na(component)] <- "wet_season_baseflow"
  
  df %>%
    mutate(
      component = component,
      peak_event_id = peak_event_id,
      fall_event_id = fall_event_id,
      spring_peak_flag = seq_len(nrow(df)) == spring_peak
    ) %>%
    dplyr::select(-yr, -mo)
}

# =========================================================
# 3. Apply by site
# =========================================================

hydro_labeled <- hydro_subset %>%
  group_by(site, analysis_year) %>%
  group_modify(~ identify_functional_flows(.x)) %>%
  ungroup() %>%
  arrange(site, analysis_year, date) %>%
  group_by(site, analysis_year) %>%
  mutate(
    component_change = component != lag(component, default = first(component)),
    component_run_id = cumsum(component_change) + 1L
  ) %>%
  ungroup()

### maybe ..
hydro_labeled %>%
  dplyr::mutate(month = lubridate::month(date)) %>%
  dplyr::filter(
    (component == "fall_pulse_flow" & !month %in% c(8, 9, 10, 11)) |
      (component == "wet_season_baseflow" & !month %in% c(11, 12, 1, 2, 3)) |
      (component == "spring_recession_flow" & !month %in% c(3, 4, 5, 6, 7)) |
      (component == "dry_season_baseflow" & !month %in% c(8, 9, 10, 11))
  )

# =========================================================
# 4. Metrics by contiguous component run within each site
# =========================================================

calc_functional_metrics <- function(df) {
  
  df <- df %>%
    dplyr::ungroup() %>%
    tibble::as_tibble()
  
  needed_cols <- c(
    "site", "analysis_year", "date", "flow", "component",
    "peak_event_id", "fall_event_id", "component_run_id"
  )
  missing_cols <- setdiff(needed_cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  get_start_date <- function(x) {
    if (all(is.na(x))) as.Date(NA) else min(x, na.rm = TRUE)
  }
  
  get_end_date <- function(x) {
    if (all(is.na(x))) as.Date(NA) else max(x, na.rm = TRUE)
  }
  
  component_metrics <- df %>%
    dplyr::group_by(.data$site, .data$analysis_year, .data$component, .data$component_run_id) %>%
    dplyr::summarise(
      start_date = get_start_date(.data$date),
      end_date   = get_end_date(.data$date),
      duration_days = as.integer(end_date - start_date + 1),
      n_days = length(.data$date),
      start_flow = dplyr::first(.data$flow),
      end_flow   = dplyr::last(.data$flow),
      peak_flow = max(.data$flow, na.rm = TRUE),
      median_flow = median(.data$flow, na.rm = TRUE),
      p10_flow = as.numeric(stats::quantile(.data$flow, 0.10, na.rm = TRUE, names = FALSE)),
      p50_flow = as.numeric(stats::quantile(.data$flow, 0.50, na.rm = TRUE, names = FALSE)),
      p90_flow = as.numeric(stats::quantile(.data$flow, 0.90, na.rm = TRUE, names = FALSE)),
      cv_flow  = stats::sd(.data$flow, na.rm = TRUE) / max(mean(.data$flow, na.rm = TRUE), 1e-8),
      mean_pct_change_per_day = mean(
        (.data$flow - dplyr::lag(.data$flow)) / pmax(dplyr::lag(.data$flow), 1e-8),
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  peak_event_metrics <- df %>%
    dplyr::filter(.data$component == "peak_flow", !is.na(.data$peak_event_id)) %>%
    dplyr::group_by(.data$site, .data$analysis_year, .data$peak_event_id) %>%
    dplyr::summarise(
      event_start = min(.data$date),
      event_end   = max(.data$date),
      event_duration_days = as.integer(event_end - event_start + 1),
      event_peak_flow = max(.data$flow, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(.data$site, .data$analysis_year) %>%
    dplyr::summarise(
      peak_frequency = length(.data$peak_event_id),
      peak_cumulative_duration_days = sum(.data$event_duration_days, na.rm = TRUE),
      max_peak_event_flow = max(.data$event_peak_flow, na.rm = TRUE),
      .groups = "drop"
    )
  
  fall_event_metrics <- df %>%
    dplyr::filter(.data$component == "fall_pulse_flow", !is.na(.data$fall_event_id)) %>%
    dplyr::group_by(.data$site, .data$analysis_year, .data$fall_event_id) %>%
    dplyr::summarise(
      event_start = min(.data$date),
      event_end   = max(.data$date),
      event_duration_days = as.integer(event_end - event_start + 1),
      event_peak_flow = max(.data$flow, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(.data$site, .data$analysis_year) %>%
    dplyr::summarise(
      fall_pulse_frequency = length(.data$fall_event_id),
      fall_pulse_cumulative_duration_days = sum(.data$event_duration_days, na.rm = TRUE),
      max_fall_pulse_flow = max(.data$event_peak_flow, na.rm = TRUE),
      .groups = "drop"
    )
  
  list(
    component_metrics = component_metrics,
    peak_event_metrics = peak_event_metrics,
    fall_event_metrics = fall_event_metrics
  )
}

feb_peak_quantile = 0.95

metrics <- calc_functional_metrics(hydro_labeled)

component_metrics  <- metrics$component_metrics
peak_event_metrics <- metrics$peak_event_metrics
fall_event_metrics <- metrics$fall_event_metrics

# =========================================================
# 5. Final summary table
# =========================================================

functional_flow_summary <- component_metrics %>%
  dplyr::mutate(
    metric_magnitude = dplyr::case_when(
      .data$component == "fall_pulse_flow" ~ .data$peak_flow,
      .data$component == "wet_season_baseflow" ~ .data$p10_flow,
      .data$component == "spring_recession_flow" ~ .data$start_flow,
      .data$component == "dry_season_baseflow" ~ .data$p50_flow,
      .data$component == "peak_flow" ~ .data$peak_flow,
      TRUE ~ NA_real_
    )
  ) %>%
  dplyr::left_join(peak_event_metrics, by = c("site", "analysis_year")) %>%
  dplyr::left_join(fall_event_metrics, by = c("site", "analysis_year")) %>%
  dplyr::arrange(.data$site, .data$analysis_year, .data$start_date)

# =========================================================
# 6. Useful checks
# =========================================================

print(names(hydro_labeled))
print(dplyr::count(hydro_labeled, site, component))
print(dplyr::count(hydro_labeled, site, component, component_run_id))

functional_flow_summary

# write.csv(functional_flow_summary, "/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Processes_data/Davis_MS_fxnflow_v2.csv")
# write.csv(hydro_labeled, "/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Processes_data/Davis_MS_fxnflow_hydro_labeled.csv")


# =========================================================
# Revised plotting workflow
# =========================================================

library(dplyr)
library(tibble)
library(ggplot2)
library(ggnewscale)

# ---------------------------------------------------------
# 1. Plot settings / lookup tables
# ---------------------------------------------------------

site_order <- c(
  "ophir",
  "browns_sub",
  "browns",
  "winters_up",
  "winters_usgs"
)

site_colors <- c(
  "ophir" = "#7d5ba6",
  "winters_usgs" = "#4287f5",
  "winters_up" = "#7fc5f0",
  "davis" = "#d9cb7c",
  "browns" = "#c94d00",
  "browns_sub" = "#bd772d"
)

site_labs <- c(
  browns = "Browns",
  browns_sub = "Browns sub.",
  davis = "Davis",
  ophir = "Ophir",
  winters_up = "Winters up.",
  winters_usgs = "Winters USGS"
)

site_strip_labs <- c(
  ophir = "Ophir",
  browns_sub = "Browns sub.",
  browns = "Browns",
  winters_up = "Winters up.",
  winters_usgs = "Winters USGS"
)

# component_cols <- c(
#   fall_pulse_flow        = "#D55E00",
#   wet_season_baseflow    = "#0072B2",
#   peak_flow              = "#56B4E9",
#   spring_recession_flow  = "#009E73",
#   dry_season_baseflow    = "#CC79A7"
# )

component_cols <- c(
  fall_pulse_flow        = "#4D4D4D",  # dark gray
  wet_season_baseflow    = "#BDBDBD",  # light gray
  peak_flow              = "#000000",  # black (most prominent)
  spring_recession_flow  = "#737373",  # medium-dark gray
  dry_season_baseflow    = "#E0E0E0"   # very light gray
)

catchment_area_lookup <- tibble::tibble(
  site = c("ophir", "davis", "browns", "browns_sub", "winters_usgs", "winters_up"),
  catchment_area = c(50.1, 4.3, 9.8, 9.4, 5.4, 4.8)
)

plot_start <- as.Date("2024-09-05")
plot_end   <- as.Date("2025-12-15")

# ---------------------------------------------------------
# 2. Build plotting dataset
# ---------------------------------------------------------

df_plot_norm <- hydro_subset %>%
  dplyr::left_join(catchment_area_lookup, by = "site") %>%
  dplyr::mutate(
    date = as.Date(date),
    flow_norm = flow / catchment_area,
    site = factor(site, levels = site_order)
  ) %>%
  dplyr::filter(date >= plot_start, date <= plot_end)


# ensure test_dat is clean BEFORE join
if (exists("test_dat")) {
  test_keys <- test_dat %>%
    dplyr::mutate(
      site = as.character(site),
      date = as.Date(date)
    ) %>%
    dplyr::distinct(site, date) %>%
    dplyr::mutate(est_obs = "yes")
  
  df_plot_norm <- df_plot_norm %>%
    dplyr::left_join(test_keys, by = c("site", "date")) %>%
    dplyr::mutate(
      est_obs = dplyr::coalesce(est_obs, "no")
    )
} else {
  df_plot_norm <- df_plot_norm %>%
    dplyr::mutate(est_obs = "NA")
}


# ---------------------------------------------------------
# 3. Build functional-flow segments from hydro_labeled
# ---------------------------------------------------------

flow_segments <- hydro_labeled %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::filter(!is.na(.data$component), !is.na(.data$component_run_id)) %>%
  dplyr::arrange(.data$site, .data$analysis_year, .data$date) %>%
  dplyr::group_by(.data$site, .data$analysis_year, .data$component, .data$component_run_id) %>%
  dplyr::summarise(
    seg_start = min(.data$date, na.rm = TRUE),
    seg_end   = max(.data$date, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    seg_start = pmax(seg_start, plot_start),
    seg_end   = pmin(seg_end, plot_end),
    site = factor(site, levels = site_order)
  ) %>%
  dplyr::filter(seg_start <= seg_end)

# y-position for segment bars near bottom of each facet
segment_y <- df_plot_norm %>%
  dplyr::group_by(site) %>%
  dplyr::summarise(
    seg_y = max(flow_norm, na.rm = TRUE) * 0.04,
    .groups = "drop"
  )

flow_segments_plot <- flow_segments %>%
  dplyr::left_join(segment_y, by = "site")

# ---------------------------------------------------------
# 4. Rainfall scaling
# ---------------------------------------------------------

Q_top <- max(df_plot_norm$flow_norm, na.rm = TRUE) * 0.95

PPT_max <- max(df_plot_norm$ppt..mm., na.rm = TRUE)
if (!is.finite(PPT_max) || is.na(PPT_max) || PPT_max <= 0) {
  PPT_max <- 1
}

k <- Q_top / PPT_max

df_plot_ready <- df_plot_norm %>%
  dplyr::mutate(
    ppt..mm. = dplyr::coalesce(ppt..mm., 0),
    PPT_ymin = Q_top - k * ppt..mm. * 0.5,
    PPT_ymax = Q_top
  )

# ---------------------------------------------------------
# 5. Plot
# ---------------------------------------------------------

Baseflowplot_mod <- ggplot(df_plot_ready, aes(x = date)) +
  
  # ppt bars
  geom_rect(
    aes(
      xmin = date - 0.5,
      xmax = date + 0.5,
      ymin = PPT_ymin,
      ymax = PPT_ymax,
      fill = site
    ),
    alpha = 0.75,
    inherit.aes = FALSE
  ) +
  
  scale_fill_manual(
    values = site_colors,
    labels = site_labs,
    name = "Site"
  ) +
  
  # functional-flow segments
  ggnewscale::new_scale_color() +
  
  geom_segment(
    data = flow_segments_plot,
    aes(
      x = seg_start,
      xend = seg_end,
      y = seg_y,
      yend = seg_y,
      color = component
    ),
    inherit.aes = FALSE,
    linewidth = 1.75,
    lineend = "butt",
    alpha = 0.9
  ) +
  
  scale_color_manual(
    values = component_cols,
    name = "Functional flow\ncomponent",
    labels = c(
      fall_pulse_flow       = "Fall pulse",
      wet_season_baseflow   = "Wet-season baseflow",
      peak_flow             = "Peak flow",
      spring_recession_flow = "Spring recession",
      dry_season_baseflow   = "Dry-season baseflow"
    )
  ) +
  
  # hydrograph
  ggnewscale::new_scale_color() +
  
  geom_line(
    aes(y = flow_norm, color = site),
    linewidth = 0.9
  ) +
  
  scale_color_manual(
    values = site_colors,
    labels = site_labs,
    name = "Site"
  ) +
  
  # sample points (will only show if est_obs == "yes")
  geom_point(
    data = dplyr::filter(df_plot_ready, est_obs == "yes"),
    aes(x = date, y = flow_norm),
    shape = 4,
    fill = NA,
    color = "black",
    stroke = 1.2,
    size = 1.15,
    na.rm = TRUE
  ) +
  
  geom_vline(
    xintercept = as.Date("2024-09-25"),
    linetype = "dashed"
  ) +
  
  facet_grid(
    site ~ .,
    scales = "free_y",
    labeller = labeller(site = as_labeller(site_strip_labs))
  ) +
  
  scale_y_continuous(
    name = expression(Catchment~normalized~streamflow~(m^3~s^-1~km^-1)),
    sec.axis = sec_axis(
      trans = ~ (Q_top - .) / k,
      name  = "PPT (mm)"
    )
  ) +
  
  scale_x_date(
    limits = c(plot_start, plot_end),
    date_breaks = "1.75 month",
    date_labels = "%b-%y",
    expand = c(0, 0)
  ) +
  
  xlab("Date") +
  theme_classic() +
  theme(legend.position = "right")

Baseflowplot_mod
