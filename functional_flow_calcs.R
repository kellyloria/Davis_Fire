# Chem data

flow_dat <- read.csv("/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Processes_data/ERDC_water_hourlyflow.csv") %>%
  mutate(date=as.Date(date)) %>%  filter((site!="davis")) %>%
  mutate(Name = case_when(
    site== "ophir" ~ "ophir",
    site == "winters_usgs" ~ "winters",
    site == "winters_up" ~ "winters",
    site == "browns" ~ "browns", 
    site == "browns_sub" ~ "browns"))

str(flow_dat)

# Load precipitation data
ppt_dat <- read.csv("/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Raw_data/prism_cat_new.csv") %>%
  filter((Name!="davis")) %>%
  mutate(
    date = as.Date(Date, format = "%Y-%m-%d"),
    lagPPT = lag(ppt..mm.),
    lag_C_PPT = ppt..mm. + lagPPT) %>%
  dplyr::select(Name, date, ppt..mm., lag_C_PPT)

new_df_hydro <-flow_dat %>%
  left_join(ppt_dat, by=c("date", "Name")) 


analysis_start <- as.Date("2024-09-25")
analysis_end   <- as.Date("2025-12-15")

hydro_subset <- new_df_hydro %>%
  filter(date >= analysis_start,
         date <= analysis_end)


hydro_subset <- hydro_subset %>%
  mutate(
    analysis_year = 1
  )

library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)
library(zoo)


hydro <- new_df_hydro %>%
  transmute(
    site = as.character(site),
    date = as.Date(date),
    flow = as.numeric(flow)
  ) %>%
  arrange(site, date) %>%
  mutate(
    wy = if_else(month(date) >= 10, year(date) + 1L, year(date)),
    doy = yday(date)
  )

## A function to identify the 5 functional flow components for one site-year



identify_functional_flows <- function(df_site_year,
                                      fall_rise_ratio = 1.5,
                                      min_fall_peak_quantile = 0.60,
                                      peak_quantile = 0.98,
                                      recession_min_days = 7,
                                      dry_cv_threshold = 0.20,
                                      dry_quantile = 0.30) {
  df <- df_site_year %>%
    arrange(date) %>%
    mutate(
      flow = if_else(flow < 0, NA_real_, flow),
      lag1 = lag(flow),
      pct_change = (flow - lag1) / pmax(lag1, 1e-8),
      roll7_slope = zoo::rollapply(
        flow,
        width = 7,
        align = "right",
        fill = NA_real_,
        FUN = function(x) coef(lm(x ~ seq_along(x)))[2]
      ),
      roll7_cv = zoo::rollapply(
        flow,
        width = 7,
        align = "right",
        fill = NA_real_,
        FUN = function(x) sd(x, na.rm = TRUE) / max(mean(x, na.rm = TRUE), 1e-8)
      )
    )
  
  n <- nrow(df)
  if (n < 30 || all(is.na(df$flow))) {
    df$component <- NA_character_
    return(df)
  }
  
  # ----- 1. Peak flow days -----
  # For a 1-year record, use a high-flow threshold as a proxy event detector.
  # Replace later with externally derived 2-, 5-, 10-year thresholds if available.
  peak_thr <- quantile(df$flow, probs = peak_quantile, na.rm = TRUE)
  peak_idx <- which(df$flow >= peak_thr)
  
  # Merge consecutive peak days into events
  peak_event_id <- rep(NA_integer_, n)
  if (length(peak_idx) > 0) {
    event_breaks <- c(1, which(diff(peak_idx) > 1) + 1)
    event_ids <- rep(seq_along(event_breaks),
                     times = diff(c(event_breaks, length(peak_idx) + 1)))
    peak_event_id[peak_idx] <- event_ids
  }
  
  # ----- 2. Fall pulse -----
  # Look in Sep-Dec for first substantial pulse after low summer conditions
  fall_window <- which(month(df$date) %in% c(9, 10, 11, 12))
  fall_thr <- quantile(df$flow, probs = min_fall_peak_quantile, na.rm = TRUE)
  
  fall_candidates <- fall_window[
    which(
      !is.na(df$pct_change[fall_window]) &
        df$pct_change[fall_window] >= (fall_rise_ratio - 1) &
        df$flow[fall_window] >= fall_thr
    )
  ]
  
  fall_start <- if (length(fall_candidates) > 0) min(fall_candidates) else NA_integer_
  
  fall_end <- NA_integer_
  if (!is.na(fall_start)) {
    after_start <- seq(fall_start, n)
    # event ends when hydrograph returns to near pre-pulse / stops declining from pulse
    baseline <- median(df$flow[pmax(1, fall_start - 14):pmax(1, fall_start - 1)], na.rm = TRUE)
    end_candidates <- after_start[
      df$flow[after_start] <= max(baseline * 1.2, quantile(df$flow, 0.40, na.rm = TRUE))
    ]
    if (length(end_candidates) > 0) {
      fall_end <- min(end_candidates)
    }
  }
  
  # ----- 3. Spring recession -----
  # Find annual peak in roughly Feb-Jun, then identify sustained decline
  spring_window <- which(month(df$date) %in% c(2, 3, 4, 5, 6))
  spring_peak <- if (length(spring_window) > 0) {
    spring_window[which.max(df$flow[spring_window])]
  } else {
    which.max(df$flow)
  }
  
  recession_start <- NA_integer_
  recession_end   <- NA_integer_
  
  if (!is.na(spring_peak) && spring_peak < (n - recession_min_days)) {
    candidate_start <- seq(spring_peak, n - recession_min_days)
    
    is_recession_start <- vapply(candidate_start, function(i) {
      idx <- i:min(i + recession_min_days - 1, n)
      vals <- df$flow[idx]
      all(diff(vals) <= 0, na.rm = TRUE)
    }, logical(1))
    
    if (any(is_recession_start, na.rm = TRUE)) {
      recession_start <- candidate_start[which(is_recession_start)[1]]
    }
    
    if (!is.na(recession_start)) {
      low_flow_thr <- quantile(df$flow, probs = dry_quantile, na.rm = TRUE)
      end_candidates <- seq(recession_start, n)[
        !is.na(df$roll7_cv[seq(recession_start, n)]) &
          df$flow[seq(recession_start, n)] <= low_flow_thr &
          df$roll7_cv[seq(recession_start, n)] <= dry_cv_threshold
      ]
      if (length(end_candidates) > 0) {
        recession_end <- min(end_candidates)
      }
    }
  }
  
  # ----- 4. Dry-season baseflow -----
  dry_start <- if (!is.na(recession_end)) recession_end + 1 else NA_integer_
  dry_end   <- n
  
  # ----- 5. Wet-season baseflow -----
  # Between end of fall pulse and spring recession, excluding peak days
  wet_start <- if (!is.na(fall_end)) fall_end + 1 else NA_integer_
  wet_end   <- if (!is.na(recession_start)) recession_start - 1 else NA_integer_
  
  # ----- Assign labels -----
  component <- rep(NA_character_, n)
  
  if (!is.na(fall_start) && !is.na(fall_end) && fall_start <= fall_end) {
    component[fall_start:fall_end] <- "fall_pulse_flow"
  }
  
  if (!is.na(wet_start) && !is.na(wet_end) && wet_start <= wet_end) {
    component[wet_start:wet_end] <- "wet_season_baseflow"
  }
  
  if (length(peak_idx) > 0) {
    component[peak_idx] <- "peak_flow"
  }
  
  if (!is.na(recession_start) && !is.na(recession_end) && recession_start <= recession_end) {
    component[recession_start:recession_end] <- "spring_recession_flow"
  }
  
  if (!is.na(dry_start) && dry_start <= dry_end) {
    component[dry_start:dry_end] <- "dry_season_baseflow"
  }
  
  # Fill unlabeled days before fall pulse or between components
  # Prefer low-flow stable periods as dry; otherwise wet
  unlabeled <- which(is.na(component))
  if (length(unlabeled) > 0) {
    component[unlabeled] <- ifelse(
      df$flow[unlabeled] <= quantile(df$flow, dry_quantile, na.rm = TRUE),
      "dry_season_baseflow",
      "wet_season_baseflow"
    )
  }
  
  df %>%
    mutate(
      component = component,
      peak_event_id = peak_event_id
    )
}


## Apply the function to each site × water year

# hydro_labeled <- hydro %>%
# group_by(site, wy) %>%
#   group_modify(~ identify_functional_flows(.x)) %>%
#   ungroup()


hydro_labeled <- hydro_subset %>%
  group_by(site, analysis_year) %>%
  group_modify(~ identify_functional_flows(.x)) %>%
  ungroup()

## Compute Table 1-style metrics

calc_functional_metrics <- function(df) {
  
  get_start_date <- function(x) {
    if (all(is.na(x))) as.Date(NA) else min(x, na.rm = TRUE)
  }
  
  get_end_date <- function(x) {
    if (all(is.na(x))) as.Date(NA) else max(x, na.rm = TRUE)
  }
  
  # component-level metrics
  comp_metrics <- df %>%
    group_by(site, analysis_year, component) %>% # wy
    summarise(
      start_date = get_start_date(date),
      end_date   = get_end_date(date),
      duration_days = as.integer(end_date - start_date + 1),
      peak_flow = max(flow, na.rm = TRUE),
      median_flow = median(flow, na.rm = TRUE),
      p10_flow = quantile(flow, 0.10, na.rm = TRUE),
      p50_flow = quantile(flow, 0.50, na.rm = TRUE),
      p90_flow = quantile(flow, 0.90, na.rm = TRUE),
      cv_flow  = sd(flow, na.rm = TRUE) / max(mean(flow, na.rm = TRUE), 1e-8),
      mean_pct_change_per_day = mean(
        c(NA, diff(flow) / pmax(dplyr::lag(flow), 1e-8)[-1]),
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  # peak-flow event metrics (proxy for Table 1 peak-flow duration/frequency)
  peak_metrics <- df %>%
    filter(component == "peak_flow", !is.na(peak_event_id)) %>%
    group_by(site, analysis_year, peak_event_id) %>% #wy
    summarise(
      event_start = min(date),
      event_end   = max(date),
      event_duration_days = as.integer(event_end - event_start + 1),
      event_peak_flow = max(flow, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    group_by(site, analysis_year) %>% #wy
    summarise(
      peak_frequency = n(),
      peak_cumulative_duration_days = sum(event_duration_days),
      max_peak_event_flow = max(event_peak_flow, na.rm = TRUE),
      .groups = "drop"
    )
  
  list(
    component_metrics = comp_metrics,
    peak_event_metrics = peak_metrics
  )
}

## run scripts

metrics <- calc_functional_metrics(hydro_labeled)

metrics <- calc_functional_metrics(dplyr::ungroup(hydro_labeled))

component_metrics <- metrics$component_metrics
peak_event_metrics <- metrics$peak_event_metrics

## Produce a clean Table 1-style summary

functional_flow_summary <- component_metrics %>%
  mutate(
    metric_magnitude = case_when(
      component == "fall_pulse_flow" ~ peak_flow,
      component == "wet_season_baseflow" ~ p10_flow,
      component == "spring_recession_flow" ~ first(peak_flow),
      component == "dry_season_baseflow" ~ p50_flow,
      component == "peak_flow" ~ peak_flow,
      TRUE ~ NA_real_
    )
  ) %>%
  left_join(peak_event_metrics, by = c("site", "analysis_year")) %>% #wy
  arrange(site, analysis_year, component) #wy

functional_flow_summary

## Optional — make the output look closer to the paper’s five components

functional_flow_summary %>%
  select(
    site, component,
    start_date, end_date, duration_days,
    peak_flow, p10_flow, p50_flow, p90_flow,
    cv_flow, mean_pct_change_per_day,
    peak_frequency, peak_cumulative_duration_days
  ) %>%
  arrange(site, component)


# write.csv(functional_flow_summary, "/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Processes_data/Davis_MS_fxnflow.csv")


library(dplyr)

flow_segments <- hydro_labeled %>%
  mutate(date = as.Date(date)) %>%
  arrange(site, date) %>%
  group_by(site) %>%
  mutate(
    comp_change = component != lag(component, default = first(component)),
    seg_id = cumsum(comp_change)
  ) %>%
  group_by(site, seg_id, component) %>%
  summarise(
    seg_start = min(date, na.rm = TRUE),
    seg_end   = max(date, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(component))


# write.csv(functional_flow_summary, "/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Processes_data/Davis_MS_fxnflow_v2.csv")


# write.csv(hydro_labeled, "/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Processes_data/Davis_MS_fxnflow_hydro_labeled.csv")

library(dplyr)

flow_segments <- hydro_labeled %>%
  mutate(date = as.Date(date)) %>%
  arrange(site, date) %>%
  group_by(site) %>%
  mutate(
    comp_change = component != lag(component, default = first(component)),
    seg_id = cumsum(comp_change)
  ) %>%
  group_by(site, seg_id, component) %>%
  summarise(
    seg_start = min(date, na.rm = TRUE),
    seg_end   = max(date, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(component))


plot_start <- as.Date("2024-09-05")
plot_end   <- as.Date("2025-12-15")

flow_segments_plot <- flow_segments %>%
  mutate(
    seg_start = pmax(seg_start, plot_start),
    seg_end   = pmin(seg_end, plot_end)
  ) %>%
  filter(seg_start <= seg_end) %>%
  mutate(
    site = factor(site, levels = site_order)
  )


component_cols <- c(
  fall_pulse_flow        = "#D55E00",
  wet_season_baseflow    = "#0072B2",
  peak_flow              = "#56B4E9",
  spring_recession_flow  = "#009E73",
  dry_season_baseflow    = "#CC79A7"
)


segment_y <- df_plot_norm %>%
  group_by(site) %>%
  summarise(
    seg_y = max(flow_norm, na.rm = TRUE) * 0.04,
    .groups = "drop"
  )

flow_segments_plot <- flow_segments_plot %>%
  left_join(segment_y, by = "site")




library(ggplot2)
library(ggnewscale)
library(dplyr)

Baseflowplot_mod <- local({
  
  df_plot_ppt <- df_plot_norm
  Q_top <- max(df_plot_norm$flow_norm, na.rm = TRUE) * 0.95
  PPT_max <- max(df_plot_norm$ppt..mm., na.rm = TRUE)
  k <- Q_top / PPT_max
  
  df <- df_plot_norm %>%
    mutate(
      PPT_ymin = Q_top - k * ppt..mm. * 0.5,
      PPT_ymax = Q_top
    )
  
  ggplot(df, aes(x = date)) +
    
    # geom_rect(
    #   data = event_rects,
    #   aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    #   inherit.aes = FALSE,
    #   fill = "grey70",
    #   alpha = 0.5
    # ) +
    # 
    geom_rect(
      aes(
        xmin = date - 0.5, xmax = date + 0.5,
        ymin = PPT_ymin, ymax = PPT_ymax,
        fill = site
      ),
      alpha = 0.75,
      inherit.aes = FALSE
    ) +
    
    scale_fill_manual(
      values = c(site_colors),
      labels = site_labs,
      name = "Site"
    ) +
    
    geom_line(aes(y = flow_norm, color = site), linewidth = 0.9) +
    
    scale_color_manual(
      values = site_colors,
      labels = site_labs,
      name = "Site"
    ) +
    
    geom_point(
      data = df %>% filter(est_obs == "yes"),
      aes(x = date, y = flow_norm),
      shape = 4,
      fill = NA,
      color = "black",
      stroke = 1.2,
      size = 1.15,
      na.rm = TRUE
    ) +
    
    geom_vline(xintercept = as.Date("2024-09-25"), linetype = "dashed") +
    
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
      linewidth = 1.2,
      lineend = "butt"
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
      limits = c(as.Date("2024-09-05"), as.Date("2025-12-15")),
      date_breaks = "1.75 month",
      date_labels = "%b-%y",
      expand = c(0, 0)
    ) +
    
    xlab("Date") +
    theme_classic() +
    theme(legend.position = "right")
})

Baseflowplot_mod


Baseflowplot_mod <- local({
  
  df_plot_ppt <- df_plot_norm
  Q_top <- max(df_plot_norm$flow_norm, na.rm = TRUE) * 0.95
  PPT_max <- max(df_plot_norm$ppt..mm., na.rm = TRUE)
  k <- Q_top / PPT_max
  
  df <- df_plot_norm %>%
    mutate(
      PPT_ymin = Q_top - k * ppt..mm. * 0.5,
      PPT_ymax = Q_top
    )
  
  ggplot(df, aes(x = date)) +
    
    # 1. background storm windows
    geom_rect(
      data = event_rects,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
      inherit.aes = FALSE,
      fill = "grey70",
      alpha = 0.5
    ) +
    
    # 2. ppt bars
    geom_rect(
      aes(
        xmin = date - 0.5, xmax = date + 0.5,
        ymin = PPT_ymin, ymax = PPT_ymax,
        fill = site
      ),
      alpha = 0.75,
      inherit.aes = FALSE
    ) +
    
    scale_fill_manual(
      values = c(site_colors),
      labels = site_labs,
      name = "Site"
    ) +
    
    # 3. reset color scale, then draw functional-flow segments
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
      linewidth = 3.2,
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
    
    # 4. reset color scale again for site-colored hydrograph
    ggnewscale::new_scale_color() +
    
    geom_line(aes(y = flow_norm, color = site), linewidth = 0.9) +
    
    scale_color_manual(
      values = site_colors,
      labels = site_labs,
      name = "Site"
    ) +
    
    # 5. sample points above line
    geom_point(
      data = df %>% filter(est_obs == "yes"),
      aes(x = date, y = flow_norm),
      shape = 4,
      fill = NA,
      color = "black",
      stroke = 1.2,
      size = 1.15,
      na.rm = TRUE
    ) +
    
    geom_vline(xintercept = as.Date("2024-09-25"), linetype = "dashed") +
    
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
      limits = c(as.Date("2024-09-05"), as.Date("2025-12-15")),
      date_breaks = "1.75 month",
      date_labels = "%b-%y",
      expand = c(0, 0)
    ) +
    
    xlab("Date") +
    theme_classic() +
    theme(legend.position = "right")
})

Baseflowplot_mod




##### revised summer baseflow fxn 


calc_functional_metrics <- function(df) {
  
  df <- df %>%
    dplyr::ungroup() %>%
    tibble::as_tibble()
  
  needed_cols <- c("site", "analysis_year", "date", "flow", "component", "peak_event_id")
  missing_cols <- setdiff(needed_cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop(
      paste("Missing required columns:", paste(missing_cols, collapse = ", "))
    )
  }
  
  get_start_date <- function(x) {
    if (all(is.na(x))) as.Date(NA) else min(x, na.rm = TRUE)
  }
  
  get_end_date <- function(x) {
    if (all(is.na(x))) as.Date(NA) else max(x, na.rm = TRUE)
  }
  
  comp_metrics <- df %>%
    dplyr::group_by(.data$site, .data$analysis_year, .data$component) %>%
    dplyr::summarise(
      start_date = get_start_date(.data$date),
      end_date   = get_end_date(.data$date),
      duration_days = as.integer(end_date - start_date + 1),
      peak_flow = max(.data$flow, na.rm = TRUE),
      median_flow = median(.data$flow, na.rm = TRUE),
      p10_flow = quantile(.data$flow, 0.10, na.rm = TRUE),
      p50_flow = quantile(.data$flow, 0.50, na.rm = TRUE),
      p90_flow = quantile(.data$flow, 0.90, na.rm = TRUE),
      cv_flow  = sd(.data$flow, na.rm = TRUE) / max(mean(.data$flow, na.rm = TRUE), 1e-8),
      mean_pct_change_per_day = mean(
        (.data$flow - dplyr::lag(.data$flow)) / pmax(dplyr::lag(.data$flow), 1e-8),
        na.rm = TRUE
      ),
      start_flow = dplyr::first(.data$flow),
      .groups = "drop"
    )
  
  peak_metrics <- df %>%
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
      peak_frequency = dplyr::n(),
      peak_cumulative_duration_days = sum(.data$event_duration_days, na.rm = TRUE),
      max_peak_event_flow = max(.data$event_peak_flow, na.rm = TRUE),
      .groups = "drop"
    )
  
  list(
    component_metrics = comp_metrics,
    peak_event_metrics = peak_metrics
  )
}


hydro_labeled <- hydro_subset %>%
  dplyr::group_by(site, analysis_year) %>%
  dplyr::group_modify(~ identify_functional_flows(.x)) %>%
  dplyr::ungroup()

names(hydro_labeled)
dplyr::glimpse(hydro_labeled)

metrics <- calc_functional_metrics(hydro_labeled)

component_metrics <- metrics$component_metrics
peak_event_metrics <- metrics$peak_event_metrics


hydro_subset <- new_df_hydro %>%
  filter(date >= analysis_start,
         date <= analysis_end)


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
  filter(date >= analysis_start,
         date <= analysis_end) %>%
  arrange(site, date)


component == "spring_recession_flow" ~ first(peak_flow)


start_flow = dplyr::first(.data$flow)



functional_flow_summary <- component_metrics %>%
  mutate(
    metric_magnitude = dplyr::case_when(
      component == "fall_pulse_flow" ~ peak_flow,
      component == "wet_season_baseflow" ~ p10_flow,
      component == "spring_recession_flow" ~ start_flow,
      component == "dry_season_baseflow" ~ p50_flow,
      component == "peak_flow" ~ peak_flow,
      TRUE ~ NA_real_
    )
  ) %>%
  dplyr::left_join(peak_event_metrics, by = c("site", "analysis_year")) %>%
  dplyr::arrange(site, analysis_year, component)


#######

#####
####


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
  arrange(site, date)

# =========================================================
# 2. Functional flow identification function
# =========================================================

identify_functional_flows <- function(df_site_year,
                                      fall_rise_ratio = 1.25,
                                      min_fall_peak_quantile = 0.50,
                                      peak_quantile = 0.98,
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
      lag1 = lag(flow),
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
      gap_days = c(NA_integer_, as.integer(diff(date)))
    )
  
  n <- nrow(df)
  
  if (n < 30 || all(is.na(df$flow))) {
    return(df %>%
             mutate(
               component = NA_character_,
               peak_event_id = NA_integer_,
               fall_event_id = NA_integer_,
               spring_peak_flag = FALSE
             ))
  }
  
  # ---------------------------------------------------------
  # A. Peak-flow events (multiple allowed)
  # ---------------------------------------------------------
  peak_thr <- quantile(df$flow, probs = peak_quantile, na.rm = TRUE)
  peak_flag <- !is.na(df$flow) & df$flow >= peak_thr
  
  peak_event_start <- peak_flag & !lag(peak_flag, default = FALSE)
  peak_event_id <- if_else(peak_flag, cumsum(peak_event_start), NA_integer_)
  
  # ---------------------------------------------------------
  # B. Fall pulse events (multiple allowed, constrained to fall months)
  # ---------------------------------------------------------
  fall_window <- month(df$date) %in% fall_months
  fall_thr <- quantile(df$flow, probs = min_fall_peak_quantile, na.rm = TRUE)
  
  fall_candidate_flag <- fall_window &
    !is.na(df$pct_change) &
    !is.na(df$flow) &
    df$pct_change >= (fall_rise_ratio - 1) &
    df$flow >= fall_thr
  
  # expand each fall candidate into a short pulse window until flow drops back
  fall_flag <- rep(FALSE, n)
  
  fall_candidate_idx <- which(fall_candidate_flag)
  
  if (length(fall_candidate_idx) > 0) {
    for (i in fall_candidate_idx) {
      baseline_start <- max(1, i - 14)
      baseline_end   <- max(1, i - 1)
      
      baseline <- median(df$flow[baseline_start:baseline_end], na.rm = TRUE)
      
      if (!is.finite(baseline)) baseline <- quantile(df$flow, 0.25, na.rm = TRUE)
      
      j <- i
      while (j <= n &&
             month(df$date[j]) %in% fall_months &&
             !is.na(df$flow[j]) &&
             df$flow[j] > max(baseline * 1.1, quantile(df$flow, 0.35, na.rm = TRUE))) {
        fall_flag[j] <- TRUE
        j <- j + 1
      }
    }
  }
  
  # merge contiguous fall days into events
  fall_event_start <- fall_flag & !lag(fall_flag, default = FALSE)
  fall_event_id <- if_else(fall_flag, cumsum(fall_event_start), NA_integer_)
  
  # ---------------------------------------------------------
  # C. Spring peak and spring recession (single seasonal period)
  # ---------------------------------------------------------
  spring_window <- which(month(df$date) %in% spring_months)
  
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
  
  if (!is.na(spring_peak) && spring_peak < (n - recession_min_days)) {
    candidate_start <- seq(spring_peak, n - recession_min_days)
    
    is_recession_start <- vapply(candidate_start, function(i) {
      idx <- i:min(i + recession_min_days - 1, n)
      
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
      low_flow_thr <- quantile(df$flow, probs = dry_quantile, na.rm = TRUE)
      
      end_candidates <- seq(recession_start, n)[
        !is.na(df$roll7_cv[seq(recession_start, n)]) &
          !is.na(df$flow[seq(recession_start, n)]) &
          df$flow[seq(recession_start, n)] <= low_flow_thr &
          df$roll7_cv[seq(recession_start, n)] <= dry_cv_threshold
      ]
      
      if (length(end_candidates) > 0) {
        recession_end <- min(end_candidates)
      }
    }
  }
  
  spring_flag <- rep(FALSE, n)
  if (!is.na(recession_start) && !is.na(recession_end) && recession_start <= recession_end) {
    spring_flag[recession_start:recession_end] <- TRUE
  }
  
  # ---------------------------------------------------------
  # D. Dry-season baseflow
  # ---------------------------------------------------------
  dry_flag <- rep(FALSE, n)
  if (!is.na(recession_end) && recession_end < n) {
    dry_flag[(recession_end + 1):n] <- TRUE
  }
  
  # ---------------------------------------------------------
  # E. Start with unlabeled days, then assign in priority order
  # ---------------------------------------------------------
  component <- rep(NA_character_, n)
  
  # wet-season default fills anything not otherwise classified
  component[] <- "wet_season_baseflow"
  
  # dry overrides wet
  component[dry_flag] <- "dry_season_baseflow"
  
  # spring overrides wet
  component[spring_flag] <- "spring_recession_flow"
  
  # peak overrides wet/spring if desired
  component[peak_flag] <- "peak_flow"
  
  # fall pulse overrides whatever is already there in fall months
  component[fall_flag] <- "fall_pulse_flow"
  
  # optionally force very low unlabeled early values to dry
  low_flow_thr <- quantile(df$flow, probs = dry_quantile, na.rm = TRUE)
  component[is.na(component) | (!fall_flag & !peak_flag & !spring_flag & !dry_flag & df$flow <= low_flow_thr)] <- 
    ifelse(df$flow[is.na(component) | (!fall_flag & !peak_flag & !spring_flag & !dry_flag & df$flow <= low_flow_thr)] <= low_flow_thr,
           "dry_season_baseflow",
           "wet_season_baseflow")
  
  df %>%
    mutate(
      component = component,
      peak_event_id = peak_event_id,
      fall_event_id = fall_event_id,
      spring_peak_flag = row_number() == spring_peak
    )
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

# =========================================================
# 4. Metrics by contiguous component run within each site
# =========================================================

calc_functional_metrics <- function(df) {
  
  df <- df %>%
    dplyr::ungroup() %>%
    tibble::as_tibble()
  
  needed_cols <- c("site", "analysis_year", "date", "flow", "component",
                   "peak_event_id", "component_run_id")
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
    group_by(site, analysis_year, component, component_run_id) %>%
    summarise(
      start_date = get_start_date(date),
      end_date   = get_end_date(date),
      duration_days = as.integer(end_date - start_date + 1),
      n_days = n(),
      start_flow = first(flow),
      end_flow   = last(flow),
      peak_flow = max(flow, na.rm = TRUE),
      median_flow = median(flow, na.rm = TRUE),
      p10_flow = quantile(flow, 0.10, na.rm = TRUE),
      p50_flow = quantile(flow, 0.50, na.rm = TRUE),
      p90_flow = quantile(flow, 0.90, na.rm = TRUE),
      cv_flow  = sd(flow, na.rm = TRUE) / max(mean(flow, na.rm = TRUE), 1e-8),
      mean_pct_change_per_day = mean(
        (flow - lag(flow)) / pmax(lag(flow), 1e-8),
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  peak_event_metrics <- df %>%
    filter(component == "peak_flow", !is.na(peak_event_id)) %>%
    group_by(site, analysis_year, peak_event_id) %>%
    summarise(
      event_start = min(date),
      event_end   = max(date),
      event_duration_days = as.integer(event_end - event_start + 1),
      event_peak_flow = max(flow, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(site, analysis_year) %>%
    summarise(
      peak_frequency = n(),
      peak_cumulative_duration_days = sum(event_duration_days, na.rm = TRUE),
      max_peak_event_flow = max(event_peak_flow, na.rm = TRUE),
      .groups = "drop"
    )
  
  fall_event_metrics <- df %>%
    filter(component == "fall_pulse_flow", !is.na(fall_event_id)) %>%
    group_by(site, analysis_year, fall_event_id) %>%
    summarise(
      event_start = min(date),
      event_end   = max(date),
      event_duration_days = as.integer(event_end - event_start + 1),
      event_peak_flow = max(flow, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(site, analysis_year) %>%
    summarise(
      fall_pulse_frequency = n(),
      fall_pulse_cumulative_duration_days = sum(event_duration_days, na.rm = TRUE),
      max_fall_pulse_flow = max(event_peak_flow, na.rm = TRUE),
      .groups = "drop"
    )
  
  list(
    component_metrics = component_metrics,
    peak_event_metrics = peak_event_metrics,
    fall_event_metrics = fall_event_metrics
  )
}

metrics <- calc_functional_metrics(hydro_labeled)

component_metrics  <- metrics$component_metrics
peak_event_metrics <- metrics$peak_event_metrics
fall_event_metrics <- metrics$fall_event_metrics

# =========================================================
# 5. Final summary table
# =========================================================

functional_flow_summary <- component_metrics %>%
  mutate(
    metric_magnitude = case_when(
      component == "fall_pulse_flow" ~ peak_flow,
      component == "wet_season_baseflow" ~ p10_flow,
      component == "spring_recession_flow" ~ start_flow,
      component == "dry_season_baseflow" ~ p50_flow,
      component == "peak_flow" ~ peak_flow,
      TRUE ~ NA_real_
    )
  ) %>%
  left_join(peak_event_metrics, by = c("site", "analysis_year")) %>%
  left_join(fall_event_metrics, by = c("site", "analysis_year")) %>%
  arrange(site, analysis_year, start_date)

# =========================================================
# 6. Useful checks
# =========================================================

print(names(hydro_labeled))
print(dplyr::count(hydro_labeled, site, component))
print(dplyr::count(hydro_labeled, site, component, component_run_id))

functional_flow_summary


###
fall_rise_ratio = 1.25
min_fall_peak_quantile = 0.50
peak_quantile = 0.98


#fall_rise_ratio = 1.5
min_fall_peak_quantile = 0.60

