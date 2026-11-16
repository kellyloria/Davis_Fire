ERDC Davis Fire MS
================
K. Loria
2026-11-16

- [1. Libraries and input checks](#1-libraries-and-input-checks)
- [2. Chemistry analytes and units](#2-chemistry-analytes-and-units)
- [3. Prepare continuous daily
  hydrology](#3-prepare-continuous-daily-hydrology)
- [4. Prepare water-chemistry
  observations](#4-prepare-water-chemistry-observations)
- [5. Estimate interval water volumes and solute
  loads](#5-estimate-interval-water-volumes-and-solute-loads)
- [6. DMC slope models and descriptive
  interpretation](#6-dmc-slope-models-and-descriptive-interpretation)
- [7. Plot DMCs](#7-plot-dmcs)
- [8. Residual diagnostics](#8-residual-diagnostics)
- [9. Export results (optional)](#9-export-results-optional)
  - [Flow time series](#flow-time-series)

## 1. Libraries and input checks

``` r
WC_dat <- read.csv(
  file.path(data_dir, "DFMS_water_quality_data_2026.csv")
) %>%
  mutate(
    datetime = ymd_hms(datetime, tz = "UTC"),
    datetime = with_tz(datetime, tzone = local_tz),
    date = as.Date(datetime, tz = local_tz),
    year = year(datetime),
    mon = month(datetime),
    jday = yday(datetime),
    burn_lab = case_when(
      site_lab == "ophir" ~ "unburned",
      site_lab %in% c(
        "davis",
        "winters_usgs",
        "winters_up",
        "browns",
        "browns_sub"
      ) ~ "burn",
      TRUE ~ NA_character_
    )
  )

# Sites used throughout the DMC workflow
sites_keep <- c("winters_usgs", "ophir")
site_labels <- c(winters_usgs = "Winters USGS", ophir = "Ophir")

# Set start and end deliberately: these dates define the cumulative-volume baseline.
analysis_start <- as.Date("2024-10-15")
analysis_end   <- as.Date("2026-05-14")
fire_date      <- as.Date("2024-09-25")  # containment date; used for plot color

# Set FALSE to exclude below-detection-limit substituted concentrations.
include_bdl <- TRUE
```

For each chemistry sample, the measured concentration is assumed
representative of the water volume **since the preceding chemistry
sample** (the first sample represents the period since the analysis
start date). Interval loads are estimates, not measured continuous
loads. A chemistry value with a missing discharge day in its interval is
**not** assigned a load; the default analysis starts a new complete
series after such a gap and does not silently sum across missing
intervals. The analysis uses one common observation window for both
sites and each analyte’s available chemistry observations.

## 2. Chemistry analytes and units

The original script used `TSS`, but the updated object has `TSS_QC`. The
source’s `togL` values were mislabeled as g/L conversions; here
everything is converted explicitly to **mg/L**, then mg/L × liters /
1000 gives grams.

``` r
## Analyte definitions and unit conversions
analytes <- tribble(
  ~analyte, ~col,       ~unit,  ~to_mg_L,
  "DOC",    "DOC_QC",    "mg/L", 1,
  "TOC",    "TOC_QC",    "mg/L", 1,
  "TDN",    "TDN_QC",    "mg/L", 1,
  "PO4_P",  "PO4.P_QC",  "mg/L", 1,
  "NO3_N",  "NO3.N_QC",  "mg/L", 1,
  "NH4_N",  "NH4.N_QC",  "mg/L", 1,
  "TSS",    "TSS_QC",    "mg/L", 1,
  "Al",     "Al_QC",     "µg/L", 0.001,
  "As",     "As_QC",     "µg/L", 0.001,
  "B",      "B_QC",      "µg/L", 0.001,
  "Ba",     "Ba_QC",     "µg/L", 0.001,
  "Ca",     "Ca_QC",     "µg/L", 0.001,
  "Cr",     "Cr_QC",     "µg/L", 0.001,
  "Fe",     "Fe_QC",     "µg/L", 0.001,
  "Mn",     "Mn_QC",     "µg/L", 0.001,
  "Pb",     "Pb_QC",     "µg/L", 0.001,
  "Sr",     "Sr_QC",     "µg/L", 0.001,
  "Zn",     "Zn_QC",     "µg/L", 0.001
)

# Check that all required analytes are present
missing_cols <- setdiff(analytes$col, names(WC_dat))

if (length(missing_cols) > 0) {
  stop(
    "Missing analyte columns: ",
    paste(missing_cols, collapse = ", ")
  )
}


stopifnot(nrow(analytes) > 0)

# Match lab detection flags, when available; a blank flag is not a BDL flag.
analytes <- analytes %>%
  mutate(dl_col = sub("_QC$", "_DL_lab", col),
         dl_col = if_else(dl_col %in% names(WC_dat), dl_col, NA_character_))
```

``` r
# Daily discharge and precipitation observations
df_hydro <- read.csv(
  file.path(data_dir, "DFMS_hydro_clim_WY2024_2026.csv")
) %>%
  mutate(
    date = as.Date(date, format = "%Y-%m-%d")
  )
```

## 3. Prepare continuous daily hydrology

Daily `Q` must be m3/s. `NA` is preserved; only a measured zero is
treated as zero

``` r
flow_daily <- df_hydro %>%
  transmute(site = tolower(trimws(as.character(site))),
            date = as.Date(date), Q = as.numeric(Q),
            water_year, precip_mm) %>%
  filter(site %in% sites_keep, between(date, analysis_start, analysis_end))

if (anyDuplicated(flow_daily[c("site", "date")])) {
  stop("Duplicate site/date combinations in df_hydro: resolve before estimating loads.")
}

flow_daily <- tidyr::expand_grid(
  site = sites_keep,
  date = seq(analysis_start, analysis_end, by = "day")
) %>%
  left_join(flow_daily, by = c("site", "date")) %>%
  arrange(site, date) %>%
  mutate(Q_daily_liters = if_else(!is.na(Q) & Q >= 0, Q * 86400 * 1000, NA_real_),
         missing_or_invalid_Q = is.na(Q_daily_liters))

flow_coverage <- flow_daily %>%
  group_by(site) %>%
  summarise(n_days = n(), n_valid_Q = sum(!missing_or_invalid_Q),
            pct_valid_Q = 100 * mean(!missing_or_invalid_Q),
            n_zero_Q = sum(Q == 0, na.rm = TRUE), .groups = "drop")
knitr::kable(flow_coverage, digits = 1)
```

| site         | n_days | n_valid_Q | pct_valid_Q | n_zero_Q |
|:-------------|-------:|----------:|------------:|---------:|
| ophir        |    577 |       577 |         100 |        2 |
| winters_usgs |    577 |       577 |         100 |        0 |

## 4. Prepare water-chemistry observations

``` r
# 4. Prepare chemistry observations
chem_wide <- WC_dat %>%
  mutate(
    site = site_lab,
    sample_id = row_number()
  ) %>%
  filter(
    site %in% sites_keep,
    between(date, analysis_start, analysis_end)
  ) %>%
  arrange(site, datetime)


# Detection limit flags
dl_lookup <- analytes %>%
  filter(!is.na(dl_col)) %>%
  select(analyte, dl_col)

flag_long <- chem_wide %>%
  select(
    sample_id,
    any_of(dl_lookup$dl_col)
  ) %>%
  pivot_longer(
    cols = all_of(dl_lookup$dl_col),
    names_to = "dl_col",
    values_to = "bdl_flag"
  ) %>%
  left_join(dl_lookup, by = "dl_col") %>%
  select(sample_id, analyte, bdl_flag)


# Individual chemistry observations
chem_long <- chem_wide %>%
  select(
    sample_id,
    site,
    date,
    datetime,
    storm,
    all_of(analytes$col)
  ) %>%
  pivot_longer(
    cols = all_of(analytes$col),
    names_to = "col",
    values_to = "concentration_raw"
  ) %>%
  left_join(analytes, by = "col") %>%
  left_join(
    flag_long,
    by = c("sample_id", "analyte")
  ) %>%
  mutate(
    concentration_raw = as.numeric(concentration_raw),
    concentration_mg_L = concentration_raw * to_mg_L,
    tsf = as.integer(date - fire_date)
  ) %>%
  filter(
    is.finite(concentration_mg_L),
    concentration_mg_L >= 0
  ) %>%
  {
    if (include_bdl) {
      .
    } else {
      filter(., bdl_flag != "yes" | is.na(bdl_flag))
    }
  } %>%
  arrange(site, analyte, datetime, sample_id)


# Daily representative concentrations
chem_daily <- chem_long %>%
  group_by(site, analyte, date) %>%
  summarise(
    concentration_mg_L = median(
      concentration_mg_L,
      na.rm = TRUE
    ),
    n_samples = n(),
    concentration_min = min(concentration_mg_L),
    concentration_max = max(concentration_mg_L),
    first_sample = min(datetime),
    last_sample = max(datetime),
    tsf = first(tsf),
    .groups = "drop"
  ) %>%
  mutate(
    multiple_samples = n_samples > 1
  ) %>%
  arrange(site, analyte, date)


# Review storm sampling
storm_daily_summary <- chem_daily %>%
  filter(multiple_samples) %>%
  arrange(site, analyte, date)

knitr::kable(
  storm_daily_summary,
  digits = 3
)
```

| site | analyte | date | concentration_mg_L | n_samples | concentration_min | concentration_max | first_sample | last_sample | tsf | multiple_samples |
|:---|:---|:---|---:|---:|---:|---:|:---|:---|---:|:---|
| ophir | Al | 2024-10-16 | 0.001 | 2 | 0.001 | 0.001 | 2024-10-16 14:14:00 | 2024-10-16 16:16:00 | 21 | TRUE |
| ophir | As | 2024-10-16 | 0.000 | 2 | 0.000 | 0.000 | 2024-10-16 14:14:00 | 2024-10-16 16:16:00 | 21 | TRUE |
| ophir | B | 2024-10-16 | 0.003 | 2 | 0.003 | 0.003 | 2024-10-16 14:14:00 | 2024-10-16 16:16:00 | 21 | TRUE |
| ophir | Ba | 2024-10-16 | 0.009 | 2 | 0.009 | 0.009 | 2024-10-16 14:14:00 | 2024-10-16 16:16:00 | 21 | TRUE |
| ophir | Ca | 2024-10-16 | 1.008 | 2 | 1.008 | 1.008 | 2024-10-16 14:14:00 | 2024-10-16 16:16:00 | 21 | TRUE |
| ophir | Cr | 2024-10-16 | 0.000 | 2 | 0.000 | 0.000 | 2024-10-16 14:14:00 | 2024-10-16 16:16:00 | 21 | TRUE |
| ophir | DOC | 2024-10-16 | 5.215 | 2 | 5.215 | 5.215 | 2024-10-16 14:14:00 | 2024-10-16 16:16:00 | 21 | TRUE |
| ophir | Fe | 2024-10-16 | 0.154 | 2 | 0.154 | 0.154 | 2024-10-16 14:14:00 | 2024-10-16 16:16:00 | 21 | TRUE |
| ophir | Mn | 2024-10-16 | 0.011 | 2 | 0.011 | 0.011 | 2024-10-16 14:14:00 | 2024-10-16 16:16:00 | 21 | TRUE |
| ophir | NH4_N | 2024-10-16 | 0.050 | 2 | 0.050 | 0.050 | 2024-10-16 14:14:00 | 2024-10-16 16:16:00 | 21 | TRUE |
| ophir | NO3_N | 2024-10-16 | 0.010 | 2 | 0.010 | 0.010 | 2024-10-16 14:14:00 | 2024-10-16 16:16:00 | 21 | TRUE |
| ophir | PO4_P | 2024-10-16 | 0.087 | 2 | 0.087 | 0.087 | 2024-10-16 14:14:00 | 2024-10-16 16:16:00 | 21 | TRUE |
| ophir | Pb | 2024-10-16 | 0.000 | 2 | 0.000 | 0.000 | 2024-10-16 14:14:00 | 2024-10-16 16:16:00 | 21 | TRUE |
| ophir | Sr | 2024-10-16 | 0.076 | 2 | 0.076 | 0.076 | 2024-10-16 14:14:00 | 2024-10-16 16:16:00 | 21 | TRUE |
| ophir | TDN | 2024-10-16 | 0.186 | 2 | 0.186 | 0.186 | 2024-10-16 14:14:00 | 2024-10-16 16:16:00 | 21 | TRUE |
| ophir | TSS | 2024-10-16 | 9.600 | 2 | 9.600 | 9.600 | 2024-10-16 14:14:00 | 2024-10-16 16:16:00 | 21 | TRUE |
| ophir | Zn | 2024-10-16 | 0.002 | 2 | 0.002 | 0.002 | 2024-10-16 14:14:00 | 2024-10-16 16:16:00 | 21 | TRUE |
| winters_usgs | Al | 2024-10-16 | 0.004 | 3 | 0.004 | 0.004 | 2024-10-16 11:00:00 | 2024-10-16 16:48:00 | 21 | TRUE |
| winters_usgs | As | 2024-10-16 | 0.000 | 3 | 0.000 | 0.000 | 2024-10-16 11:00:00 | 2024-10-16 16:48:00 | 21 | TRUE |
| winters_usgs | B | 2024-10-16 | 0.006 | 3 | 0.006 | 0.006 | 2024-10-16 11:00:00 | 2024-10-16 16:48:00 | 21 | TRUE |
| winters_usgs | Ba | 2024-10-16 | 0.012 | 3 | 0.012 | 0.012 | 2024-10-16 11:00:00 | 2024-10-16 16:48:00 | 21 | TRUE |
| winters_usgs | Ca | 2024-10-16 | 1.356 | 3 | 1.356 | 1.356 | 2024-10-16 11:00:00 | 2024-10-16 16:48:00 | 21 | TRUE |
| winters_usgs | Cr | 2024-10-16 | 0.000 | 3 | 0.000 | 0.000 | 2024-10-16 11:00:00 | 2024-10-16 16:48:00 | 21 | TRUE |
| winters_usgs | DOC | 2024-10-16 | 5.100 | 3 | 5.100 | 5.100 | 2024-10-16 11:00:00 | 2024-10-16 16:48:00 | 21 | TRUE |
| winters_usgs | Fe | 2024-10-16 | 0.289 | 3 | 0.289 | 0.289 | 2024-10-16 11:00:00 | 2024-10-16 16:48:00 | 21 | TRUE |
| winters_usgs | Mn | 2024-10-16 | 0.034 | 3 | 0.034 | 0.034 | 2024-10-16 11:00:00 | 2024-10-16 16:48:00 | 21 | TRUE |
| winters_usgs | NH4_N | 2024-10-16 | 0.050 | 3 | 0.050 | 0.050 | 2024-10-16 11:00:00 | 2024-10-16 16:48:00 | 21 | TRUE |
| winters_usgs | NO3_N | 2024-10-16 | 0.010 | 3 | 0.010 | 0.010 | 2024-10-16 11:00:00 | 2024-10-16 16:48:00 | 21 | TRUE |
| winters_usgs | PO4_P | 2024-10-16 | 0.056 | 3 | 0.056 | 0.056 | 2024-10-16 11:00:00 | 2024-10-16 16:48:00 | 21 | TRUE |
| winters_usgs | Pb | 2024-10-16 | 0.000 | 3 | 0.000 | 0.000 | 2024-10-16 11:00:00 | 2024-10-16 16:48:00 | 21 | TRUE |
| winters_usgs | Sr | 2024-10-16 | 0.081 | 3 | 0.081 | 0.081 | 2024-10-16 11:00:00 | 2024-10-16 16:48:00 | 21 | TRUE |
| winters_usgs | TDN | 2024-10-16 | 0.133 | 3 | 0.133 | 0.133 | 2024-10-16 11:00:00 | 2024-10-16 16:48:00 | 21 | TRUE |
| winters_usgs | TSS | 2024-10-16 | 23.100 | 3 | 23.100 | 23.100 | 2024-10-16 11:00:00 | 2024-10-16 16:48:00 | 21 | TRUE |
| winters_usgs | Zn | 2024-10-16 | 0.004 | 3 | 0.004 | 0.004 | 2024-10-16 11:00:00 | 2024-10-16 16:48:00 | 21 | TRUE |

## 5. Estimate interval water volumes and solute loads

For a sample on date `t_k`, interval volume includes days **after the
previous sample date through `t_k`**, inclusive. For the first sample it
begins on `analysis_start`. The number of observed daily Q values must
equal the number of calendar days in the interval. Missing intervals are
flagged and excluded from the contiguous DMC run; subsequent intervals
can start a new run. The first interval is only meaningful if the
assumed initial chemistry concentration represents the preceding
interval.

``` r
# One row per chemistry sample interval, using daily flow observations.

# 5. Estimate interval water volumes and solute loads
chem_intervals <- chem_daily %>%
  group_by(site, analyte) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    interval_start = lag(
      date,
      default = analysis_start - 1L
    ) + 1L,

    interval_end = date,

    expected_days = as.integer(
      interval_end - interval_start
    ) + 1L
  ) %>%
  ungroup()


# Calculate water volume for each interval
interval_flow <- chem_intervals %>%
  select(
    site,
    analyte,
    date,
    interval_start,
    interval_end,
    expected_days
  ) %>%
  left_join(
    flow_daily %>%
      select(
        site,
        flow_date = date,
        Q_daily_liters
      ),
    by = "site",
    relationship = "many-to-many"
  ) %>%
  filter(
    flow_date >= interval_start,
    flow_date <= interval_end
  ) %>%
  group_by(site, analyte, date) %>%
  summarise(
    n_valid_Q = sum(!is.na(Q_daily_liters)),

    Q_interval_liters = sum(
      Q_daily_liters,
      na.rm = TRUE
    ),

    .groups = "drop"
  )


# Calculate solute loads
loads_all <- chem_intervals %>%
  left_join(
    interval_flow,
    by = c("site", "analyte", "date")
  ) %>%
  mutate(
    n_valid_Q = coalesce(n_valid_Q, 0L),

    complete_interval =
      n_valid_Q == expected_days &
      expected_days > 0,

    Q_interval_liters = if_else(
      complete_interval,
      Q_interval_liters,
      NA_real_
    ),

    load_interval_g =
      concentration_mg_L *
      Q_interval_liters / 1000
  ) %>%
  arrange(site, analyte, date) %>%
  group_by(site, analyte) %>%
  mutate(
    run_id = cumsum(!complete_interval),

    in_dmc =
      run_id == 0L &
      complete_interval
  ) %>%
  ungroup()


# QC summary
interval_qc <- loads_all %>%
  group_by(site, analyte) %>%
  summarise(
    n_intervals = n(),

    n_complete_intervals = sum(
      complete_interval
    ),

    n_in_dmc = sum(in_dmc),

    n_multiple_sample_days = sum(
      multiple_samples
    ),

    .groups = "drop"
  )

knitr::kable(interval_qc)
```

| site | analyte | n_intervals | n_complete_intervals | n_in_dmc | n_multiple_sample_days |
|:---|:---|---:|---:|---:|---:|
| ophir | Al | 27 | 27 | 27 | 1 |
| ophir | As | 27 | 27 | 27 | 1 |
| ophir | B | 27 | 27 | 27 | 1 |
| ophir | Ba | 27 | 27 | 27 | 1 |
| ophir | Ca | 27 | 27 | 27 | 1 |
| ophir | Cr | 26 | 26 | 26 | 1 |
| ophir | DOC | 27 | 27 | 27 | 1 |
| ophir | Fe | 27 | 27 | 27 | 1 |
| ophir | Mn | 27 | 27 | 27 | 1 |
| ophir | NH4_N | 25 | 25 | 25 | 1 |
| ophir | NO3_N | 24 | 24 | 24 | 1 |
| ophir | PO4_P | 24 | 24 | 24 | 1 |
| ophir | Pb | 26 | 26 | 26 | 1 |
| ophir | Sr | 27 | 27 | 27 | 1 |
| ophir | TDN | 27 | 27 | 27 | 1 |
| ophir | TOC | 23 | 23 | 23 | 0 |
| ophir | TSS | 23 | 23 | 23 | 1 |
| ophir | Zn | 27 | 27 | 27 | 1 |
| winters_usgs | Al | 27 | 27 | 27 | 1 |
| winters_usgs | As | 27 | 27 | 27 | 1 |
| winters_usgs | B | 27 | 27 | 27 | 1 |
| winters_usgs | Ba | 27 | 27 | 27 | 1 |
| winters_usgs | Ca | 27 | 27 | 27 | 1 |
| winters_usgs | Cr | 26 | 26 | 26 | 1 |
| winters_usgs | DOC | 27 | 27 | 27 | 1 |
| winters_usgs | Fe | 27 | 27 | 27 | 1 |
| winters_usgs | Mn | 27 | 27 | 27 | 1 |
| winters_usgs | NH4_N | 25 | 25 | 25 | 1 |
| winters_usgs | NO3_N | 25 | 25 | 25 | 1 |
| winters_usgs | PO4_P | 24 | 24 | 24 | 1 |
| winters_usgs | Pb | 26 | 26 | 26 | 1 |
| winters_usgs | Sr | 27 | 27 | 27 | 1 |
| winters_usgs | TDN | 27 | 27 | 27 | 1 |
| winters_usgs | TOC | 23 | 23 | 23 | 0 |
| winters_usgs | TSS | 23 | 23 | 23 | 1 |
| winters_usgs | Zn | 27 | 27 | 27 | 1 |

``` r
# The first incomplete interval stops the primary series. If it occurs early,
# change analysis_start to a date with complete Q coverage, then rerun.
dmc_long <- loads_all %>%
  filter(in_dmc) %>%
  group_by(site, analyte) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    Q_c_liters = cumsum(Q_interval_liters),

    load_cum_g = cumsum(load_interval_g),

    Q_c_norm = if (
      max(Q_c_liters) > 0
    ) {
      Q_c_liters / max(Q_c_liters)
    } else {
      NA_real_
    },

    load_c_norm = if (
      max(load_cum_g) > 0
    ) {
      load_cum_g / max(load_cum_g)
    } else {
      NA_real_
    }
  ) %>%
  ungroup() %>%
  mutate(
    site = factor(
      site,
      levels = sites_keep,
      labels = unname(site_labels[sites_keep])
    )
  )
```

## 6. DMC slope models and descriptive interpretation

The original script fits `load_c_norm ~ 0 + Q_c_norm`. Retained here for
comparability, with the important caveat that normalized cumulative data
are not independent. Interpret slopes above/below one as **relative
curvature summaries** (enrichment/dilution), not proof of a process. The
1:1 reference is also plotted.

``` r
fit_one_dmc <- function(dat) {
  dat <- filter(dat, is.finite(Q_c_norm), is.finite(load_c_norm))
  if (nrow(dat) < 3 || n_distinct(dat$Q_c_norm) < 2) {
    return(tibble(n_samples = nrow(dat), slope = NA_real_, std_error = NA_real_,
                  lwr = NA_real_, upr = NA_real_, p_vs_1 = NA_real_, r_squared = NA_real_))
  }
  fit <- lm(load_c_norm ~ 0 + Q_c_norm, data = dat)
  co <- summary(fit)$coefficients
  slope <- unname(co[1, "Estimate"])
  se <- unname(co[1, "Std. Error"])
  df <- df.residual(fit)
  tcrit <- qt(0.975, df)
  tibble(n_samples = nrow(dat), slope = slope, std_error = se,
         lwr = slope - tcrit * se, upr = slope + tcrit * se,
         p_vs_1 = 2 * pt(abs((slope - 1) / se), df, lower.tail = FALSE),
         r_squared = summary(fit)$r.squared)
}

models_tbl <- dmc_long %>%
  group_by(site, analyte) %>%
  group_modify(~fit_one_dmc(.x)) %>%
  ungroup() %>%
  mutate(interpretation = case_when(
    is.na(slope) ~ "Insufficient data",
    slope > 1 ~ "Relative enrichment (descriptive)",
    slope < 1 ~ "Relative dilution (descriptive)",
    TRUE ~ "Proportional"
  ))

knitr::kable(models_tbl %>% select(site, analyte, n_samples, slope, lwr, upr, p_vs_1, interpretation), digits = 3)
```

| site | analyte | n_samples | slope | lwr | upr | p_vs_1 | interpretation |
|:---|:---|---:|---:|---:|---:|---:|:---|
| Winters USGS | Al | 27 | 1.173 | 1.091 | 1.255 | 0.000 | Relative enrichment (descriptive) |
| Winters USGS | As | 27 | 1.012 | 0.980 | 1.044 | 0.462 | Relative enrichment (descriptive) |
| Winters USGS | B | 27 | 1.663 | 1.412 | 1.914 | 0.000 | Relative enrichment (descriptive) |
| Winters USGS | Ba | 27 | 1.054 | 1.039 | 1.070 | 0.000 | Relative enrichment (descriptive) |
| Winters USGS | Ca | 27 | 0.960 | 0.937 | 0.982 | 0.001 | Relative dilution (descriptive) |
| Winters USGS | Cr | 26 | 0.287 | 0.131 | 0.443 | 0.000 | Relative dilution (descriptive) |
| Winters USGS | DOC | 27 | 1.004 | 0.990 | 1.017 | 0.569 | Relative enrichment (descriptive) |
| Winters USGS | Fe | 27 | 1.061 | 0.973 | 1.149 | 0.166 | Relative enrichment (descriptive) |
| Winters USGS | Mn | 27 | 0.692 | 0.581 | 0.804 | 0.000 | Relative dilution (descriptive) |
| Winters USGS | NH4_N | 25 | 1.000 | 1.000 | 1.000 | 0.000 | Relative dilution (descriptive) |
| Winters USGS | NO3_N | 25 | 0.734 | 0.559 | 0.909 | 0.004 | Relative dilution (descriptive) |
| Winters USGS | PO4_P | 24 | 1.231 | 1.132 | 1.330 | 0.000 | Relative enrichment (descriptive) |
| Winters USGS | Pb | 26 | 0.462 | 0.320 | 0.603 | 0.000 | Relative dilution (descriptive) |
| Winters USGS | Sr | 27 | 0.977 | 0.960 | 0.994 | 0.009 | Relative dilution (descriptive) |
| Winters USGS | TDN | 27 | 0.868 | 0.802 | 0.935 | 0.000 | Relative dilution (descriptive) |
| Winters USGS | TOC | 23 | 1.024 | 1.009 | 1.039 | 0.003 | Relative enrichment (descriptive) |
| Winters USGS | TSS | 23 | 0.654 | 0.509 | 0.798 | 0.000 | Relative dilution (descriptive) |
| Winters USGS | Zn | 27 | 1.265 | 1.159 | 1.372 | 0.000 | Relative enrichment (descriptive) |
| Ophir | Al | 27 | 0.892 | 0.859 | 0.926 | 0.000 | Relative dilution (descriptive) |
| Ophir | As | 27 | 1.000 | 0.967 | 1.033 | 0.995 | Relative dilution (descriptive) |
| Ophir | B | 27 | 1.377 | 1.200 | 1.554 | 0.000 | Relative enrichment (descriptive) |
| Ophir | Ba | 27 | 0.982 | 0.966 | 0.998 | 0.032 | Relative dilution (descriptive) |
| Ophir | Ca | 27 | 0.984 | 0.966 | 1.003 | 0.100 | Relative dilution (descriptive) |
| Ophir | Cr | 26 | 0.249 | 0.096 | 0.402 | 0.000 | Relative dilution (descriptive) |
| Ophir | DOC | 27 | 0.923 | 0.907 | 0.940 | 0.000 | Relative dilution (descriptive) |
| Ophir | Fe | 27 | 1.086 | 1.050 | 1.122 | 0.000 | Relative enrichment (descriptive) |
| Ophir | Mn | 27 | 0.604 | 0.473 | 0.734 | 0.000 | Relative dilution (descriptive) |
| Ophir | NH4_N | 25 | 0.998 | 0.989 | 1.006 | 0.567 | Relative dilution (descriptive) |
| Ophir | NO3_N | 24 | 1.034 | 1.021 | 1.047 | 0.000 | Relative enrichment (descriptive) |
| Ophir | PO4_P | 24 | 1.023 | 0.986 | 1.059 | 0.215 | Relative enrichment (descriptive) |
| Ophir | Pb | 26 | 0.468 | 0.347 | 0.588 | 0.000 | Relative dilution (descriptive) |
| Ophir | Sr | 27 | 1.012 | 0.998 | 1.027 | 0.094 | Relative enrichment (descriptive) |
| Ophir | TDN | 27 | 0.898 | 0.866 | 0.931 | 0.000 | Relative dilution (descriptive) |
| Ophir | TOC | 23 | 0.921 | 0.903 | 0.940 | 0.000 | Relative dilution (descriptive) |
| Ophir | TSS | 23 | 1.178 | 1.073 | 1.283 | 0.002 | Relative enrichment (descriptive) |
| Ophir | Zn | 27 | 0.728 | 0.646 | 0.810 | 0.000 | Relative dilution (descriptive) |

## 7. Plot DMCs

``` r
plot_dmc <- function(analyte_name) {
  dat <- dmc_long %>% filter(analyte == analyte_name)
  mod <- models_tbl %>% filter(analyte == analyte_name, is.finite(slope)) %>%
    mutate(label = sprintf("β = %.2f", slope))
  ggplot(dat, aes(Q_c_norm, load_c_norm)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey65") +
    geom_abline(data = mod, aes(slope = slope, intercept = 0),
                inherit.aes = FALSE, color = "black", linewidth = 0.6) +
    geom_path(color = "grey55", linewidth = 0.7, na.rm = TRUE) +
    geom_point(aes(color = tsf), size = 2.5, na.rm = TRUE) +
    geom_text(data = mod, aes(x = 0.05, y = 0.94, label = label),
              inherit.aes = FALSE, hjust = 0, size = 4) +
    facet_wrap(~site, nrow = 1, drop = FALSE) +
    scale_color_viridis_c(option = "plasma", direction = -1, name = "Days since fire") +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    labs(x = "Norm. cumulative streamflow",
         y = paste("Norm. cumulative", analyte_name)) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
}

### better visual
# Updated DMC plotting function

plot_dmc <- function(analyte_name) {

  dat <- dmc_long %>%
    filter(analyte == analyte_name)

  mod <- models_tbl %>%
    filter(
      analyte == analyte_name,
      is.finite(slope)
    ) %>%
    mutate(
      label = sprintf("β = %.2f", slope)
    )

  ggplot(dat, aes(Q_c_norm, load_c_norm)) +

    # 1:1 reference line
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      color = "grey65",
      linewidth = 0.5
    ) +

    # Estimated slope
    geom_abline(
      data = mod,
      aes(slope = slope, intercept = 0),
      inherit.aes = FALSE,
      color = "black",
      linewidth = 0.6
    ) +

    # Cumulative trajectory
    geom_path(
      color = "grey55",
      linewidth = 0.6,
      na.rm = TRUE
    ) +

    # Observations
    geom_point(
      aes(color = tsf),
      size = 2,
      na.rm = TRUE
    ) +

    # Slope annotations
    geom_text(
      data = mod,
      aes(
        x = 0.05,
        y = 0.88,
        label = label
      ),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = 1,
      size = 2.7
    ) +

    # Site panels
    facet_wrap(
      ~site,
      nrow = 1,
      drop = FALSE
    ) +

    # Color scale
    scale_color_viridis_c(
      option = "plasma",
      direction = -1,
      name = "Days since fire"
    ) +

    # Consistent axes
    scale_x_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.5, 1),
      expand = expansion(mult = 0.02)
    ) +

    scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.5, 1),
      expand = expansion(mult = 0.02)
    ) +

    coord_equal() +

    # Individual panel title only
    labs(
      title = analyte_name,
      x = NULL,
      y = NULL
    ) +

    theme_minimal(base_size = 10) +

    theme(
      # Title
      plot.title = element_text(
        size = 13,
        face = "bold",
        hjust = 0.5,
        margin = margin(b = 6)
      ),

      # Facet labels
      strip.text = element_text(
        size = 9,
        face = "bold"
      ),

      # Axis tick labels
      axis.text = element_text(
        size = 8,
        color = "black"
      ),

      # Remove individual axis titles
      axis.title = element_blank(),

      # Grid
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(
        color = "grey90",
        linewidth = 0.3
      ),

      # Spacing between site facets
      panel.spacing.x = unit(0.25, "cm"),

      # Legend
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),

      # Margins
      plot.margin = margin(5, 5, 5, 5)
    )
}

###


dmc_TSS_plt <- plot_dmc("TSS")
dmc_DOC_plt <- plot_dmc("DOC")
dmc_TOC_plt <- plot_dmc("TOC")
dmc_TND_plt <- plot_dmc("TDN")
dmc_NO3_plt <- plot_dmc("NO3_N")
plot_dmc("NH4_N")
```

<img src="DMC_residual_panels_02_files/figure-gfm/plotting-functions-1.png" alt="" width="80%" />

``` r
dmc_PO4_plt <- plot_dmc("PO4_P")

dmc_Al_plt <- plot_dmc("Al")
plot_dmc("As")
```

<img src="DMC_residual_panels_02_files/figure-gfm/plotting-functions-2.png" alt="" width="80%" />

``` r
plot_dmc("B")
```

<img src="DMC_residual_panels_02_files/figure-gfm/plotting-functions-3.png" alt="" width="80%" />

``` r
dmc_Ba_plt <- plot_dmc("Ba")
dmc_Ca_plt <- plot_dmc("Ca")
plot_dmc("Cr")
```

<img src="DMC_residual_panels_02_files/figure-gfm/plotting-functions-4.png" alt="" width="80%" />

``` r
dmc_Fe_plt <- plot_dmc("Fe")
dmc_Pb_plt <- plot_dmc("Mn")
plot_dmc("Pb")
```

<img src="DMC_residual_panels_02_files/figure-gfm/plotting-functions-5.png" alt="" width="80%" />

``` r
dmc_Sr_plt <- plot_dmc("Sr")
plot_dmc("Zn")
```

<img src="DMC_residual_panels_02_files/figure-gfm/plotting-functions-6.png" alt="" width="80%" />

## 8. Residual diagnostics

These are residuals from the descriptive normalized DMC regression.
Their cumulative sum is a visual diagnostic only, not a formal
independent-error change-point test.

``` r
residuals_long <- dmc_long %>%
  left_join(models_tbl %>% select(site, analyte, slope), by = c("site", "analyte")) %>%
  mutate(fitted_norm = slope * Q_c_norm,
         resid_norm = load_c_norm - fitted_norm) %>%
  group_by(site, analyte) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(cumul_resid = cumsum(resid_norm)) %>%
  ungroup()

plot_dmc_residuals <- function(analyte_name) {
  residuals_long %>%
    filter(analyte == analyte_name) %>%
    ggplot(aes(date, cumul_resid)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey65") +
    geom_line(color = "grey45") +
    geom_point(aes(color = tsf), size = 2) +
    facet_wrap(~site, nrow = 1) + #scales = "free_y")
    scale_color_viridis_c(option = "plasma", direction = -1,
                          name = "Days since fire") +
    labs(title = paste("DMC residuals:", analyte_name),
         x = NULL, y = "Residual") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}



plot_dmc_residuals("TSS")
```

<img src="DMC_residual_panels_02_files/figure-gfm/residuals-1.png" alt="" width="80%" />

``` r
plot_dmc_residuals("DOC")
```

<img src="DMC_residual_panels_02_files/figure-gfm/residuals-2.png" alt="" width="80%" />

``` r
plot_dmc_residuals("TOC")
```

<img src="DMC_residual_panels_02_files/figure-gfm/residuals-3.png" alt="" width="80%" />

``` r
plot_dmc_residuals("TDN")
```

<img src="DMC_residual_panels_02_files/figure-gfm/residuals-4.png" alt="" width="80%" />

``` r
plot_dmc_residuals("NO3_N")
```

<img src="DMC_residual_panels_02_files/figure-gfm/residuals-5.png" alt="" width="80%" />

``` r
plot_dmc_residuals("NH4_N")
```

<img src="DMC_residual_panels_02_files/figure-gfm/residuals-6.png" alt="" width="80%" />

``` r
plot_dmc_residuals("PO4_P")
```

<img src="DMC_residual_panels_02_files/figure-gfm/residuals-7.png" alt="" width="80%" />

``` r
plot_dmc_residuals("Al")
```

<img src="DMC_residual_panels_02_files/figure-gfm/residuals-8.png" alt="" width="80%" />

``` r
plot_dmc_residuals("As")
```

<img src="DMC_residual_panels_02_files/figure-gfm/residuals-9.png" alt="" width="80%" />

``` r
plot_dmc_residuals("B")
```

<img src="DMC_residual_panels_02_files/figure-gfm/residuals-10.png" alt="" width="80%" />

``` r
plot_dmc_residuals("Ba")
```

<img src="DMC_residual_panels_02_files/figure-gfm/residuals-11.png" alt="" width="80%" />

``` r
plot_dmc_residuals("Ca")
```

<img src="DMC_residual_panels_02_files/figure-gfm/residuals-12.png" alt="" width="80%" />

``` r
plot_dmc_residuals("Cr")
```

<img src="DMC_residual_panels_02_files/figure-gfm/residuals-13.png" alt="" width="80%" />

``` r
plot_dmc_residuals("Fe")
```

<img src="DMC_residual_panels_02_files/figure-gfm/residuals-14.png" alt="" width="80%" />

``` r
plot_dmc_residuals("Mn")
```

<img src="DMC_residual_panels_02_files/figure-gfm/residuals-15.png" alt="" width="80%" />

``` r
plot_dmc_residuals("Pb")
```

<img src="DMC_residual_panels_02_files/figure-gfm/residuals-16.png" alt="" width="80%" />

``` r
plot_dmc_residuals("Sr")
```

<img src="DMC_residual_panels_02_files/figure-gfm/residuals-17.png" alt="" width="80%" />

``` r
plot_dmc_residuals("Zn")
```

<img src="DMC_residual_panels_02_files/figure-gfm/residuals-18.png" alt="" width="80%" />

``` r
####
dmc_TSS_res_plt <- plot_dmc_residuals("TSS")
dmc_DOC_res_plt <- plot_dmc_residuals("DOC")
dmc_TOC_res_plt <- plot_dmc_residuals("TOC")
dmc_TND_res_plt <- plot_dmc_residuals("TDN")
dmc_NO3_res_plt <- plot_dmc_residuals("NO3_N")

dmc_PO4_res_plt <- plot_dmc_residuals("PO4_P")

dmc_Al_res_plt <- plot_dmc_residuals("Al")
plot_dmc("As")
```

<img src="DMC_residual_panels_02_files/figure-gfm/residuals-19.png" alt="" width="80%" />

``` r
plot_dmc("B")
```

<img src="DMC_residual_panels_02_files/figure-gfm/residuals-20.png" alt="" width="80%" />

``` r
dmc_Ba_res_plt <- plot_dmc_residuals("Ba")
dmc_Ca_res_plt <- plot_dmc_residuals("Ca")
plot_dmc("Cr")
```

<img src="DMC_residual_panels_02_files/figure-gfm/residuals-21.png" alt="" width="80%" />

``` r
dmc_Fe_res_plt <- plot_dmc_residuals("Fe")
dmc_Pb_res_plt <- plot_dmc_residuals("Mn")
plot_dmc("Pb")
```

<img src="DMC_residual_panels_02_files/figure-gfm/residuals-22.png" alt="" width="80%" />

``` r
dmc_Sr_res_plt <- plot_dmc_residuals("Sr")
plot_dmc("Zn")
```

<img src="DMC_residual_panels_02_files/figure-gfm/residuals-23.png" alt="" width="80%" />

## 9. Export results (optional)

### Flow time series

<img src="DMC_residual_panels_02_files/figure-gfm/unnamed-chunk-3-1.png" alt="" width="99%" />
