---
title: "ERDC Davis Fire MS L-Q analysis"
subtitle: "Figures 2-4  box plots, double mass curves, coefficient estimates" 
author: "Kelly Loria"
date: "`r Sys.Date()`"
output:
  html_document:
    theme: united
    highlight: tango
  pdf_document: default
editor_options:
  markdown:
    wrap: 72
---

```{=html}
<style type="text/css">
body, td {font-size: 12px;}
code.r{font-size: 8px;}
pre {font-size: 10px}
</style>
```

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(
  fig.width  = 7,     # inches
  fig.height = 3,     # inches
  fig.retina = 2,     # sharper in HTML
  out.width  = "80%"  # “medium” on the page (HTML); can use "70%" etc.
)
knitr::opts_chunk$set(warning = F, message = F)
#knitr::opts_knit$set(root.dir = 'C/Users/kloria/Documents/Davis_data_exploration/')
def.chunk.hook  <- knitr::knit_hooks$get("chunk")
knitr::knit_hooks$set(chunk = function(x, options) {
  x <- def.chunk.hook(x, options)
  ifelse(options$size != "normalsize", paste0("\n \\", options$size,"\n\n", x, "\n\n \\normalsize"), x)
})

```

```{r, echo = F, message = F}
### Packages
#setwd("C/Users/kloria/Documents/Davis_data_exploration/")

library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(viridis)
library(cowplot)
library(zoo)
library(readxl)
library(ggpubr)
library(knitr)
library(kableExtra)
library(plotly)
library(readxl)
library(ggpmisc)
library(slider)
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(ggpubr)


site_colors <- c(
  "ophir" = "#7d5ba6",
  "winters_usgs" = "#4287f5",
  "winters_up" = "#7fc5f0",
  "davis" = "#d9cb7c",
  "browns" = "#c94d00", 
  "browns_sub"  = "#bd772d"
)

site_colors_OL <- c(
  "A_ophir" = "#7d5ba6",
  "F_winters_usgs" = "#4287f5",
  "E_winters_up" = "#7fc5f0",
  "B_davis" = "#d9cb7c",
  "D_browns" = "#c94d00",
  "C_browns_sub"  = "#bd772d"
)

# site_colors_OL <- c(
#   "ophir" = "#7d5ba6",
#   "winters_usgs" = "#4287f5",
#   "winters_up" = "#7fc5f0",
#   "davis" = "#d9cb7c",
#   "browns" = "#c94d00", 
#   "browns_sub"  = "#bd772d"
# )

ref_date <-c(as.Date("2024-09-25"))
```


```{r, eval=TRUE, echo = F, error=FALSE, warning=FALSE, message=FALSE}
##### Function for water year:
# fxn for water year
water_year <- function(data) {
  data %>%
    mutate(date = ymd(date)) %>%
    mutate(WaterYear = if_else(month(date) >= 10, year(date) + 1, year(date)))
}
```

```{r, warning=F, size = 'small', echo=FALSE, include=FALSE}
# Chem data

test_dat <- read.csv("/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Processes_data/ERDC_water_only_chem_dat_processed.csv") %>%
  dplyr::select(-site) %>%   
  rename(site = site_lab) %>%  
  filter(site !="davis") %>%
  mutate(
    date = as.Date(date),
    datetime = as.POSIXct(datetime) 
  ) %>%
    mutate(date=as.Date(date),
         datetime =as.POSIXct(datetime)) %>%
    mutate(burn_area = case_when(
    site== "ophir" ~ 0,
    site == "winters_usgs" ~ 65,
    site == "winters_up" ~ 62,
    site == "browns" ~ 42, 
    site == "browns_sub" ~ 41)) 

### Create a new column for Sr:B
test_dat <- test_dat%>%
  mutate(Sr_B_QC = c(Sr_QC/B_QC))


```

### ============================================================================

## I. Characterize hydroclimate condtions

### ============================================================================


 - How do sampling events reflect baseflow or runoff?
 
 - When did the largest transport events happen in each site?



```{r, warning=F, size = 'small', echo=FALSE, include=FALSE}
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

```

### Create variable that corresponds with PPT flow increase

Based on

1. Flow for any given day is elevated: (>μ(previous 7 days)+1⋅σ(previous 7 days))

2. Rain happened on that day or the prior day

```{r, warning=F, size = 'small', echo=T, include=T}

library(dplyr)
library(slider)

head(new_df_hydro)
### infill missing flow:
new_df_hydro <- new_df_hydro %>%
  arrange(site, date) %>%
  group_by(site) %>%
  mutate(
    flow_filled = na.approx(flow, x = date, na.rm = FALSE)) %>%
  ungroup()


N <- 3 # Set pre-event window length in days

df2 <- new_df_hydro %>%
  arrange(site, date) %>%
  group_by(site) %>%
  mutate(
    # use the filled flow for stability; swap to `flow` if needed
    Q = flow_filled,

    # Rolling stats of PRIOR N days (exclude current day by sliding then lagging)
    mean_preN = lag(slide_dbl(Q, ~mean(.x, na.rm = TRUE), .before = N-1, .complete = TRUE)),
    sd_preN   = lag(slide_dbl(Q, ~sd(.x,   na.rm = TRUE), .before = N-1, .complete = TRUE)),

    # condition (2): ppt today OR yesterday > 0
    ppt_recent = (ppt..mm. > 0) | (lag(lag_C_PPT, default = 0) > 0),

    # condition (1): flow elevated at least 1 SD above mean of prior N days
    Q_thresh = mean_preN + sd_preN,
    elevated =(Q >= Q_thresh),

    runoff = if_else(ppt_recent & elevated, "yes", "no"),

    dQ = Q - lag(Q),
    runoff_dQ = if_else(runoff == "yes", dQ, NA_real_)
  ) %>%
  ungroup()


df21 <- df2 %>%
 arrange(site, date) %>%
  group_by(site) %>%
  mutate(
    runoff_flag = dplyr::coalesce(runoff == "yes", FALSE),

    event_start = runoff_flag & !lag(runoff_flag, default = FALSE),
    event_id    = if_else(runoff_flag, cumsum(event_start), 0L),
    event_end   = runoff_flag & !lead(runoff_flag, default = FALSE)
  ) %>%
  group_by(site, event_id) %>%
  mutate(
    event_start_date = if_else(event_id > 0L, min(date), as.Date(NA)),
    event_end_date   = if_else(event_id > 0L, max(date), as.Date(NA)),
    day_in_event     = if_else(event_id > 0L, row_number(), NA_integer_)
  ) %>%
  ungroup()


```


```{r, warning=F, echo=FALSE, include=T}

min_Q_event <- c(0.00001)

df_events <- df21 %>%
  mutate(
    min_Q_event = if_else(is.na(min_Q_event), 0, min_Q_event),

    large_flow = !is.na(Q_thresh) &
                 Q >= Q_thresh &
                 Q >= min_Q_event
  ) %>%
  arrange(site, date) %>%
  group_by(site) %>%
  mutate(
    event_start_lf = large_flow & !lag(large_flow, default = FALSE),
    event_id_lf    = if_else(large_flow, cumsum(event_start_lf), 0L),
    event_end_lf   = large_flow & !lead(large_flow, default = FALSE)
  ) %>%
  ungroup()


event_summary <- df_events %>%
  filter(event_start_date< as.Date("2025-12-10")) %>%
  filter(event_id_lf > 0) %>%
  group_by(site, event_id_lf) %>%
  summarise(
    event_start_date = min(date),
    event_end_date   = max(date),
    duration_days    = as.integer(event_end_date - event_start_date) + 1,

    peak_flow        = max(Q, na.rm = TRUE),
    peak_flow_date   = date[which.max(Q)][1],

    # ppt summaries (safe even if ppt is all NA)
    total_ppt_mm     = sum(ppt..mm., na.rm = TRUE),
    peak_ppt_mm      = max(ppt..mm., na.rm = TRUE),

    .groups = "drop"
  ) %>%
  filter(total_ppt_mm>0)

event_summary
```


## Figure 2:

### Runoff and sampling timing as streamflow, sample dates, and ppt 


```{r, warning=F, size = 'small', echo=F, fig.width=9, fig.height=10.25}

library(dplyr)
library(ggplot2)

site_labs <- c(
browns = "Browns",
browns_sub = "Browns sub.",
davis = "Davis",
ophir = "Ophir",
winters_up = "Winters up.",
winters_usgs = "Winters USGS"
)
library(ggnewscale)


# 1) sampling keys (unique site-date combos)
test_keys <- test_dat %>%
  transmute(
    site   = site,
    date   = as.Date(date),
    est_obs = "yes"
  ) %>%
  distinct(site, date, .keep_all = TRUE)

# 2) join + fill non-sampled days
df_plot <- df2 %>%
  mutate(date = as.Date(date)) %>%
  full_join(test_keys, by = c("site", "date")) %>%
  mutate(est_obs = if_else(is.na(est_obs), "no", est_obs))

site_order <- c(
  "ophir",
  "browns_sub",
  "browns",
  "winters_up",
  "winters_usgs")


site_strip_labs <- c(
  ophir        = "Ophir",
  browns_sub   = "Browns sub.",
  browns       = "Browns",
  winters_up   = "Winters up.",
  winters_usgs = "Winters USGS"
)


df_plot_norm <- df_plot %>%
  filter(date < as.Date("2025-12-13"))   %>% 
  mutate(
    catchment_area = case_when(
      site == "ophir"         ~ 50.1,
      site == "davis"         ~ 4.3,
      site == "browns"        ~ 9.8,
      site == "browns_sub"    ~ 9.4,
      site == "winters_usgs"  ~ 5.4,
      site == "winters_up"    ~ 4.8,
      TRUE ~ NA_real_ ),
    
    # normalized flow (choose flow or flow_filled)
    flow_norm = c(flow / catchment_area)
    # or: flow_norm = flow_filled / catchment_area
    )

df_plot_norm <- df_plot_norm %>%
  mutate(
    site = factor(site, levels = site_order))


###
event_rects <- event_summary %>%
  dplyr::filter(!is.na(event_start_date), !is.na(event_end_date)) %>%
  dplyr::filter(duration_days>=1) %>%
  dplyr::mutate(
    xmin = event_start_date - 0.5,
    xmax = event_end_date + 0.5 )

event_rects <- event_rects %>%
  mutate(
    site = factor(site, levels = site_order))

Baseflowplot <- local({

  df_plot_ppt <- df_plot_norm 
  # Use only the plotted window to get sensible scaling
  Q_top <- max(df_plot_norm$flow_norm, na.rm = TRUE) * 0.95
  PPT_max <- max(df_plot_norm$ppt..mm., na.rm = TRUE)
  k <- Q_top / PPT_max

  df <- df_plot_norm %>%
    mutate(
      PPT_ymin = Q_top - k * ppt..mm. * 0.5,
      PPT_ymax = Q_top)

  ggplot(df, aes(x = date)) +
 # # add in storms
    geom_rect(
    data = event_rects,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "grey70",
    alpha = 0.5 ) +

    geom_rect(
      aes(
        xmin = date - 0.5, xmax = date + 0.5,
        ymin = PPT_ymin, ymax = PPT_ymax,
        fill = site
      ),
      alpha = 0.75,
      inherit.aes = FALSE )   +
    
    geom_line(aes(y = flow_norm, color = site), linewidth = 0.9) +
    scale_color_manual(
      values = site_colors,
      labels = site_labs,
      name = "Site"
      ) +

    facet_grid(site ~ ., scales = "free",
                 labeller = labeller(site = as_labeller(site_strip_labs))) +

    scale_fill_manual(
      values = c(site_colors),
      labels = site_labs,
      name = "Site"
    ) +

    scale_y_continuous(
      limits = c(0, Q_top),
      name = expression(Catchment~normalized~streamflow~(m^3~s^-1~km^-1)),
      sec.axis = sec_axis(
        trans = ~ (Q_top - .) / k,
        name  = "PPT (mm)"
      )
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


 facet_grid(site~., scales = "free_y",
              labeller = labeller(site = as_labeller(site_strip_labs))) +

    xlab("Date") +
    theme_classic() +
    theme(legend.position = "right")  +
      scale_x_date(limits = c(as.Date("2024-09-05"), as.Date("2025-12-15")),
        date_breaks = "1.75 month", date_labels = "%b-%y",
          expand = c(0, 0)) 
})

Baseflowplot

```


```{r, echo=FALSE, include =F}
# ggsave(plot = Baseflowplot, filename = paste("/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Figures/baseflow_plot.png",sep=""),width=9.5,height=10,dpi=300)

```



### Look at run off largest events:

#### A. Biggest peak exceedance

```{r, warning=F, echo=FALSE, include=T}
# 
# largest_runoff_by_exceed <- peak_exceed %>%
#   group_by(site) %>%
#   slice_max(order_by = peak_exceed, n = 1, with_ties = FALSE) %>%
#   ungroup()
# 
# largest_runoff_by_exceed
```

#### B. Biggest peak flow

```{r, warning=F, echo=FALSE, include=T}
largest_runoff_by_peakQ <- event_summary %>%
  group_by(site) %>%
  slice_max(order_by = peak_flow, n = 1, with_ties = FALSE) %>%
  ungroup()
largest_runoff_by_peakQ
```



### ============================================================================

## II.  MS basin differences in solute concetrations 

### ============================================================================

## 2. Basin water quality differences 



```{r, warning=F, size = 'small', echo=FALSE, include=FALSE}
# Read in TSS data

library(dplyr)
library(readr)   # parse_number()

tss_dat2 <- read_csv("/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Raw_data/TSS_data.csv")%>%
  filter((site!="davis")) %>%
  mutate(
    date = as.Date(Collection_date, format = "%m/%d/%y"),
    TSS_mgL = parse_number(as.character(TSS_mgL))
  ) %>%
  dplyr::select(site, date, TSS_mgL)

# Average TSS_mgL for each site-date (prevents join duplicates)
tss_mean <- tss_dat2 %>%
  group_by(site, date) %>%
  summarise(TSS_mgL = mean(TSS_mgL, na.rm = TRUE), .groups = "drop") %>%
  mutate(TSS_mgL = ifelse(is.nan(TSS_mgL), NA_real_, TSS_mgL))  # all-NA groups -> NA

merged_df <- test_dat %>%
  left_join(tss_mean, by = c("site", "date")) %>%
  mutate(
    TSS = as.numeric(TSS),              # ensure numeric
    TSS = coalesce(TSS, TSS_mgL)        # fill missing TSS from averaged TSS_mgL
  ) %>%
  dplyr::select(-TSS_mgL)

```



```{r, warning=F, size = 'small', echo=FALSE, include=FALSE}

test_dat

ref_date <- as.Date("2024-09-25")


df_WQ_plot <- merged_df %>%
  mutate(burn_area = case_when(
    site== "ophir" ~ 0,
    site == "winters_usgs" ~ 65,
    site == "winters_up" ~ 62,
    site == "browns" ~ 42, 
    site == "browns_sub" ~ 41)) %>%
  mutate(
    site_lab = fct_reorder(site, burn_area, .fun = min, .desc = FALSE)
  )%>%
  mutate(
    tsf = as.integer(difftime(datetime, ref_date, units = "days"))
  ) 

```


```{r}

library(dplyr)
library(ggplot2)
library(forcats)
library(rlang)

wq_boxplot <- function(data, y, y_lab,
                       x = burn_area,
                       site = site_lab,
                       fill = site_lab,
                       color = tsf,
                       hline = NULL,
                       filter_expr = NULL,
                       site_colors = NULL,
                       add_anova = TRUE,
                       anova_label_digits = 2,
                       anova_by_x = FALSE,
                       dodge_width = 0.75) {
  
  yq <- enquo(y); xq <- enquo(x); siteq <- enquo(site); fillq <- enquo(fill); colorq <- enquo(color)
  
  # names as strings (for base-model formulas)
  y_name    <- as_name(yq)
  x_name    <- as_name(xq)
  site_name <- as_name(siteq)
  
  pdat <- data
  if (!is.null(filter_expr)) pdat <- pdat %>% filter(!!enquo(filter_expr))
  
  # helper: p formatting - MODIFIED for 2 sig figs
  fmt_p <- function(p) {
    if (is.na(p)) return("p = NA")
    if (p < 0.001) return("p < 0.001")  # Changed: return statement for clarity
    if (p >= 0.001) return(paste0("p = ", signif(p, 2)))  # Changed: 2 sig figs instead of 3
  }
  
  make_anova_text <- function(dat) {
    dat <- dat %>%
      filter(!is.na(.data[[y_name]]), !is.na(.data[[site_name]])) %>%
      mutate(.y_num = as.numeric(.data[[y_name]]),
             .site  = as.factor(.data[[site_name]]))
    
    if (nrow(dat) < 3 || dplyr::n_distinct(dat$.site) < 2) return(NULL)
    
    fit <- aov(reformulate(".site", response = ".y_num"), data = dat)
    sm  <- summary(fit)[[1]]
    
    Fv  <- sm[["F value"]][1]
    df1 <- sm[["Df"]][1]
    df2 <- sm[["Df"]][2]
    pv  <- sm[["Pr(>F)"]][1]
    
    # CHANGE 2: Only return text if p < 0.09
    if (!is.na(pv) && pv >= 0.09) return(NULL)
    
    # CHANGE 1: Use "reach" instead of site_name
    paste0(
      "ANOVA (reach)\n",
      "F(", df1, ", ", df2, ") = ", round(Fv, anova_label_digits), "\n",
      fmt_p(pv)
    )
  }
  
  # compute label text
  anova_text <- NULL
  if (add_anova) {
    if (!anova_by_x) {
      anova_text <- make_anova_text(pdat)
    } else {
      res <- pdat %>%
        group_by(!!xq) %>%
        group_modify(~{
          txt <- make_anova_text(.x)
          
          # pull p to choose smallest p
          .x2 <- .x %>%
            filter(!is.na(.data[[y_name]]), !is.na(.data[[site_name]])) %>%
            mutate(.y_num = as.numeric(.data[[y_name]]),
                   .site  = as.factor(.data[[site_name]]))
          
          if (nrow(.x2) < 3 || dplyr::n_distinct(.x2$.site) < 2) {
            tibble(p = NA_real_, label = txt)
          } else {
            fit <- aov(reformulate(".site", response = ".y_num"), data = .x2)
            pv <- summary(fit)[[1]][["Pr(>F)"]][1]
            
            # CHANGE 2: Set label to NULL if p >= 0.09
            if (!is.na(pv) && pv >= 0.09) {
              tibble(p = pv, label = as.character(NA))
            } else {
              tibble(p = pv, label = txt)
            }
          }
        }) %>%
        ungroup()
      
      # choose smallest p (strongest evidence)
      if (all(is.na(res$p)) || all(is.na(res$label))) {
        anova_text <- NULL
      } else {
        # Filter out NA labels before selecting minimum p
        valid_res <- res %>% filter(!is.na(label))
        if (nrow(valid_res) > 0) {
          anova_text <- valid_res$label[which.min(valid_res$p)]
        } else {
          anova_text <- NULL
        }
      }
    }
  }
  
  # ---- Plot ----
  dodge <- position_dodge(width = dodge_width)
  
  p <- ggplot(pdat, aes(x = factor(!!xq), y = !!yq)) +
    geom_boxplot(
      aes(fill = !!fillq, group = interaction(!!xq, !!fillq)),
      alpha = 0.95,
      position = dodge
    ) +
     labs(y = y_lab, x = "Burn %", fill= "Reach") +
    theme_minimal()
  
  if (!is.null(site_colors)) p <- p + scale_fill_manual(values = site_colors,
                                                        breaks = names(site_strip_labs),
                                                        labels = site_strip_labs)
  if (!is.null(hline)) p <- p + geom_hline(yintercept = hline, linetype = "dashed")
  
  if (!is.null(anova_text)) {
    p <- p + annotate(
      "text",
      x = Inf, y = Inf,
      label = anova_text,
      hjust = 1.05, vjust = 1.1,
      size = 3
    )
  }
  
  p
}


```

```{r, warning=F, size = 'small', echo=FALSE, include=FALSE}

DOC_plot <- wq_boxplot(df_WQ_plot, DOC_QC, expression(DOC~(mg~L^-1)), hline = 0.1, site_colors = site_colors)

TDN_plot <- wq_boxplot(df_WQ_plot, TDN_QC, expression(TDN~(mg~L^-1)), hline = 0.02, site_colors = site_colors)

PO4_plot <- wq_boxplot(df_WQ_plot, PO4.P_QC, expression(PO[4]~(mg~L^-1)), hline = 0.01, site_colors = site_colors)


TSS_plot <- wq_boxplot(df_WQ_plot, TSS, expression(TSS~(mg~L^-1)), site_colors = site_colors)

```


```{r, warning=F, size = 'small', echo=FALSE, include=FALSE}

##
Al_plot  <- wq_boxplot(df_WQ_plot, Al_QC, expression(Al~(mu*g~L^-1)),  hline = 1,  site_colors = site_colors)

As_plot  <- wq_boxplot(df_WQ_plot, As_QC, expression(As~(mu*g~L^-1)),  hline = 0.1,  site_colors = site_colors)

B_plot  <- wq_boxplot(df_WQ_plot, B_QC, expression(B~(mu*g~L^-1)),  hline = 1,  site_colors = site_colors)

Ba_plot  <- wq_boxplot(df_WQ_plot, Ba_QC, expression(Ba~(mu*g~L^-1)),  hline = 1,  site_colors = site_colors)

Ca_plot  <- wq_boxplot(df_WQ_plot, Ca_QC, expression(Ca~(mu*g~L^-1)),  hline = 10,  site_colors = site_colors)

Cr_plot  <- wq_boxplot(df_WQ_plot, Cr_QC, expression(Cr~(mu*g~L^-1)),  hline = 0.1,  site_colors = site_colors)

Fe_plot  <- wq_boxplot(df_WQ_plot, Fe_QC, expression(Fe~(mu*g~L^-1)),  hline = 1,  site_colors = site_colors)

Mn_plot  <- wq_boxplot(df_WQ_plot, Mn_QC, expression(Mn~(mu*g~L^-1)),  hline = 0.1,  site_colors = site_colors)

Pb_plot  <- wq_boxplot(df_WQ_plot, Pb_QC, expression(Pb~(mu*g~L^-1)),  hline = 0.1,  site_colors = site_colors)

Sr_plot  <- wq_boxplot(df_WQ_plot, Sr_QC, expression(Sr~(mu*g~L^-1)),  hline = 0.1,  site_colors = site_colors)

B_QC_plot  <- wq_boxplot(df_WQ_plot, B_QC, expression(B~(mu*g~L^-1)),  hline = 1,  site_colors = site_colors)

SrB_QC_plot  <- wq_boxplot(df_WQ_plot, Sr_B_QC, expression(Sr:B~(mu*g~L^-1)),  hline = 1,  site_colors = site_colors)

```

## Figure 3:

#### Boxplot of solutes


```{r,  fig.width=6, fig.height=4, out.width="85%", echo=F}

library(cowplot)

legend <- get_legend(
  TSS_plot + theme(legend.position = "right")
)

plots <- lapply(
  list(
    TSS_plot, DOC_plot, TDN_plot, PO4_plot,
    Al_plot, As_plot, Ba_plot, Ca_plot,
    Fe_plot, Mn_plot, Sr_plot
  ),
  function(p) p + theme(legend.position = "none")
)


library(ggpubr)
library(grid)

legend_plot <- as_ggplot(legend)

DMC_grid_nuts <- ggarrange(
  plotlist = c(plots, list(legend_plot)),
  ncol = 4,
  nrow = 3,
    labels = LETTERS[1:11],   # A–K
  label.x = 0.08,           # left
  label.y = 0.98,           # top
  hjust = 0,
  vjust = 1,
  font.label = list(size = 12, face = "bold")
)

DMC_grid_nuts



```

```{r, echo=FALSE, include =F}
# ggsave(plot = DMC_grid_nuts, filename = paste("/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Figures/Nuts_load_grid_plot.png",sep=""),width=8,height=7,dpi=300)

```




### ============================================================================

## III. Double mass curves: Calculate interval-based loads

### ============================================================================

#### For each chemistry sample, calculate:

##### 1. Cumulative discharge from start to that sample date

##### 2. Load for that interval = concentration × discharge volume in interval

##### 3. Cumulative load over time

### =========

#### STEP 1: Prepare discharge data (daily values) + DOC loads

### =========


#### One-time setup: parameter tables + helpers

```{r, warning=F, echo=FALSE, include=T}
# Which analytes to include + unit conversion to mg/L
# (DOC etc already mg/L => factor 1; trace in ug/L => /1000 to mg/L)
analytes <- tibble::tribble(
  ~analyte, ~col,        ~togL,
  "DOC",    "DOC_QC",     1,
  "BC",     "BC_QC",      1,
  "TOC",    "TOC_QC",     1,
  "TDN",    "TDN_QC",     1,
  "PO4_P",  "PO4.P_QC",   1,
  "As",     "As_QC",      1/1000,
  "Al",     "Al_QC",      1/1000,
  "Ba",     "Ba_QC",      1/1000,
  "Ca",     "Ca_QC",      1/1000,
  "Cr",     "Cr_QC",      1/1000,
  "Fe",     "Fe_QC",      1/1000,
  "Pb",     "Pb_QC",      1/1000,
  "Mn",     "Mn_QC",      1/1000,
  "Sr",     "Sr_QC",      1/1000,
  "Zn",     "Zn_QC",      1/1000,
  "B",      "B_QC",       1/1000,
  "Sr_B",   "Sr_B_QC",    1/1000,
  "TSS",      "TSS",      1,
)

label_site <- function(site){
  dplyr::case_when(
    site == "ophir" ~ "A_ophir",
    site == "davis" ~ "B_davis",
    site == "browns_sub" ~ "C_browns_sub",
    site == "browns" ~ "D_browns",
    site == "winters_up" ~ "E_winters_up",
    site == "winters_usgs" ~ "F_winters_usgs",
    TRUE ~ site
  )
}

```

### Flow + storm flags (unchanged logic, but isolated)

```{r, warning=F, echo=T, include=T}
prep_flow <- function(new_df_hydro, flow_start = as.Date("2024-10-01")) {
  new_df_hydro %>%
    filter(date >= flow_start) %>%
    mutate(sec_per_day = 86400) %>%
    arrange(site, date) %>%
    group_by(site) %>%
    mutate(
      Q_daily_liters = replace_na(flow_filled, 0) * 1000 * sec_per_day,
      Q_c_liters     = cumsum(Q_daily_liters),
      ppt_daily_cum  = cumsum(ppt..mm.)
    ) %>%
    ungroup()
}

#### YOU ARE HERE 


prep_storm_flags <- function(event_summary,test_keys) {
  event_summary %>%
    dplyr::select(site, event_id_lf, event_start_date, event_end_date, duration_days) %>%
    dplyr::filter(!is.na(event_start_date), !is.na(event_end_date)) %>%
    dplyr::arrange(site, event_start_date) %>%
    dplyr::distinct(site, event_id_lf, event_start_date, event_end_date, .keep_all = TRUE)

  test_keys %>%
    dplyr::select(site, date) %>%
    inner_join(prep_storm_flags, by = c("site", "date")) %>%
    transmute(site, date, storm = "storm") %>%
    distinct()
}

```

### Core: interval loads + cumulative loads for all analytes 

```{r, warning=F, echo=T, include=T}

prep_chem_loads_long <- function(test_dat, flow_df, storm_df, analytes_tbl,
                                 chem_start = as.Date("2024-09-30")) {

  chem_long <- test_dat %>%
    filter(date >= chem_start) %>%
    dplyr::select(site, date, all_of(analytes_tbl$col)) %>%
    pivot_longer(cols = all_of(analytes_tbl$col), names_to = "col", values_to = "conc_raw") %>%
    filter(!is.na(conc_raw)) %>%                       # keep actual chem samples
    left_join(analytes_tbl, by = "col") %>%
    mutate(concgL = conc_raw * togL)

  chem_long %>%
    left_join(flow_df %>% dplyr::select(site, date, Q_c_liters), by = c("site", "date")) %>%
    left_join(storm_df, by = c("site", "date")) %>%
    mutate(site_OL = label_site(site)) %>%
    arrange(site_OL, analyte, date) %>%
    group_by(site_OL, analyte) %>%
    mutate(
      Q_interval_liters = Q_c_liters - lag(Q_c_liters, default = 0),
      load_intervalg  = concgL * Q_interval_liters,
      load_cumg       = cumsum(replace_na(load_intervalg, 0))
    ) %>%
    ungroup()
}

```


### Fxn to fit DMC models for any analyte 

```{r, warning=F, echo=T, include=T}
fit_dmc <- function(dmc_long, analyte_name = "DOC") {
  df <- dmc_long %>%
    filter(analyte == analyte_name) %>%
    group_by(site_OL) %>%
    mutate(
      load_c_norm = load_cumg / max(load_cumg, na.rm = TRUE),
      Q_c_norm    = Q_c_liters  / max(Q_c_liters,  na.rm = TRUE)
    ) %>%
    ungroup()

  models <- df %>%
    filter(!is.na(load_c_norm), !is.na(Q_c_norm)) %>%
    group_by(site_OL) %>%
    nest() %>%
    mutate(
      n_samples = map_int(data, nrow),
      model     = map(data, ~ lm(load_c_norm ~ 0 + Q_c_norm, data = .x)),
      tidy      = map(model, ~ broom::tidy(.x, conf.int = TRUE)),
      augmented = map2(model, data, ~ broom::augment(.x, data = .y))
    ) %>%
    unnest(tidy) %>%
    filter(term == "Q_c_norm") %>%
    transmute(site_OL, n_samples,
              slope = estimate, std_error = std.error,
              lwr = conf.low, upr = conf.high,
              p.value, data, model, augmented)

  list(df = df, models = models)
}

```

### Inference

```{r, warning=F, echo=T, include=T}
infer_slopes_vs1 <- function(models_tbl) {
  models_tbl %>%
    mutate(
      t_stat = (slope - 1) / std_error,
      df = n_samples - 1,
      p_vs_1 = 2 * pt(abs(t_stat), df, lower.tail = FALSE),
      interpretation = case_when(
        slope > 1 & p_vs_1 < 0.05 ~ "Enrichment: load ↑ faster than discharge",
        slope < 1 & p_vs_1 < 0.05 ~ "Dilution: discharge ↑ faster than load",
        p_vs_1 >= 0.05 ~ "Proportional: ∝",
        TRUE ~ "Check data"
      )
    ) %>%
    dplyr::select(site_OL, n_samples, slope, std_error, lwr, upr, p.value, p_vs_1, interpretation)
}
```

### Plotting fxn for DMC

```{r, warning=F, echo=T, include=T}

site_strip_labs <- c(
  A_ophir        = "Ophir",
  C_browns_sub   = "Browns sub.",
  D_browns       = "Browns",
  E_winters_up   = "Winters up.",
  F_winters_usgs = "Winters USGS"
)

plot_dmc <- function(df_norm, models_tbl, value_label = "N.C. load", 
                     flow_label ="flow lab",
                     color_var = "tsf") {

  ggplot(df_norm, aes(Q_c_norm, load_c_norm)) +
        geom_abline(data = models_tbl, aes(slope = slope, intercept = 0),
                linewidth = 0.5, alpha = 0.8) +
    geom_path(alpha = 0.5, linewidth = 1) +
    geom_point(aes(color = .data[[color_var]]), size = 2.5, alpha = 0.9) +
    geom_point(
      data = df_norm %>% filter(storm == "storm"),
      shape = 21, fill = NA, color = "black", stroke = 1.5, size = 3
    ) +
    scale_color_viridis_c(option = "plasma", direction = -1) +
    facet_wrap(~ site_OL, nrow = 1,
                   labeller = labeller(site_OL = as_labeller(site_strip_labs))) +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    labs(x = flow_label, y = value_label) +
        theme_minimal() +

    theme(legend.position = "bottom", panel.grid.minor = element_blank())
}


```

### Fxn for DMC residuals (cumulative) 

```{r, warning=F, echo=T, include=T}
calc_cumul_resid <- function(df_norm, models_tbl) {
  # Join each site's fitted model back onto its normalized data
  # models_tbl must contain: site_OL, model (lm)
  df_norm %>%
    left_join(models_tbl %>% dplyr::select(site_OL, model), by = "site_OL") %>%
    group_by(site_OL) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(
      fit   = predict(model[[1]], newdata = cur_data()),
      resid = load_c_norm - fit,
      cumul_resid = cumsum(replace_na(resid, 0))
    ) %>%
    ungroup() %>%
    dplyr::select(-model)
}

```



### Breakpoint detection (Davies + segmented) by site

```{r, warning=F, echo=T, include=T}
detect_breakpoints_site <- function(data, min_n = 10, alpha = 0.05) {
  # data must include: date, Q_c_norm, load_c_norm
  # returns a one-row tibble

  if (!requireNamespace("segmented", quietly = TRUE)) {
    return(tibble::tibble(
      breakpoint = NA_real_, breakpoint_date = as.Date(NA),
      slope_1 = NA_real_, slope_2 = NA_real_, delta_slope = NA_real_,
      davies_p = NA_real_, note = "Package 'segmented' not installed"
    ))
  }

  tryCatch({
    if (nrow(data) < min_n) {
      return(tibble::tibble(
        breakpoint = NA_real_, breakpoint_date = as.Date(NA),
        slope_1 = NA_real_, slope_2 = NA_real_, delta_slope = NA_real_,
        davies_p = NA_real_, note = "Insufficient data"
      ))
    }

    linear_fit <- lm(load_c_norm ~ Q_c_norm, data = data)

    # Davies test for a change in slope
    dav <- segmented::davies.test(linear_fit, ~ Q_c_norm)

    if (!is.null(dav$p.value) && dav$p.value < alpha) {
      seg_fit <- segmented::segmented(linear_fit, seg.Z = ~ Q_c_norm, npsi = 1)

      bp_val <- tryCatch(
        as.numeric(summary(seg_fit)$psi[, "Est."]),
        error = function(e) NA_real_
      )

      # date closest to the estimated breakpoint in Q space
      data_sorted <- data[order(data$Q_c_norm), ]
      bp_date <- data_sorted$date[which.min(abs(data_sorted$Q_c_norm - bp_val))]

      slopes <- segmented::slope(seg_fit)$Q_c_norm

      tibble::tibble(
        breakpoint = bp_val,
        breakpoint_date = bp_date,
        slope_1 = slopes[1, 1],
        slope_2 = slopes[2, 1],
        delta_slope = slopes[2, 1] - slopes[1, 1],
        davies_p = dav$p.value,
        note = "Significant breakpoint detected"
      )
    } else {
      tibble::tibble(
        breakpoint = NA_real_, breakpoint_date = as.Date(NA),
        slope_1 = unname(coef(linear_fit)[["Q_c_norm"]]),
        slope_2 = NA_real_, delta_slope = NA_real_,
        davies_p = dav$p.value,
        note = "No significant breakpoint"
      )
    }
  }, error = function(e) {
    tibble::tibble(
      breakpoint = NA_real_, breakpoint_date = as.Date(NA),
      slope_1 = NA_real_, slope_2 = NA_real_, delta_slope = NA_real_,
      davies_p = NA_real_, note = paste("Error:", e$message)
    )
  })
}

breakpoint_analysis_by_site <- function(df_norm, min_n = 10, alpha = 0.05) {
  df_norm %>%
    filter(!is.na(load_c_norm), !is.na(Q_c_norm)) %>%
    group_by(site_OL) %>%
    tidyr::nest() %>%
    mutate(breaks = purrr::map(data, detect_breakpoints_site, min_n = min_n, alpha = alpha)) %>%
    tidyr::unnest_wider(breaks)
}


```

 
### Fxn for companion plot (C residuals + breakpoint markers + storms)

```{r, warning=F, echo=T, include=T}
plot_cumul_resid <- function(resid_df,
                             bp_tbl = NULL,
                             start_date = as.Date("2024-10-01"),
                             site_colors = NULL,
                             ylab = "Cum. residuals (load ~ Q)",
                             show_legend = FALSE) {

  plot_df <- resid_df %>% filter(date > start_date)

  if (!is.null(bp_tbl)) {
    bp_pts <- bp_tbl %>%
      filter(!is.na(breakpoint_date)) %>%
      dplyr::select(site_OL, breakpoint_date) %>%
      distinct() %>%
      left_join(
        plot_df %>% dplyr::select(site_OL, date, cumul_resid),
        by = c("site_OL" = "site_OL", "breakpoint_date" = "date")
      )
  } else {
    bp_pts <- tibble::tibble(
      site_OL = character(),
      breakpoint_date = as.Date(character()),
      cumul_resid = numeric()
    )
  }

  p <- ggplot(plot_df, aes(x = date, y = cumul_resid)) +
    geom_line(aes(color = site_OL), linewidth = 1.1) +
    geom_point(aes(color = site_OL), size = 3) +
    geom_point(
      data = bp_pts %>% filter(breakpoint_date > start_date),
      aes(x = breakpoint_date, y = cumul_resid, color = site_OL),
      shape = 23, stroke = 1.5, fill = NA, size = 4
    ) +
    { if ("storm" %in% names(plot_df))
        geom_point(
          data = plot_df %>% filter(storm == "storm"),
          aes(x = date, y = cumul_resid),
          shape = 21, fill = NA, color = "black",
          stroke = 1.5, size = 3
        ) } +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = NULL, y = ylab) +
    theme_bw() +
    scale_x_date(date_breaks = "2 month", date_labels = "%b-%y") +
    theme(legend.position = if (show_legend) "right" else "none")

  if (!is.null(site_colors)) {
    p <- p + scale_color_manual(values = site_colors)
  }

  p
}



```

```{r, warning=F, echo=T, include=T}

# 1) Prep once
davis_flow <- prep_flow(new_df_hydro, flow_start = as.Date("2024-09-30"))
storm <- prep_storm_flags(event_summary, test_keys)

# optional: flow summary
davis_flow %>%
  group_by(site) %>%
  summarise(
    max_Q_c_liters = max(Q_c_liters, na.rm = TRUE),
    max_ppt_daily_cum = max(ppt_daily_cum, na.rm = TRUE),
    .groups = "drop"
  )

# 2) Loads for all analytes (one call)
dmc_long <- prep_chem_loads_long(merged_df%>%filter(site!="davis"), davis_flow, storm, analytes)

# 3) Add tsf once (if ref_date exists in env)
dmc_long <- dmc_long %>%
  mutate(tsf = as.integer(difftime(date, ref_date, units = "days")))

```


### quick water yield calculation for just water year 2025

```{r, warning=F, echo=T, include=T}


library(dplyr)

yield_m3 <- davis_flow %>%
  filter(date >= as.Date("2024-10-01"),
         date <= as.Date("2025-09-30")) %>%
  group_by(site) %>%
  summarise(
    total_liters = sum(Q_daily_liters, na.rm = TRUE),
    total_m3 = total_liters / 1000
  ) %>%

  mutate(
    total_m3_sci = formatC(total_m3, format = "e", digits = 2)
  ) %>%
  arrange(site)

yield_m3


```


## TSS

```{r, warning=F, echo=T, include=T}
TSS_fit   <- fit_dmc(dmc_long, "TSS")
models_TSS <- TSS_fit$models
davis_TSS  <- TSS_fit$df

inference_TSS <- infer_slopes_vs1(models_TSS)
inference_TSS
```


```{r,  fig.width=9, fig.height=3, out.width="90%"}

DMC_TSS_plot <- plot_dmc(davis_TSS, models_TSS, 
                         flow_label = expression(Normalized~cumulative~water~yield~(m^3~day^-1)),
                         value_label = expression(Normalized~TSS~load~(mg~L^-1)))
DMC_TSS_plot

```

```{r}
TSS_resid <- calc_cumul_resid(TSS_fit$df, TSS_fit$models)
bp_TSS <- breakpoint_analysis_by_site(TSS_fit$df)
bp_TSS
```

```{r,  fig.width=7, fig.height=2.25, out.width="85%"}

DMCres_TSS_plot <- plot_cumul_resid(
  resid_df   = TSS_resid,
  bp_tbl     = bp_TSS,
  site_colors = site_colors_OL,
  ylab = "C residuals TSS ~ Q"
)

DMCres_TSS_plot
```



## DOC dynamics 

```{r, warning=F, echo=T, include=T}
DOC_fit   <- fit_dmc(dmc_long, "DOC")
models_DOC <- DOC_fit$models
davis_DOC  <- DOC_fit$df

inference_DOC <- infer_slopes_vs1(models_DOC)
inference_DOC
```


```{r,  fig.width=9, fig.height=5, out.width="90%"}

DMC_DOC_plot <- plot_dmc(davis_DOC, models_DOC, 
                         flow_label = expression(Normalized~cumulative~water~yield~(m^3~day^-1)),
                         value_label = expression(Normalized~DOC~load~(mg~L^-1)))
DMC_DOC_plot
```

```{r}
DOC_resid <- calc_cumul_resid(DOC_fit$df, DOC_fit$models)
bp_DOC <- breakpoint_analysis_by_site(DOC_fit$df)
bp_DOC
```

```{r,  fig.width=7, fig.height=2.25, out.width="85%"}

DMCres_DOC_plot <- plot_cumul_resid(
  resid_df   = DOC_resid,
  bp_tbl     = bp_DOC,
  site_colors = site_colors_OL,
  ylab = "C residuals DOC ~ Q"
)

DMCres_DOC_plot
```



## TDN dynamics 

```{r}
TDN_fit   <- fit_dmc(dmc_long, "TDN")
models_TDN <- TDN_fit$models
davis_TDN  <- TDN_fit$df

inference_TDN <- infer_slopes_vs1(models_TDN)
inference_TDN
```


```{r,  fig.width=9, fig.height=5, out.width="90%"}

DMC_TDN_plot <- plot_dmc(davis_TDN, models_TDN, 
                         flow_label = expression(Normalized~cumulative~water~yield~(m^3~day^-1)),
                         value_label = expression(Normalized~TDN~load~(mg~L^-1)))
DMC_TDN_plot
```

```{r}
TDN_resid <- calc_cumul_resid(TDN_fit$df, TDN_fit$models)
bp_TDN <- breakpoint_analysis_by_site(TDN_fit$df)
bp_TDN
```

```{r,  fig.width=7, fig.height=2.25, out.width="85%"}

DMCres_TDN_plot <- plot_cumul_resid(
  resid_df   = TDN_resid,
  bp_tbl     = bp_TDN,
  site_colors = site_colors_OL,
  ylab = "C residuals TDN ~ Q"
)

DMCres_TDN_plot
```


### PO4 dynamics 

```{r}
PO4_fit   <- fit_dmc(dmc_long, "PO4_P")
models_PO4 <- PO4_fit$models
davis_PO4  <- PO4_fit$df

inference_PO4 <- infer_slopes_vs1(models_PO4)
inference_PO4
```

```{r,  fig.width=9, fig.height=5, out.width="90%"}

DMC_PO4_plot <- plot_dmc(davis_PO4, models_PO4,
                         flow_label = expression(Normalized~cumulative~water~yield~(m^3~day^-1)),
                         value_label = expression(Normalized~PO[4]~load~(mg~L^-1)))
DMC_PO4_plot
```

```{r}
PO4_resid <- calc_cumul_resid(PO4_fit$df, PO4_fit$models)
bp_PO4 <- breakpoint_analysis_by_site(PO4_fit$df)
bp_PO4
```

```{r,  fig.width=7, fig.height=2.25, out.width="85%"}

DMCres_PO4_plot <- plot_cumul_resid(
  resid_df   = PO4_resid,
  bp_tbl     = bp_PO4,
  site_colors = site_colors_OL,
  ylab = "C residuals PO4 ~ Q"
)

DMCres_PO4_plot
```




### Al dynamics 

```{r}
Al_fit   <- fit_dmc(dmc_long, "Al")
models_Al <- Al_fit$models
davis_Al  <- Al_fit$df

inference_Al <- infer_slopes_vs1(models_Al)
inference_Al
```

```{r,  fig.width=9, fig.height=5, out.width="90%"}

DMC_Al_plot <- plot_dmc(davis_Al, models_Al, value_label = "Al load")
DMC_Al_plot
```

```{r}
Al_resid <- calc_cumul_resid(Al_fit$df, Al_fit$models)
bp_Al <- breakpoint_analysis_by_site(Al_fit$df)
bp_Al
```

```{r,  fig.width=7, fig.height=2.25, out.width="85%"}

DMCres_Al_plot <- plot_cumul_resid(
  resid_df   = Al_resid,
  bp_tbl     = bp_Al,
  site_colors = site_colors_OL,
  ylab = "C residuals Al ~ Q"
)

DMCres_Al_plot
```



### As dynamics 

```{r}
As_fit   <- fit_dmc(dmc_long, "As")
models_As <- As_fit$models
davis_As  <- As_fit$df

inference_As <- infer_slopes_vs1(models_As)
inference_As
```

```{r,  fig.width=9, fig.height=5, out.width="90%"}

DMC_As_plot <- plot_dmc(davis_As, models_As, value_label = "As load")
DMC_As_plot
```

```{r}
As_resid <- calc_cumul_resid(As_fit$df, As_fit$models)
bp_As <- breakpoint_analysis_by_site(As_fit$df)
bp_As
```

```{r,  fig.width=7, fig.height=2.25, out.width="85%"}

DMCres_As_plot <- plot_cumul_resid(
  resid_df   = As_resid,
  bp_tbl     = bp_As,
  site_colors = site_colors_OL,
  ylab = "C residuals As ~ Q"
)

DMCres_As_plot
```


### B dynamics 

```{r}
B_fit   <- fit_dmc(dmc_long, "B")
models_B <- B_fit$models
davis_B  <- B_fit$df

inference_B <- infer_slopes_vs1(models_B)
inference_B
```

```{r,  fig.width=9, fig.height=5, out.width="90%"}

DMC_B_plot <- plot_dmc(davis_B, models_B, value_label = "B load")
DMC_B_plot
```

```{r}
B_resid <- calc_cumul_resid(B_fit$df, B_fit$models)
bp_B <- breakpoint_analysis_by_site(B_fit$df)
bp_B
```

```{r,  fig.width=7, fig.height=2.25, out.width="85%"}

DMBes_B_plot <- plot_cumul_resid(
  resid_df   = B_resid,
  bp_tbl     = bp_B,
  site_colors = site_colors_OL,
  ylab = "C residuals B ~ Q"
)

DMBes_B_plot
```


### Ba dynamics 

```{r}
Ba_fit   <- fit_dmc(dmc_long, "Ba")
models_Ba <- Ba_fit$models
davis_Ba  <- Ba_fit$df

inference_Ba <- infer_slopes_vs1(models_Ba)
inference_Ba
```

```{r,  fig.width=9, fig.height=5, out.width="90%"}

DMC_Ba_plot <- plot_dmc(davis_Ba, models_Ba, value_label = "Ba load")
DMC_Ba_plot
```

```{r}
Ba_resid <- calc_cumul_resid(Ba_fit$df, Ba_fit$models)
bp_Ba <- breakpoint_analysis_by_site(Ba_fit$df)
bp_Ba
```

```{r,  fig.width=7, fig.height=2.25, out.width="85%"}

DMCres_Ba_plot <- plot_cumul_resid(
  resid_df   = Ba_resid,
  bp_tbl     = bp_Ba,
  site_colors = site_colors_OL,
  ylab = "C residuals Ba ~ Q"
)

DMCres_Ba_plot
```


### Ca dynamics 

```{r}
Ca_fit   <- fit_dmc(dmc_long, "Ca")
models_Ca <- Ca_fit$models
davis_Ca  <- Ca_fit$df

inference_Ca <- infer_slopes_vs1(models_Ca)
inference_Ca
```

```{r,  fig.width=9, fig.height=5, out.width="90%"}

DMC_Ca_plot <- plot_dmc(davis_Ca, models_Ca, value_label = "Ca load")
DMC_Ca_plot
```

```{r}
Ca_resid <- calc_cumul_resid(Ca_fit$df, Ca_fit$models)
bp_Ca <- breakpoint_analysis_by_site(Ca_fit$df)
bp_Ca
```

```{r,  fig.width=7, fig.height=2.25, out.width="85%"}

DMCres_Ca_plot <- plot_cumul_resid(
  resid_df   = Ca_resid,
  bp_tbl     = bp_Ca,
  site_colors = site_colors_OL,
  ylab = "C residuals Ca ~ Q"
)

DMCres_Ca_plot
```




### Cr dynamics 

```{r}
Cr_fit   <- fit_dmc(dmc_long, "Cr")
models_Cr <- Cr_fit$models
davis_Cr  <- Cr_fit$df

inference_Cr <- infer_slopes_vs1(models_Cr)
inference_Cr
```

```{r,  fig.width=9, fig.height=5, out.width="90%"}

DMC_Cr_plot <- plot_dmc(davis_Cr, models_Cr, value_label = "Cr load")
DMC_Cr_plot
```

```{r}
Cr_resid <- calc_cumul_resid(Cr_fit$df, Cr_fit$models)
bp_Cr <- breakpoint_analysis_by_site(Cr_fit$df)
bp_Cr
```

```{r,  fig.width=7, fig.height=2.25, out.width="85%"}

DMCres_Cr_plot <- plot_cumul_resid(
  resid_df   = Cr_resid,
  bp_tbl     = bp_Cr,
  site_colors = site_colors_OL,
  ylab = "C residuals Cr ~ Q"
)

DMCres_Cr_plot
```



### Fe dynamics 

```{r}
Fe_fit   <- fit_dmc(dmc_long, "Fe")
models_Fe <- Fe_fit$models
davis_Fe  <- Fe_fit$df

inference_Fe <- infer_slopes_vs1(models_Fe)
inference_Fe
```

```{r,  fig.width=9, fig.height=5, out.width="90%"}
DMC_Fe_plot <- plot_dmc(davis_Fe, models_Fe, value_label = "Fe load")
DMC_Fe_plot
```

```{r}
Fe_resid <- calc_cumul_resid(Fe_fit$df, Fe_fit$models)
bp_Fe <- breakpoint_analysis_by_site(Fe_fit$df)
bp_Fe
```


```{r, fig.width=7, fig.height=2.25, out.width="85%"}

DMCres_Fe_plot <- plot_cumul_resid(
  resid_df   = Fe_resid,
  bp_tbl     = bp_Fe,
  site_colors = site_colors_OL,
  ylab = "C residuals Fe ~ Q"
)

DMCres_Fe_plot
```

### Mn dynamics 

```{r}
Mn_fit   <- fit_dmc(dmc_long, "Mn")
models_Mn <- Mn_fit$models
davis_Mn  <- Mn_fit$df

inference_Mn <- infer_slopes_vs1(models_Mn)
inference_Mn
```

```{r,  fig.width=9, fig.height=5, out.width="90%"}

DMC_Mn_plot <- plot_dmc(davis_Mn, models_Mn, value_label = "Mn load")
DMC_Mn_plot
```

```{r}
Mn_resid <- calc_cumul_resid(Mn_fit$df, Mn_fit$models)
bp_Mn <- breakpoint_analysis_by_site(Mn_fit$df)
bp_Mn
```

```{r,  fig.width=7, fig.height=2.25, out.width="85%"}

DMCres_Mn_plot <- plot_cumul_resid(
  resid_df   = Mn_resid,
  bp_tbl     = bp_Mn,
  site_colors = site_colors_OL,
  ylab = "C residuals Mn ~ Q"
)

DMCres_Mn_plot
```



### Pb dynamics 

```{r}
Pb_fit   <- fit_dmc(dmc_long, "Pb")
models_Pb <- Pb_fit$models
davis_Pb  <- Pb_fit$df

inference_Pb <- infer_slopes_vs1(models_Pb)
inference_Pb
```

```{r,  fig.width=9, fig.height=5, out.width="90%"}

DMC_Pb_plot <- plot_dmc(davis_Pb, models_Pb, value_label = "Pb load")
DMC_Pb_plot
```

```{r}
Pb_resid <- calc_cumul_resid(Pb_fit$df, Pb_fit$models)
bp_Pb <- breakpoint_analysis_by_site(Pb_fit$df)
bp_Pb
```

```{r,  fig.width=7, fig.height=2.25, out.width="85%"}

DMCres_Pb_plot <- plot_cumul_resid(
  resid_df   = Pb_resid,
  bp_tbl     = bp_Pb,
  site_colors = site_colors_OL,
  ylab = "C residuals Pb ~ Q"
)

DMCres_Pb_plot
```




### Sr dynamics 

```{r}
Sr_fit   <- fit_dmc(dmc_long, "Sr")
models_Sr <- Sr_fit$models
davis_Sr  <- Sr_fit$df

inference_Sr <- infer_slopes_vs1(models_Sr)
inference_Sr
```

```{r,  fig.width=9, fig.height=5, out.width="90%"}

DMC_Sr_plot <- plot_dmc(davis_Sr, models_Sr, value_label = "Sr load")
DMC_Sr_plot
```

```{r}
Sr_resid <- calc_cumul_resid(Sr_fit$df, Sr_fit$models)
bp_Sr <- breakpoint_analysis_by_site(Sr_fit$df)
bp_Sr
```

```{r,  fig.width=7, fig.height=2.25, out.width="85%"}

DMSres_Sr_plot <- plot_cumul_resid(
  resid_df   = Sr_resid,
  bp_tbl     = bp_Sr,
  site_colors = site_colors_OL,
  ylab = "C residuals Sr ~ Q"
)

DMSres_Sr_plot
```




### B dynamics 

```{r}
B_fit   <- fit_dmc(dmc_long, "B")
models_B <- B_fit$models
davis_B  <- B_fit$df

inference_B <- infer_slopes_vs1(models_B)
inference_B
```

```{r,  fig.width=9, fig.height=5, out.width="90%"}

DMC_B_plot <- plot_dmc(davis_B, models_B, value_label = "B load")
DMC_B_plot
```

```{r}
B_resid <- calc_cumul_resid(B_fit$df, B_fit$models)
bp_B <- breakpoint_analysis_by_site(B_fit$df)
bp_B
```

```{r,  fig.width=7, fig.height=2.25, out.width="85%"}

DMBes_B_plot <- plot_cumul_resid(
  resid_df   = B_resid,
  bp_tbl     = bp_B,
  site_colors = site_colors_OL,
  ylab = "C residuals B ~ Q"
)

DMBes_B_plot
```


### Zn dynamics 

```{r}

Zn_fit   <- fit_dmc(dmc_long, "Zn")
models_Zn <- Zn_fit$models
davis_Zn  <- Zn_fit$df

inZnrence_Zn <- infer_slopes_vs1(models_Zn)
inZnrence_Zn
```

```{r,  fig.width=9, fig.height=5, out.width="90%"}
DMC_Zn_plot <- plot_dmc(davis_Zn, models_Zn, value_label = "Zn load")
DMC_Zn_plot
```

```{r}
Zn_resid <- calc_cumul_resid(Zn_fit$df, Zn_fit$models)
bp_Zn <- breakpoint_analysis_by_site(Zn_fit$df)
bp_Zn
```


```{r, fig.width=7, fig.height=2.25, out.width="85%"}

DMCres_Zn_plot <- plot_cumul_resid(
  resid_df   = Zn_resid,
  bp_tbl     = bp_Zn,
  site_colors = site_colors_OL,
  ylab = "C residuals Zn ~ Q"
)

DMCres_Zn_plot
```


####

```{r,  fig.width=10, fig.height=8, out.width="85%", echo=F}

DMC_grid_nuts <- ggarrange( 
  DMC_TSS_plot,
  DMC_DOC_plot,
  DMC_TDN_plot,
  DMC_PO4_plot,
  common.legend = T,
  ncol=1,
  nrow=4)

DMC_grid_nuts
```

```{r, echo=FALSE, include =F}

# ggsave(plot = DMC_grid_nuts, filename = paste("./Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Figures/DMLC_nuts_grid_plot.png",sep=""),width=14,height=12,dpi=300)

```

#### 

```{r,  fig.width=8, fig.height=6, out.width="85%", echo=FALSE}
DMC_res_grid_nuts <- ggarrange( 
  DMCres_TSS_plot,
  DMCres_DOC_plot,
  DMCres_TDN_plot,
  DMCres_PO4_plot,
  common.legend = T,
  ncol=1,
  nrow=4)

DMC_res_grid_nuts
```

```{r, echo=FALSE, include =F}

# ggsave(plot = DMC_res_grid_nuts, filename = paste("./Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Figures/DMLC_res_nuts_grid_plot.png",sep=""),width=6,height=8,dpi=300)

```


#### 

```{r,  fig.width=11, fig.height=25, out.width="95%", echo=FALSE}

DMC_grid_metals <- 
  ggarrange( 
  DMC_Al_plot,
  #DMC_As_plot,
  #DMC_B_plot,
  #DMC_Ba_plot,
  #DMC_Ca_plot,
  #DMC_Cr_plot, 
  DMC_Fe_plot,
  DMC_Mn_plot,
  #DMC_Pb_plot,
  #DMC_Sr_plot,
  #DMC_Zn_plot,
  common.legend = T,
  ncol=1,
  nrow=3)

DMC_grid_metals

```



```{r, echo=FALSE, include =F}

# ggsave(plot = DMC_grid_metals, filename = paste("./Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Figures/DMLC_metals_grid_plot.png",sep=""),width=14,height=8,dpi=300)


# ggsave(plot = DMC_grid_metals, filename = paste("./Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Figures/DMLC_metals_grid_plot_SI.png",sep=""),width=14,height=20,dpi=300)

```


#### 

```{r,  fig.width=7, fig.height=16, out.width="85%", echo=FALSE}

DMCres_grid_metals <- 
  ggarrange( 
  #DMC_Al_plot,
  DMCres_As_plot,
  DMBes_B_plot,
  DMCres_Ba_plot,
  DMCres_Ca_plot,
  DMCres_Cr_plot, 
  #DMCres_Fe_plot,
  #DMCres_Mn_plot,
  DMCres_Pb_plot,
  DMSres_Sr_plot,
  DMCres_Zn_plot,
  common.legend = T,
  ncol=1,
  nrow=8)

DMCres_grid_metals
```


```{r, echo=FALSE, include =F}
# ggsave(plot = DMCres_grid_metals, filename = paste("./Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Figures/DMLC_res_metals_grid_plot_SI.png",sep=""),width=6,height=20,dpi=300)

```


### ============================================================================

## IV.  GLMs 

### ============================================================================


```{r, echo=FALSE, include =F}

vars <- c("TSS","lag_C_PPT","tsf","flow_filled","temp","DO","EC","pH","site")

## merge hydro and solute data 
df_model <- df_WQ_plot %>%
  left_join(new_df_hydro, by =c("site", "date")) %>%
  filter(site!="ophir") %>%
  select(all_of(vars)) %>%
  tidyr::drop_na() %>%
    mutate(catchment = case_when(
    site == "winters_usgs" ~ "winters",
    site == "winters_up" ~ "winters",
    site == "browns" ~ "browns", 
    site == "browns_sub" ~ "browns"))


head(df_model)

```


```{r, echo=FALSE, include =T}
library(PerformanceAnalytics)

covar_check <- df_model[,c(1:8)]

chart.Correlation(covar_check, histogram=TRUE, pch=19)
```


### build glm

```{r, echo=FALSE, include =F}
library(brms)

library(lme4) # fit GLMM in frequentist framework
library(lmerTest)
```

#### glm start for fast model builds

### PPT model 

```{r, echo=FALSE, include =T}

ppt_mod <- lmer(TSS~ scale(lag_C_PPT) + scale(tsf) + scale(lag_C_PPT*tsf) + (1 | catchment/site), data=df_model)
summary(ppt_mod)

ppt_mod.1 <- lmer(TSS~ scale(lag_C_PPT)+ scale(tsf) + (1 | catchment/site), data=df_model)
summary(ppt_mod.1)

ppt_mod.2 <- lmer(TSS~ scale(lag_C_PPT) + scale(tsf) + scale(flow_filled) + (1 | catchment/site), data=df_model)
summary(ppt_mod.2)

ppt_mod.3 <- lmer(TSS~ scale(lag_C_PPT) + scale(tsf) + scale(flow_filled*tsf) + (1 | catchment/site), data=df_model)
summary(ppt_mod.3)

ppt_mod.4 <- lmer(TSS~ scale(lag_C_PPT) + (1 | catchment/site), data=df_model)
summary(ppt_mod.4)

ppt_mod.5 <- lmer(TSS~ scale(lag_C_PPT) + scale(flow_filled)+ (1 | catchment/site), data=df_model)
summary(ppt_mod.5)

AIC(ppt_mod, ppt_mod.1, ppt_mod.2, ppt_mod.3, ppt_mod.4, ppt_mod.5)

library(MuMIn)
model.sel(ppt_mod, ppt_mod.1, ppt_mod.2, ppt_mod.3, ppt_mod.4, ppt_mod.5, rank = "AICc")

## best model :ppt_mod.2
```

### move forward with ppt_mod.2

- ΔAICc difference is trivial from ppt_mod.2 to ppt_mod

- interaction unsupported and more complex in ppt_mod

- strong collinearity (correlation = −0.871) and in ppt_mod




### global model of all water quality parameters (

```{r, echo=FALSE, include =T}

glob_mod <- lmer(TSS~ scale(lag_C_PPT) + scale(tsf) + scale(flow_filled) +
                   scale(temp) + scale(DO) +scale(EC) +scale(pH) +
                   (1 | catchment/site), data=df_model)
summary(glob_mod)

glob_mod.1 <- lmer(TSS~ scale(lag_C_PPT) + scale(tsf) + scale(flow_filled) +
                    scale(DO) +scale(EC) +scale(pH) +
                   (1 | catchment/site), data=df_model)
summary(glob_mod.1)


glob_mod.2 <- lmer(TSS~ scale(lag_C_PPT) + scale(tsf) + scale(flow_filled) +
                    scale(DO)+ scale(pH) +
                   (1 | catchment/site), data=df_model)
summary(glob_mod.2)

glob_mod.3 <- lmer(TSS~ scale(lag_C_PPT) + scale(tsf) + scale(flow_filled) +
                    scale(pH) +
                   (1 | catchment/site), data=df_model)
summary(glob_mod.3)


library(MuMIn)
model.sel(glob_mod, glob_mod.1, glob_mod.2, glob_mod.3, rank = "AICc")

## best model :glob_mod
```


### Null model time + space

```{r, echo=FALSE, include =T}

null_mod <- lmer(TSS~ scale(tsf) + 
                   (1 | catchment/site), data=df_model)
summary(null_mod)

model.sel(null_mod, ppt_mod.2, glob_mod,  rank = "AICc")

## best model :glob_mod
```

## set up bayes linear model for more certainty around effect sizes 

```{r, echo=FALSE, include =T}
 
hist(df_model$TSS)

```
Strong skew - log normal distribution 


```{r}

library(brms)

mu0 <- log(mean(df_model$TSS, na.rm = TRUE))

library(brms)

priors_logn <- c(
  set_prior(sprintf("normal(%f, 2)", mu0), class = "Intercept"),
  set_prior("normal(0, 1)", class = "b"),
  set_prior("exponential(1)", class = "sd"),
  set_prior("exponential(1)", class = "sigma")
)

priors_logn

TSS_m_logn <- brm(
  TSS ~ scale(lag_C_PPT) + scale(tsf) + scale(flow_filled) +
    catchment + (1 | catchment:site),
  data   = df_model,
  family = lognormal(),
  prior  = priors_logn,
  chains = 3, cores = 3, iter = 4000,
  warmup = 2000, thin = 1

)
```

#### TSS model fit 

```{r, echo=FALSE, include =T}

pp_check(TSS_m_logn)
pp_check(TSS_m_logn, type = "hist")
pp_check(TSS_m_logn, type = "scatter_avg")


loo_mod <- loo(TSS_m_logn)
print(loo_mod)


bayes_R2(TSS_m_logn)

```

```{r, echo=FALSE, include =T}

post_TSS <- brms::posterior_summary(TSS_m_logn, pars = "^b_")
post_TSS
```

### Extract effects + 95% CI cleanly

```{r}
library(tidybayes)
library(dplyr)

effects_df_TSS <- data.frame(
  term = gsub("^b_", "", rownames(post)),
  estimate = post_TSS[, "Estimate"],
  l95 = post_TSS[, "Q2.5"],
  u95 = post_TSS[, "Q97.5"]
)

# exponentiated (multiplicative effects on TSS)
effects_df_TSS$estimate_exp <- round(exp(effects_df_TSS$estimate),2)
effects_df_TSS$l95_exp <- round(exp(effects_df_TSS$l95),2)
effects_df_TSS$u95_exp <- round(exp(effects_df_TSS$u95),2)

effects_df_TSS


library(ggplot2)

effects_df_TSS %>%
  filter(term != "Intercept") %>%
  ggplot(aes(x = (estimate_exp), y = term)) +
  geom_point() +
  geom_errorbar(aes(xmin = l95_exp, xmax = u95_exp), width = 0.1) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(x = NULL, y = "Drivers of TSS (exp(beta))") +
  theme_classic()


bayes_R2 <-round(bayes_R2(TSS_m_logn),2)

effects_TSS_plot <- effects_df_TSS%>%
filter(term != "Intercept" &  term != "catchmentwinters") %>%
  mutate(analyte = "TSS",
         bayes_R2 = 0.41)

```

Exponentiated the coefficients due to the log distribution for effects < 1 are not ecologically meaningful. 


### DOC dynamics

```{r, echo=FALSE, include =T}
 
vars <- c("DOC_QC","lag_C_PPT","tsf","flow_filled","temp","DO","EC","pH","site")

## merge hydro and solute data 
df_model <- df_WQ_plot %>%
  left_join(new_df_hydro, by =c("site", "date")) %>%
  filter(site!="ophir") %>%
  select(all_of(vars)) %>%
  tidyr::drop_na() %>%
    mutate(catchment = case_when(
    site == "winters_usgs" ~ "winters",
    site == "winters_up" ~ "winters",
    site == "browns" ~ "browns", 
    site == "browns_sub" ~ "browns"))



hist(df_model$DOC_QC)

```

Some skew 

```{r, echo=FALSE, include =T}


library(brms)

mu0 <- round(log(mean(df_model$DOC_QC, na.rm = TRUE)),2)


priors_logn <- c(
  set_prior(sprintf("normal(%f, 2)", mu0), class = "Intercept"),
  set_prior("normal(0, 1)", class = "b"),
  set_prior("exponential(1)", class = "sd"),
  set_prior("exponential(1)", class = "sigma")
)

priors_logn

DOC_m_logn <- brm(
  DOC_QC ~ scale(lag_C_PPT) + scale(tsf) + scale(flow_filled) +
    catchment + (1 | catchment:site),
  data   = df_model,
  family = lognormal(),
  prior  = priors_logn,
  chains = 3, cores = 3, iter = 4000,
  warmup = 2000, thin = 1

)

plot(DOC_m_logn)

```

#### DOC model fit 

```{r, echo=FALSE, include =T}

pp_check(DOC_m_logn)
pp_check(DOC_m_logn, type = "hist")
pp_check(DOC_m_logn, type = "scatter_avg")


loo_mod <- loo(DOC_m_logn)
print(loo_mod)


bayes_R2(DOC_m_logn)

```


### Extract effects + 95% CI cleanly

```{r, echo=FALSE, include =T}

library(tidybayes)
library(dplyr)

post_DOC <- brms::posterior_summary(DOC_m_logn, pars = "^b_")
post_DOC

effects_df_DOC <- data.frame(
  term = gsub("^b_", "", rownames(post)),
  estimate = post_DOC[, "Estimate"],
  l95 = post_DOC[, "Q2.5"],
  u95 = post_DOC[, "Q97.5"]
)

# exponentiated (multiplicative effects on DOC)
effects_df_DOC$estimate_exp <- round(exp(effects_df_DOC$estimate),2)
effects_df_DOC$l95_exp <- round(exp(effects_df_DOC$l95),2)
effects_df_DOC$u95_exp <- round(exp(effects_df_DOC$u95),2)

effects_df_DOC


library(ggplot2)

effects_df_DOC %>%
  filter(term != "Intercept") %>%
  ggplot(aes(x = (estimate_exp), y = term)) +
  geom_point() +
  geom_errorbar(aes(xmin = l95_exp, xmax = u95_exp), width = 0.1) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(x = NULL, y = "Drivers of DOC (exp(beta))") +
  theme_classic()


bayes_R2 <-round(bayes_R2(DOC_m_logn),2)

effects_DOC_plot <- effects_df_DOC%>%
filter(term != "Intercept" &  term != "catchmentwinters") %>%
  mutate(analyte = "DOC",
         bayes_R2 = 0.14)

```




### TDN dynamics

```{r, echo=FALSE, include =T}
 
vars <- c("TDN_QC","lag_C_PPT","tsf","flow_filled","temp","DO","EC","pH","site")

## merge hydro and solute data 
df_model <- df_WQ_plot %>%
  left_join(new_df_hydro, by =c("site", "date")) %>%
  filter(site!="ophir") %>%
  select(all_of(vars)) %>%
  tidyr::drop_na() %>%
    mutate(catchment = case_when(
    site == "winters_usgs" ~ "winters",
    site == "winters_up" ~ "winters",
    site == "browns" ~ "browns", 
    site == "browns_sub" ~ "browns"))

hist(df_model$TDN_QC)

```

Some skew 

```{r, echo=FALSE, include =T}


library(brms)

mu0 <- round(log(mean(df_model$TDN_QC, na.rm = TRUE)),2)


priors_logn <- c(
  set_prior(sprintf("normal(%f, 2)", mu0), class = "Intercept"),
  set_prior("normal(0, 1)", class = "b"),
  set_prior("exponential(1)", class = "sd"),
  set_prior("exponential(1)", class = "sigma")
)

priors_logn

TDN_m_logn <- brm(
  TDN_QC ~ scale(lag_C_PPT) + scale(tsf) + scale(flow_filled) +
    catchment + (1 | catchment:site),
  data   = df_model,
  family = lognormal(),
  prior  = priors_logn,
  chains = 3, cores = 3, iter = 4000,
  warmup = 2000, thin = 1

)

plot(TDN_m_logn)

```

#### TDN model fit 

```{r, echo=FALSE, include =T}

pp_check(TDN_m_logn)
pp_check(TDN_m_logn, type = "hist")
pp_check(TDN_m_logn, type = "scatter_avg")


loo_mod <- loo(TDN_m_logn)
print(loo_mod)


bayes_R2(TDN_m_logn)

```


### Extract effects + 95% CI cleanly

```{r, echo=FALSE, include =T}

library(tidybayes)
library(dplyr)

post_TDN <- brms::posterior_summary(TDN_m_logn, pars = "^b_")
post_TDN

effects_df_TDN <- data.frame(
  term = gsub("^b_", "", rownames(post)),
  estimate = post_TDN[, "Estimate"],
  l95 = post_TDN[, "Q2.5"],
  u95 = post_TDN[, "Q97.5"]
)

# exponentiated (multiplicative effects on TDN)
effects_df_TDN$estimate_exp <- round(exp(effects_df_TDN$estimate),2)
effects_df_TDN$l95_exp <- round(exp(effects_df_TDN$l95),2)
effects_df_TDN$u95_exp <- round(exp(effects_df_TDN$u95),2)

effects_df_TDN


library(ggplot2)

effects_df_TDN %>%
  filter(term != "Intercept") %>%
  ggplot(aes(x = (estimate_exp), y = term)) +
  geom_point() +
  geom_errorbar(aes(xmin = l95_exp, xmax = u95_exp), width = 0.1) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(x = NULL, y = "Drivers of TDN (exp(beta))") +
  theme_classic()

bayes_R2 <-round(bayes_R2(TDN_m_logn),2)

effects_TDN_plot <- effects_df_TDN%>%
filter(term != "Intercept" &  term != "catchmentwinters") %>%
  mutate(analyte = "TDN",
         bayes_R2 = 0.31)

```





### PO4.P dynamics

```{r, echo=FALSE, include =T}
 
vars <- c("PO4.P_QC","lag_C_PPT","tsf","flow_filled","temp","DO","EC","pH","site")

## merge hydro and solute data 
df_model <- df_WQ_plot %>%
  left_join(new_df_hydro, by =c("site", "date")) %>%
  filter(site!="ophir") %>%
  select(all_of(vars)) %>%
  tidyr::drop_na() %>%
    mutate(catchment = case_when(
    site == "winters_usgs" ~ "winters",
    site == "winters_up" ~ "winters",
    site == "browns" ~ "browns", 
    site == "browns_sub" ~ "browns"))



hist(df_model$PO4.P_QC)

```

Some skew 

```{r, echo=FALSE, include =T}


library(brms)

mu0 <- round(log(mean(df_model$PO4.P_QC, na.rm = TRUE)),2)


priors_logn <- c(
  set_prior(sprintf("normal(%f, 2)", mu0), class = "Intercept"),
  set_prior("normal(0, 1)", class = "b"),
  set_prior("exponential(1)", class = "sd"),
  set_prior("exponential(1)", class = "sigma")
)

priors_logn

PO4.P_m_logn <- brm(
  PO4.P_QC ~ scale(lag_C_PPT) + scale(tsf) + scale(flow_filled) +
    catchment + (1 | catchment:site),
  data   = df_model,
  family = lognormal(),
  prior  = priors_logn,
  chains = 3, cores = 3, iter = 4000,
  warmup = 2000, thin = 1

)

plot(PO4.P_m_logn)

```

#### PO4.P model fit 

```{r, echo=FALSE, include =T}

pp_check(PO4.P_m_logn)
pp_check(PO4.P_m_logn, type = "hist")
pp_check(PO4.P_m_logn, type = "scatter_avg")


loo_mod <- loo(PO4.P_m_logn)
print(loo_mod)


bayes_R2(PO4.P_m_logn)

```


### Extract effects + 95% CI cleanly

```{r, echo=FALSE, include =T}

library(tidybayes)
library(dplyr)

post_PO4.P <- brms::posterior_summary(PO4.P_m_logn, pars = "^b_")
post_PO4.P

effects_df_PO4.P <- data.frame(
  term = gsub("^b_", "", rownames(post)),
  estimate = post_PO4.P[, "Estimate"],
  l95 = post_PO4.P[, "Q2.5"],
  u95 = post_PO4.P[, "Q97.5"]
)

# exponentiated (multiplicative effects on PO4.P)
effects_df_PO4.P$estimate_exp <- round(exp(effects_df_PO4.P$estimate),2)
effects_df_PO4.P$l95_exp <- round(exp(effects_df_PO4.P$l95),2)
effects_df_PO4.P$u95_exp <- round(exp(effects_df_PO4.P$u95),2)

effects_df_PO4.P


library(ggplot2)

effects_df_PO4.P %>%
  filter(term != "Intercept") %>%
  ggplot(aes(x = (estimate_exp), y = term)) +
  geom_point() +
  geom_errorbar(aes(xmin = l95_exp, xmax = u95_exp), width = 0.1) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(x = NULL, y = "Drivers of PO4.P (exp(beta))") +
  theme_classic()


bayes_R2 <-round(bayes_R2(PO4.P_m_logn),2)

effects_PO4_plot <- effects_df_PO4.P%>%
filter(term != "Intercept" &  term != "catchmentwinters") %>%
  mutate(analyte = "PO4",
         bayes_R2 = 0.13)

```


### metals ?


### Al dynamics

```{r, echo=FALSE, include =T}
 
vars <- c("Al_QC","lag_C_PPT","tsf","flow_filled","temp","DO","EC","pH","site")

## merge hydro and solute data 
df_model <- df_WQ_plot %>%
  left_join(new_df_hydro, by =c("site", "date")) %>%
  filter(site!="ophir") %>%
  select(all_of(vars)) %>%
  tidyr::drop_na() %>%
    mutate(catchment = case_when(
    site == "winters_usgs" ~ "winters",
    site == "winters_up" ~ "winters",
    site == "browns" ~ "browns", 
    site == "browns_sub" ~ "browns"))



hist(df_model$Al_QC)

```

Some skew 

```{r, echo=FALSE, include =T}


library(brms)

mu0 <- round(log(mean(df_model$Al_QC, na.rm = TRUE)),2)


priors_logn <- c(
  set_prior(sprintf("normal(%f, 2)", mu0), class = "Intercept"),
  set_prior("normal(0, 1)", class = "b"),
  set_prior("exponential(1)", class = "sd"),
  set_prior("exponential(1)", class = "sigma")
)

priors_logn

Al_m_logn <- brm(
  Al_QC ~ scale(lag_C_PPT) + scale(tsf) + scale(flow_filled) +
    catchment + (1 | catchment:site),
  data   = df_model,
  family = lognormal(),
  prior  = priors_logn,
  chains = 3, cores = 3, iter = 4000,
  warmup = 2000, thin = 1

)

plot(Al_m_logn)

```

#### Al model fit 

```{r, echo=FALSE, include =T}

pp_check(Al_m_logn)
pp_check(Al_m_logn, type = "hist")
pp_check(Al_m_logn, type = "scatter_avg")


loo_mod <- loo(Al_m_logn)
print(loo_mod)


bayes_R2(Al_m_logn)

```


### Extract effects + 95% CI cleanly

```{r, echo=FALSE, include =T}

library(tidybayes)
library(dplyr)

post_Al <- brms::posterior_summary(Al_m_logn, pars = "^b_")
post_Al

effects_df_Al <- data.frame(
  term = gsub("^b_", "", rownames(post)),
  estimate = post_Al[, "Estimate"],
  l95 = post_Al[, "Q2.5"],
  u95 = post_Al[, "Q97.5"]
)

# exponentiated (multiplicative effects on Al)
effects_df_Al$estimate_exp <- round(exp(effects_df_Al$estimate),2)
effects_df_Al$l95_exp <- round(exp(effects_df_Al$l95),2)
effects_df_Al$u95_exp <- round(exp(effects_df_Al$u95),2)

effects_df_Al


library(ggplot2)

effects_df_Al %>%
  filter(term != "Intercept") %>%
  ggplot(aes(x = (estimate_exp), y = term)) +
  geom_point() +
  geom_errorbar(aes(xmin = l95_exp, xmax = u95_exp), width = 0.1) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(x = NULL, y = "Drivers of Al (exp(beta))") +
  theme_classic()


bayes_R2 <-round(bayes_R2(Al_m_logn),2)

effects_Al_plot <- effects_df_Al%>%
filter(term != "Intercept" &  term != "catchmentwinters") %>%
  mutate(analyte = "Al",
         bayes_R2 = 0.25)

```



### Ba dynamics

```{r, echo=FALSE, include =T}

 
vars <- c("Ba_QC","lag_C_PPT","tsf","flow_filled","temp","DO","EC","pH","site")

## merge hydro and solute data 
df_model <- df_WQ_plot %>%
  left_join(new_df_hydro, by =c("site", "date")) %>%
  filter(site!="ophir") %>%
  select(all_of(vars)) %>%
  tidyr::drop_na() %>%
    mutate(catchment = case_when(
    site == "winters_usgs" ~ "winters",
    site == "winters_up" ~ "winters",
    site == "browns" ~ "browns", 
    site == "browns_sub" ~ "browns"))

hist(df_model$Ba_QC)

```

Some skew 

```{r, echo=FALSE, include =T}


library(brms)

mu0 <- round(log(mean(df_model$Ba_QC, na.rm = TRUE)),2)


priors_logn <- c(
  set_prior(sprintf("normal(%f, 2)", mu0), class = "Intercept"),
  set_prior("normal(0, 1)", class = "b"),
  set_prior("exponential(1)", class = "sd"),
  set_prior("exponential(1)", class = "sigma")
)

priors_logn

Ba_m_logn <- brm(
  Ba_QC ~ scale(lag_C_PPT) + scale(tsf) + scale(flow_filled) +
    catchment + (1 | catchment:site),
  data   = df_model,
  family = lognormal(),
  prior  = priors_logn,
  chains = 3, cores = 3, iter = 4000,
  warmup = 2000, thin = 1

)

plot(Ba_m_logn)

```

#### Ba model fit 

```{r, echo=FALSE, include =T}

pp_check(Ba_m_logn)
pp_check(Ba_m_logn, type = "hist")
pp_check(Ba_m_logn, type = "scatter_avg")


loo_mod <- loo(Ba_m_logn)
print(loo_mod)


bayes_R2(Ba_m_logn)

```


### Extract effects + 95% CI cleanly

```{r, echo=FALSE, include =T}

library(tidybayes)
library(dplyr)

post_Ba <- brms::posterior_summary(Ba_m_logn, pars = "^b_")
post_Ba

effects_df_Ba <- data.frame(
  term = gsub("^b_", "", rownames(post)),
  estimate = post_Ba[, "Estimate"],
  l95 = post_Ba[, "Q2.5"],
  u95 = post_Ba[, "Q97.5"]
)

# exponentiated (multiplicative effects on Al)
effects_df_Ba$estimate_exp <- round(exp(effects_df_Ba$estimate),2)
effects_df_Ba$l95_exp <- round(exp(effects_df_Ba$l95),2)
effects_df_Ba$u95_exp <- round(exp(effects_df_Ba$u95),2)

effects_df_Ba


library(ggplot2)

effects_df_Ba %>%
  filter(term != "Intercept") %>%
  ggplot(aes(x = (estimate_exp), y = term)) +
  geom_point() +
  geom_errorbar(aes(xmin = l95_exp, xmax = u95_exp), width = 0.1) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(x = NULL, y = "Drivers of Al (exp(beta))") +
  theme_classic()

bayes_R2 <-round(bayes_R2(Ba_m_logn),2)

effects_Ba_plot <- effects_df_Ba%>%
filter(term != "Intercept" &  term != "catchmentwinters") %>%
  mutate(analyte = "Ba",
         bayes_R2 = 0.61)

```





### Ca dynamics

```{r, echo=FALSE, include =T}

 
vars <- c("Ca_QC","lag_C_PPT","tsf","flow_filled","temp","DO","EC","pH","site")

## merge hydro and solute data 
df_model <- df_WQ_plot %>%
  left_join(new_df_hydro, by =c("site", "date")) %>%
  filter(site!="ophir") %>%
  select(all_of(vars)) %>%
  tidyr::drop_na() %>%
    mutate(catchment = case_when(
    site == "winters_usgs" ~ "winters",
    site == "winters_up" ~ "winters",
    site == "browns" ~ "browns", 
    site == "browns_sub" ~ "browns"))

hist(df_model$Ca_QC)

```

Some skew 

```{r, echo=FALSE, include =T}


library(brms)

mu0 <- round(log(mean(df_model$Ca_QC, na.rm = TRUE)),2)


priors_logn <- c(
  set_prior(sprintf("normal(%f, 2)", mu0), class = "Intercept"),
  set_prior("normal(0, 1)", class = "b"),
  set_prior("exponential(1)", class = "sd"),
  set_prior("exponential(1)", class = "sigma")
)

priors_logn

Ca_m_logn <- brm(
  Ca_QC ~ scale(lag_C_PPT) + scale(tsf) + scale(flow_filled) +
    catchment + (1 | catchment:site),
  data   = df_model,
  family = lognormal(),
  prior  = priors_logn,
  chains = 3, cores = 3, iter = 4000,
  warmup = 2000, thin = 1

)

plot(Ca_m_logn)

```

#### Ca model fit 

```{r, echo=FALSE, include =T}

pp_check(Ca_m_logn)
pp_check(Ca_m_logn, type = "hist")
pp_check(Ca_m_logn, type = "scatter_avg")


loo_mod <- loo(Ca_m_logn)
print(loo_mod)


bayes_R2(Ca_m_logn)

```


### Extract effects + 95% CI cleanly

```{r, echo=FALSE, include =T}

library(dplyr)

post_Ca <- brms::posterior_summary(Ca_m_logn, pars = "^b_")
post_Ca

effects_df_Ca <- data.frame(
  term = gsub("^b_", "", rownames(post)),
  estimate = post_Ca[, "Estimate"],
  l95 = post_Ca[, "Q2.5"],
  u95 = post_Ca[, "Q97.5"]
)

# exponentiated (multiplicative effects on Al)
effects_df_Ca$estimate_exp <- round(exp(effects_df_Ca$estimate),2)
effects_df_Ca$l95_exp <- round(exp(effects_df_Ca$l95),2)
effects_df_Ca$u95_exp <- round(exp(effects_df_Ca$u95),2)

effects_df_Ca


library(ggplot2)

effects_df_Ca %>%
  filter(term != "Intercept") %>%
  ggplot(aes(x = (estimate_exp), y = term)) +
  geom_point() +
  geom_errorbar(aes(xmin = l95_exp, xmax = u95_exp), width = 0.1) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(x = NULL, y = "Drivers of Al (exp(beta))") +
  theme_classic()

bayes_R2 <-round(bayes_R2(Ca_m_logn),2)

effects_Ca_plot <- effects_df_Ca%>%
filter(term != "Intercept" &  term != "catchmentwinters") %>%
  mutate(analyte = "Ca",
         bayes_R2 = 0.36)

```




### Fe dynamics

```{r, echo=FALSE, include =T}

 
vars <- c("Fe_QC","lag_C_PPT","tsf","flow_filled","temp","DO","EC","pH","site")

## merge hydro and solute data 
df_model <- df_WQ_plot %>%
  left_join(new_df_hydro, by =c("site", "date")) %>%
  filter(site!="ophir") %>%
  select(all_of(vars)) %>%
  tidyr::drop_na() %>%
    mutate(catchment = case_when(
    site == "winters_usgs" ~ "winters",
    site == "winters_up" ~ "winters",
    site == "browns" ~ "browns", 
    site == "browns_sub" ~ "browns"))



hist(df_model$Fe_QC)

```

Some skew 

```{r, echo=FALSE, include =T}


library(brms)

mu0 <- round(log(mean(df_model$Fe_QC, na.rm = TRUE)),2)


priors_logn <- c(
  set_prior(sprintf("normal(%f, 2)", mu0), class = "Intercept"),
  set_prior("normal(0, 1)", class = "b"),
  set_prior("exponential(1)", class = "sd"),
  set_prior("exponential(1)", class = "sigma")
)

priors_logn

Fe_m_logn <- brm(
  Fe_QC ~ scale(lag_C_PPT) + scale(tsf) + scale(flow_filled) +
    catchment + (1 | catchment:site),
  data   = df_model,
  family = lognormal(),
  prior  = priors_logn,
  chains = 3, cores = 3, iter = 4000,
  warmup = 2000, thin = 1

)

plot(Fe_m_logn)

```

#### Fe model fit 

```{r, echo=FALSE, include =T}

pp_check(Fe_m_logn)
pp_check(Fe_m_logn, type = "hist")


loo_mod <- loo(Fe_m_logn)
print(loo_mod)


bayes_R2(Fe_m_logn)

```


### Extract effects + 95% CI cleanly

```{r, echo=FALSE, include =T}

library(dplyr)

post_Fe <- brms::posterior_summary(Fe_m_logn, pars = "^b_")
post_Fe

effects_df_Fe <- data.frame(
  term = gsub("^b_", "", rownames(post)),
  estimate = post_Fe[, "Estimate"],
  l95 = post_Fe[, "Q2.5"],
  u95 = post_Fe[, "Q97.5"]
)

effects_df_Fe$estimate_exp <- round(exp(effects_df_Fe$estimate),2)
effects_df_Fe$l95_exp <- round(exp(effects_df_Fe$l95),2)
effects_df_Fe$u95_exp <- round(exp(effects_df_Fe$u95),2)

effects_df_Fe


library(ggplot2)

effects_df_Fe %>%
  filter(term != "Intercept") %>%
  ggplot(aes(x = (estimate_exp), y = term)) +
  geom_point() +
  geom_errorbar(aes(xmin = l95_exp, xmax = u95_exp), width = 0.1) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(x = NULL, y = "Drivers of Al (exp(beta))") +
  theme_classic()

bayes_R2 <-round(bayes_R2(Fe_m_logn),2)

effects_Fe_plot <- effects_df_Fe%>%
filter(term != "Intercept" &  term != "catchmentwinters") %>%
  mutate(analyte = "Fe",
         bayes_R2 = 0.59)

```






### Mn dynamics

```{r, echo=FALSE, include =T}

 
vars <- c("Mn_QC","lag_C_PPT","tsf","flow_filled","temp","DO","EC","pH","site")

## merge hydro and solute data 
df_model <- df_WQ_plot %>%
  left_join(new_df_hydro, by =c("site", "date")) %>%
  filter(site!="ophir") %>%
  select(all_of(vars)) %>%
  tidyr::drop_na() %>%
    mutate(catchment = case_when(
    site == "winters_usgs" ~ "winters",
    site == "winters_up" ~ "winters",
    site == "browns" ~ "browns", 
    site == "browns_sub" ~ "browns"))



hist(df_model$Mn_QC)

```

Some skew 

```{r, echo=FALSE, include =T}


library(brms)

mu0 <- round(log(mean(df_model$Mn_QC, na.rm = TRUE)),2)


priors_logn <- c(
  set_prior(sprintf("normal(%f, 2)", mu0), class = "Intercept"),
  set_prior("normal(0, 1)", class = "b"),
  set_prior("exponential(1)", class = "sd"),
  set_prior("exponential(1)", class = "sigma")
)

priors_logn

Mn_m_logn <- brm(
  Mn_QC ~ scale(lag_C_PPT) + scale(tsf) + scale(flow_filled) +
    catchment + (1 | catchment:site),
  data   = df_model,
  family = lognormal(),
  prior  = priors_logn,
  chains = 3, cores = 3, iter = 4000,
  warmup = 2000, thin = 1

)

plot(Mn_m_logn)

```

#### Mn model fit 

```{r, echo=FALSE, include =T}

pp_check(Mn_m_logn)
pp_check(Mn_m_logn, type = "hist")


loo_mod <- loo(Mn_m_logn)
print(loo_mod)


bayes_R2(Mn_m_logn)

```


### Extract effects + 95% CI cleanly

```{r, echo=FALSE, include =T}

library(dplyr)

post_Mn <- brms::posterior_summary(Mn_m_logn, pars = "^b_")
post_Mn

effects_df_Mn <- data.frame(
  term = gsub("^b_", "", rownames(post)),
  estimate = post_Mn[, "Estimate"],
  l95 = post_Mn[, "Q2.5"],
  u95 = post_Mn[, "Q97.5"]
)

effects_df_Mn$estimate_exp <- round(exp(effects_df_Mn$estimate),2)
effects_df_Mn$l95_exp <- round(exp(effects_df_Mn$l95),2)
effects_df_Mn$u95_exp <- round(exp(effects_df_Mn$u95),2)

effects_df_Mn


library(ggplot2)

effects_df_Mn %>%
  filter(term != "Intercept") %>%
  ggplot(aes(x = (estimate_exp), y = term)) +
  geom_point() +
  geom_errorbar(aes(xmin = l95_exp, xmax = u95_exp), width = 0.1) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(x = NULL, y = "Drivers of Al (exp(beta))") +
  theme_classic()


bayes_R2 <-round(bayes_R2(Mn_m_logn),2)

effects_Mn_plot <- effects_df_Mn%>%
filter(term != "Intercept" &  term != "catchmentwinters") %>%
  mutate(analyte = "Mn",
         bayes_R2 = 0.68)

```





### Sr dynamics

```{r, echo=FALSE, include =T}

 
vars <- c("Sr_QC","lag_C_PPT","tsf","flow_filled","temp","DO","EC","pH","site")

## merge hydro and solute data 
df_model <- df_WQ_plot %>%
  left_join(new_df_hydro, by =c("site", "date")) %>%
  filter(site!="ophir") %>%
  select(all_of(vars)) %>%
  tidyr::drop_na() %>%
    mutate(catchment = case_when(
    site == "winters_usgs" ~ "winters",
    site == "winters_up" ~ "winters",
    site == "browns" ~ "browns", 
    site == "browns_sub" ~ "browns"))



hist(df_model$Sr_QC)

```

Some skew 

```{r, echo=FALSE, include =T}


library(brms)

mu0 <- round(log(mean(df_model$Sr_QC, na.rm = TRUE)),2)


priors_logn <- c(
  set_prior(sprintf("normal(%f, 2)", mu0), class = "Intercept"),
  set_prior("normal(0, 1)", class = "b"),
  set_prior("exponential(1)", class = "sd"),
  set_prior("exponential(1)", class = "sigma")
)

priors_logn

Sr_m_logn <- brm(
  Sr_QC ~ scale(lag_C_PPT) + scale(tsf) + scale(flow_filled) +
    catchment + (1 | catchment:site),
  data   = df_model,
  family = lognormal(),
  prior  = priors_logn,
  chains = 3, cores = 3, iter = 4000,
  warmup = 2000, thin = 1

)

plot(Sr_m_logn)

```

#### Sr model fit 

```{r, echo=FALSE, include =T}

pp_check(Sr_m_logn)
pp_check(Sr_m_logn, type = "hist")


loo_mod <- loo(Sr_m_logn)
print(loo_mod)


bayes_R2(Sr_m_logn)


```


### Extract effects + 95% CI cleanly

```{r, echo=FALSE, include =T}

library(dplyr)

post_Sr <- brms::posterior_summary(Sr_m_logn, pars = "^b_")
post_Sr

effects_df_Sr <- data.frame(
  term = gsub("^b_", "", rownames(post)),
  estimate = post_Sr[, "Estimate"],
  l95 = post_Sr[, "Q2.5"],
  u95 = post_Sr[, "Q97.5"]
)

effects_df_Sr$estimate_exp <- round(exp(effects_df_Sr$estimate),2)
effects_df_Sr$l95_exp <- round(exp(effects_df_Sr$l95),2)
effects_df_Sr$u95_exp <- round(exp(effects_df_Sr$u95),2)

effects_df_Sr


library(ggplot2)

effects_df_Sr %>%
  filter(term != "Intercept") %>%
  ggplot(aes(x = (estimate_exp), y = term)) +
  geom_point() +
  geom_errorbar(aes(xmin = l95_exp, xmax = u95_exp), width = 0.1) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(x = NULL, y = "Drivers of Al (exp(beta))") +
  theme_classic()


bayes_R2 <-round(bayes_R2(Sr_m_logn),2)

effects_Sr_plot <- effects_df_Sr%>%
filter(term != "Intercept" &  term != "catchmentwinters") %>%
  mutate(analyte = "Sr",
         bayes_R2 = 0.56)

```

```{r, echo=FALSE, include =T}

coeff_plt_df <- rbind(effects_Sr_plot, effects_Fe_plot, effects_Mn_plot, effects_Ba_plot, effects_Ca_plot, effects_Al_plot, effects_PO4_plot, effects_TDN_plot, effects_DOC_plot, effects_TSS_plot)

```

```{r, fig.width=9, fig.height=9, out.width="90%"}

library(dplyr)
library(stringr)
library(forcats)
library(ggplot2)

# 1) Order analytes by highest estimate_exp (within analyte)
analyte_order <- coeff_plt_df %>%
  group_by(analyte) %>%
  summarise(max_est = max(estimate_exp, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(max_est)) %>%
  pull(analyte)

# 2) Build a label row per analyte (for the y-axis "header")
headers <- coeff_plt_df %>%
  distinct(analyte, bayes_R2) %>%
  mutate(
    term = NA_character_,
    estimate_exp = NA_real_,
    l95_exp = NA_real_,
    u95_exp = NA_real_,
    # y label: analyte + bayes R2
    y_lab = sprintf("%s (R² = %.2f)", analyte, bayes_R2),
    is_header = TRUE
  )

# 3) Build term rows (indented)
terms <- coeff_plt_df %>%
  mutate(
    # nice-ish term names (edit as you like)
    term_nice = recode(term,
      "scalelag_C_PPT"    = "Lagged PPT (z)",
      "scaletsf"          = "TSF (z)",
      "scaleflow_filled"  = "Flow (z)",
      .default = term
    ),
    y_lab = paste0("   ", term_nice),
    is_header = FALSE
  )

# 4) Combine headers + terms, and order within analyte
plot_df <- bind_rows(headers, terms) %>%
  mutate(
    analyte = factor(analyte, levels = analyte_order)
  ) %>%
  arrange(analyte, desc(is_header), desc(estimate_exp)) %>%
  group_by(analyte) %>%
  mutate(row_in_analyte = row_number()) %>%
  ungroup() %>%
  # create a unique ordered y factor (keeps headers above their terms)
  mutate(y = factor(paste(analyte, row_in_analyte, y_lab, sep = "___"),
                    levels = unique(paste(analyte, row_in_analyte, y_lab, sep = "___"))),
         y_text = y_lab)

# 5) Plot
ggplot(plot_df, aes(x = estimate_exp, y = y)) +
  geom_vline(xintercept = 1, linetype = 2) +
  geom_errorbarh(
    data = subset(plot_df, !is_header),
    aes(xmin = l95_exp, xmax = u95_exp),
    height = 0.25
  ) +
  geom_point(
    data = subset(plot_df, !is_header),
    size = 2
  ) +
  scale_y_discrete(labels = plot_df$y_text) +
  scale_x_log10() +  # OPTIONAL: log scale is often nice for multiplicative effects
  labs(
    x = "Multiplicative effect on response (exp(beta))",
    y = NULL
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )


```


```{r, fig.width=9, fig.height=9, out.width="90%"}


# 4) Combine headers + terms, and order within analyte
plot_df <- bind_rows(headers, terms) %>%
  mutate(
    analyte = factor(analyte, levels = analyte_order)
  ) %>%
  arrange(analyte, desc(is_header), desc(estimate_exp)) %>%
  group_by(analyte) %>%
  mutate(row_in_analyte = row_number()) %>%
  ungroup() %>%
  # create a unique ordered y factor (keeps headers above their terms)
  mutate(y = factor(paste(analyte, row_in_analyte, y_lab, sep = "___"),
                    levels = unique(paste(analyte, row_in_analyte, y_lab, sep = "___"))),
         y_text = y_lab)


```


##### Colloquim figures

```{r, echo=FALSE, include =T}

### TSS

# ggsave(plot = DMC_TSS_plot, filename = paste("/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Figures/DMC_TSS.png",sep=""),width=13,height=4,dpi=300)

# ggsave(plot = TSS_plot, filename = paste("/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Figures/DMC_TSS_bp.png",sep=""),width=4,height=3.5,dpi=300)

### DOC

# ggsave(plot = DMC_DOC_plot, filename = paste("/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Figures/DMC_DOC.png",sep=""),width=13,height=4,dpi=300)

# ggsave(plot = DOC_plot, filename = paste("/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Figures/DMC_DOC_bp.png",sep=""),width=4,height=3.5,dpi=300)


### TDN

# ggsave(plot = DMC_TDN_plot, filename = paste("/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Figures/DMC_TDN.png",sep=""),width=13,height=4,dpi=300)

# ggsave(plot = TDN_plot, filename = paste("/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Figures/DMC_TDN_bp.png",sep=""),width=4,height=3.5,dpi=300)



### PO4

# ggsave(plot = DMC_PO4_plot, filename = paste("/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Figures/DMC_PO4.png",sep=""),width=13,height=4,dpi=300)

# ggsave(plot = PO4_plot, filename = paste("/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Figures/DMC_PO4_bp.png",sep=""),width=4,height=3.5,dpi=300)



```





```{r, echo=FALSE, include =T}
 
hist(df_model$DOC)

```




End of script. 