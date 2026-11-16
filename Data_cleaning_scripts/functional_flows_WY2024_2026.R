# Functional flows: WY2024-WY2026, all sites, dischargeCMS (m3/s)
# Adaptation of Yarnell et al. (2020); thresholds exploratory, not prescribed.
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(ggplot2)


flow_path <- "/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Processes_data/ERDC_USGS_flow_data_20261026.csv"
ppt_path <- "/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Raw_data/prism_cat_new.csv"

# Load and prepare flow data

library(dplyr)
library(lubridate)
library(readxl)


# 1. Load original flow data
flow_dat <- read.csv(flow_path) %>%
  mutate(
    date = as.Date(date),
  ) %>%
  mutate(site = case_when(
    site == "ophir"        ~ "ophir",
    site == "winters" ~ "winters_usgs",
    site == "davis"   ~ "davis"
  )) %>% 
  mutate(stream = case_when(
    site == "ophir"        ~ "ophir",
    site == "winters_usgs" ~ "winters",
    site == "winters_up" ~ "winters",
    site == "davis"   ~ "davis",
    site == "browns_sub"   ~ "browns",
    site == "browns"   ~ "browns"
  )) %>%
  select(site, stream, date, dischargeCMS) %>%
  distinct(site, date, .keep_all = TRUE) %>%
  filter(date> as.Date("2023-09-29"))

# 2. Load cross-section observations
cross_flow <- read_excel(
  "/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Raw_data/Davis_fire_cross_flow.xlsx"
) %>%
  mutate(
    date = as.Date(date_sample),
    dischargeCMS = as.numeric(cross_section_flow_cms),
    site = as.character(site_lab)
  ) %>%
  mutate(stream = case_when(
    site == "ophir"        ~ "ophir",
    site == "winters_usgs" ~ "winters",
    site == "winters_up" ~ "winters",
    site == "davis"   ~ "davis",
    site == "browns_sub"   ~ "browns",
    site == "browns"   ~ "browns"
  )) %>%
  select(site, stream, date, dischargeCMS) %>%
  distinct(site, stream, date, .keep_all = TRUE)

# 3. Keep cross-section measurements only when
# the original flow dataset has no observation
cross_flow_new <- cross_flow %>%
  anti_join(
    flow_dat %>% select(site, stream, date),
    by = c("site","stream", "date")
  )

# 4. Combine datasets
flow_dat <- bind_rows(
  flow_dat,
  cross_flow_new
) %>%
  arrange(site, date)

# 5. Verify no duplicates remain
stopifnot(
  !anyDuplicated(flow_dat[c("site", "date")])
)

# 6. Check observations by site
flow_dat %>%
  count(site, name = "n_days") 

  

# Load and prepare precipitation data
ppt_dat <- read.csv(ppt_path) %>%
  mutate(
    date = as.Date(Date, format = "%Y-%m-%d"),
    lagPPT = lag(ppt..mm.),
    lag_C_PPT = ppt..mm. + lagPPT,
    precip_mm = ppt..mm.,
    site = Name
  ) %>%
  mutate(site = case_when(
    site == "ophir"        ~ "ophir",
    site == "winters" ~ "winters_usgs",
    site == "davis"   ~ "davis",
    site == "browns"   ~ "browns"
  )) %>% 
  mutate(stream = case_when(
    site == "ophir"        ~ "ophir",
    site == "winters_usgs" ~ "winters",
    site == "winters_up" ~ "winters",
    site == "davis"   ~ "davis",
    site == "browns_sub"   ~ "browns",
    site == "browns"   ~ "browns"
  )) %>%
  select(site, stream, date, precip_mm, lag_C_PPT, precip_mm)

# Join datasets
hydro_combined <- flow_dat %>%
  left_join(ppt_dat, by = c("site","stream", "date"))

settings <- list(min_complete_days=60L, min_rise_cms=0.005,
  rise_ratio=1.5, ppt_3day_min_mm=1, event_max_days=14L,
  event_recovery_fraction=.20, high_flow_quantile=.95,
  low_flow_quantile=.30, low_cv_max=.30, min_low_run_days=10L,
  min_recession_days=7L, recession_decline_days=10L,
  recession_decline_fraction=.60, min_recession_drop=.10)

season_of_month <- function(m) case_when(
  m %in% 3:5 ~ 'spring', m %in% 6:8 ~ 'summer',
  m %in% 9:11 ~ 'fall', TRUE ~ 'winter')

sites <- c('ophir','browns','browns_sub','winters_up','winters_usgs','davis')
stream <- c('ophir','browns','winters','davis')

start_date <- as.Date('2023-10-01')
end_date <- as.Date('2026-09-30')
stopifnot(all(c('site','date','dischargeCMS') %in% names(flow_dat)),
          all(c('site','date','precip_mm') %in% names(ppt_dat)))

flow_daily <- flow_dat %>% transmute(site, date=as.Date(date),
                                    Q=as.numeric(dischargeCMS)) %>%
  filter(site %in% sites, between(date,start_date,end_date))
ppt_daily <- ppt_dat %>% transmute(site,date=as.Date(date),
                                  precip_mm=as.numeric(precip_mm)) %>%
  filter(site %in% sites, between(date,start_date,end_date))
if (anyDuplicated(flow_daily[c('site','date')])) stop('Duplicate flow site-date rows; aggregate first.')
if (anyDuplicated(ppt_daily[c('site','date')])) stop('Duplicate precipitation site-date rows; aggregate first.')

hydro <- flow_daily %>% left_join(ppt_daily, by=c('site','date')) %>%
  group_by(site) %>% complete(date=seq(start_date,end_date,by='day')) %>%
  arrange(date,.by_group=TRUE) %>%
  mutate(Q=if_else(Q<0,NA_real_,Q),
         water_year=year(date)+as.integer(month(date)>=10),
         met_season=season_of_month(month(date)),
         water_year_day=as.integer(date-as.Date(paste0(water_year-1,'-10-01')))+1L,
         Q_smooth=zoo::rollapply(Q,7,function(x) if(sum(is.finite(x))>=5)
           median(x,na.rm=TRUE) else NA_real_,fill=NA_real_,align='center'),
         ppt_3day=zoo::rollapply(precip_mm,3,function(x)
           if(all(is.na(x))) NA_real_ else sum(x,na.rm=TRUE),
           fill=NA_real_,align='right',partial=TRUE)) %>% ungroup()

# Recession onset: first sustained decline in March/April, not necessarily annual maximum.
# To avoid detecting tiny fluctuations, require a meaningful net decline.
classify_year <- function(d,cfg=settings) {
  d <- arrange(d,date); n <- nrow(d)
  d$season_component <- NA_character_
  d$recession_start <- as.Date(NA)
  d$low_flow_onset <- as.Date(NA)
  d$recession_end <- as.Date(NA)
  if(sum(is.finite(d$Q)) < cfg$min_complete_days) return(d)
  d$season_component[d$met_season %in% c('fall','winter','spring')] <- 'wet_season_baseflow'
  d$season_component[d$met_season=='summer'] <- 'dry_season_baseflow'
  q <- d$Q_smooth
  recession_i <- NA_integer_
  starts <- which(month(d$date) %in% 3:4 & seq_len(n)+cfg$recession_decline_days-1L<=n)
  for(i in starts) {
    idx <- i:(i+cfg$recession_decline_days-1L)
    vals <- q[idx]
    if(anyNA(vals) || vals[1]<=0) next
    decreases <- mean(diff(vals)<0)
    net_drop <- (vals[1]-vals[length(vals)])/vals[1]
    if(decreases >= cfg$recession_decline_fraction &&
       net_drop >= cfg$min_recession_drop) { recession_i <- i; break }
  }
  # Fallback: if no sustained decline in March/April, do not invent a recession.
  if(!is.na(recession_i)) {
    d$recession_start <- d$date[recession_i]
    low_thr <- unname(quantile(d$Q,cfg$low_flow_quantile,na.rm=TRUE))
    cv10 <- zoo::rollapply(d$Q,10,function(x) {
      if(sum(is.finite(x))<8) return(NA_real_)
      mu <- mean(x,na.rm=TRUE)
      if(mu==0) return(0)
      sd(x,na.rm=TRUE)/mu
    },fill=NA_real_,align='right')
    low_flag <- is.finite(q) & q<=low_thr & is.finite(cv10) &
      cv10<=cfg$low_cv_max & month(d$date) %in% 5:8
    low_i <- NA_integer_
    last_start <- n-cfg$min_low_run_days+1L
    if(recession_i+cfg$min_recession_days <= last_start) {
      for(i in seq.int(recession_i+cfg$min_recession_days,last_start)) {
        if(all(low_flag[i:(i+cfg$min_low_run_days-1L)])) {
          low_i <- i; break
        }
      }
    }
    # Without a low-flow transition, keep recession through Aug 31,
    # but mark end as uncertain (NA) in diagnostics.
    august_end <- max(which(month(d$date)==8))
    end_i <- if(is.na(low_i)) august_end else low_i-1L
    if(end_i>=recession_i) d$season_component[recession_i:end_i] <- 'spring_recession_flow'
    if(!is.na(low_i)) {
      d$low_flow_onset <- d$date[low_i]
      d$recession_end <- d$date[end_i]
      d$season_component[low_i:n] <- 'dry_season_baseflow'
    }
  }
  d$season_component[is.na(d$Q)] <- NA_character_
  d
}

hydro_seasons <- hydro %>% group_by(site,water_year) %>%
  group_modify(~classify_year(.x)) %>% ungroup()

# Rainfall-associated events are independent of spring recession and high-flow flags.
detect_events <- function(d,cfg=settings) {
  d <- arrange(d,date); n <- nrow(d)
  d$event_id <- NA_integer_; d$event_start <- FALSE
  i <- 2L; id <- 0L
  while(i<=n) {
    prev <- d$Q[i-1L]; now <- d$Q[i]
    if(is.na(prev)||is.na(now)) {i<-i+1L;next}
    rise <- now-prev
    ratio_ok <- if(prev==0) rise>0 else now/prev>=cfg$rise_ratio
    rain_ok <- !is.na(d$ppt_3day[i]) && d$ppt_3day[i]>=cfg$ppt_3day_min_mm
    if(!(rise>=cfg$min_rise_cms && ratio_ok && rain_ok)) {i<-i+1L;next}
    id<-id+1L; start<-i; j<-i
    repeat {
      if(j>=n || j-start+1L>=cfg$event_max_days || is.na(d$Q[j+1L])) break
      peak<-max(d$Q[start:j],na.rm=TRUE)
      recovery<-prev+cfg$event_recovery_fraction*(peak-prev)
      if(j>start && d$Q[j]<=recovery) break
      j<-j+1L
    }
    d$event_id[start:j]<-id;d$event_start[start]<-TRUE;i<-j+1L
  }
  d
}

hydro_events <- hydro_seasons %>% group_by(site,water_year) %>%
  group_modify(~detect_events(.x)) %>%
  arrange(date,.by_group=TRUE) %>% mutate(Q_prev=lag(Q)) %>% ungroup()

thresholds <- hydro_events %>% group_by(site) %>% summarise(
  Q95=if(all(is.na(Q))) NA_real_ else unname(quantile(Q,settings$high_flow_quantile,na.rm=TRUE)),
  .groups='drop')
event_metrics <- hydro_events %>% filter(!is.na(event_id)) %>%
  group_by(site,water_year,event_id) %>% arrange(date,.by_group=TRUE) %>%
  summarise(start_date=first(date),end_date=last(date),duration_days=n(),
            peak_Q=max(Q,na.rm=TRUE),antecedent_Q=first(Q_prev),
            antecedent_component=first(season_component),
            start_season=first(met_season),rainfall_mm=sum(precip_mm,na.rm=TRUE),
            .groups='drop') %>% left_join(thresholds,by='site') %>%
  mutate(absolute_rise_cms=peak_Q-antecedent_Q,
         flow_initiation=!is.na(antecedent_Q)&antecedent_Q==0&peak_Q>0,
         is_high_flow=!is.na(Q95)&peak_Q>=Q95,
         is_dry_season_runoff=antecedent_component=='dry_season_baseflow' &
           start_season %in% c('summer','fall'),
         is_fall_candidate=is_dry_season_runoff & start_season=='fall') %>%
  group_by(site,water_year) %>% arrange(start_date,.by_group=TRUE) %>%
  mutate(is_fall_pulse=is_fall_candidate & cumsum(is_fall_candidate)==1L,
         event_class=case_when(is_fall_pulse~'fall_pulse_flow',
                               is_high_flow~'peak_flow',
                               is_dry_season_runoff~'dry_season_runoff',
                               TRUE~'other_runoff')) %>% ungroup()

hydro_labeled <- hydro_events %>% left_join(
  event_metrics %>% select(site,water_year,event_id,event_class,
                            is_high_flow,is_dry_season_runoff,is_fall_pulse),
  by=c('site','water_year','event_id')) %>% left_join(thresholds,by='site') %>%
  mutate(high_flow_day=!is.na(Q)&!is.na(Q95)&Q>=Q95,
         functional_component=case_when(!is.na(event_class)~event_class,
                                         high_flow_day~'peak_flow',
                                         TRUE~season_component))

seasonal_metrics <- hydro_labeled %>% group_by(site,water_year) %>%
  arrange(date,.by_group=TRUE) %>%
  mutate(run_id=cumsum(coalesce(season_component!=lag(season_component),TRUE) |
                         is.na(season_component)|is.na(lag(season_component)))) %>%
  ungroup() %>% filter(!is.na(season_component),!is.na(Q)) %>%
  group_by(site,water_year,season_component,run_id) %>%
  summarise(start_date=min(date),end_date=max(date),n_days=n(),
            start_Q=first(Q),end_Q=last(Q),Q10=unname(quantile(Q,.10)),
            Q50=median(Q),Q90=unname(quantile(Q,.90)),
            cv_Q=if(n()>1&&mean(Q)>0) sd(Q)/mean(Q) else NA_real_,
            flashiness=if(sum(Q)>0) sum(abs(diff(Q)))/sum(Q) else NA_real_,
            recession_pct_per_day=if(first(Q)>0&&last(Q)>0&&n()>1)
              100*(1-(last(Q)/first(Q))^(1/(n()-1))) else NA_real_,
            .groups='drop')
peak_metrics <- hydro_labeled %>% group_by(site,water_year) %>%
  arrange(date,.by_group=TRUE) %>%
  mutate(peak_run_start=high_flow_day & !lag(high_flow_day,default=FALSE)) %>%
  summarise(peak_days=sum(high_flow_day,na.rm=TRUE),
            peak_frequency=sum(peak_run_start,na.rm=TRUE),
            max_Q=if(all(is.na(Q))) NA_real_ else max(Q,na.rm=TRUE),
            peak_water_year_day=if(all(is.na(Q))) NA_integer_ else water_year_day[which.max(Q)],
            peak_date=if(all(is.na(Q))) as.Date(NA) else date[which.max(Q)],
            .groups='drop')

recession_diagnostics <- hydro_seasons %>% group_by(site,water_year) %>%
  summarise(observed_days=sum(!is.na(Q)),
            recession_start=first(recession_start),
            recession_end=first(recession_end),
            low_flow_onset=first(low_flow_onset),
            .groups='drop')
print(recession_diagnostics)
print(hydro_labeled %>% count(site,water_year,functional_component))

p <- ggplot(hydro_labeled%>%filter(site=="winters_usgs"),aes(water_year_day,Q)) +
  geom_line(color='grey70',linewidth=.35,na.rm=TRUE) +
  geom_point(aes(color=functional_component),size=.6,na.rm=TRUE) +
  facet_grid(water_year~., scales='free_y') +
  labs(x='Day of water year (Oct 1 = 1)',
       y=expression(Discharge~(m^3/s)),color='Functional flow') + theme_bw()
print(p)


p <- ggplot(hydro_labeled%>%filter(site=="davis"),aes(water_year_day,Q)) +
  geom_line(color='grey70',linewidth=.35,na.rm=TRUE) +
  geom_point(aes(color=functional_component),size=.6,na.rm=TRUE) +
  facet_grid(water_year~., scales='free_y') +
  labs(x='Day of water year (Oct 1 = 1)',
       y=expression(Discharge~(m^3/s)),color='Functional flow') + theme_bw()
print(p)


p <- ggplot(hydro_labeled%>%filter(site=="ophir"),aes(water_year_day,Q)) +
  geom_line(color='grey70',linewidth=.35,na.rm=TRUE) +
  geom_point(aes(color=functional_component),size=.6,na.rm=TRUE) +
  facet_grid(water_year~., scales='free_y') +
  labs(x='Day of water year (Oct 1 = 1)',
       y=expression(Discharge~(m^3/s)),color='Functional flow') + theme_bw()
print(p)

p <- ggplot(hydro_labeled%>%filter(site=="browns"),aes(water_year_day,Q)) +
  geom_line(color='grey70',linewidth=.35,na.rm=TRUE) +
  geom_point(aes(color=functional_component),size=.6,na.rm=TRUE) +
  facet_grid(water_year~., scales='free_y') +
  labs(x='Day of water year (Oct 1 = 1)',
       y=expression(Discharge~(m^3/s)),color='Functional flow') + theme_bw()
print(p)
# write.csv(hydro_labeled,'/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Processes_data/functional_flow/functional_flow_daily_WY2024_2026.csv',row.names=FALSE)
# write.csv(event_metrics,'/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Processes_data/functional_flow/functional_flow_events_WY2024_2026.csv',row.names=FALSE)
# write.csv(seasonal_metrics,'/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Processes_data/functional_flow/functional_flow_seasons_WY2024_2026.csv',row.names=FALSE)
# write.csv(peak_metrics,'/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Processes_data/functional_flow/functional_flow_peaks_WY2024_2026.csv',row.names=FALSE)



## simplify data export
hydro_clim <- hydro_labeled %>%
  select(site, water_year, date, Q, precip_mm, ppt_3day, season_component, functional_component)

# write.csv(hydro_clim,'/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Processes_data/hydro_clim_WY2024_2026.csv',row.names=FALSE)



## Bring in cross section measures 


cross_flow <- read_excel(
  "/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Raw_data/Davis_fire_cross_flow.xlsx"
) %>%
  mutate(
    # Original timestamp
    datetime_original = ymd_hms(datetime_sample, tz = "UTC"),
    date = as.Date(date_sample))


