#### Possible nutrient data infill from EGRET package

# https://github.com/DOI-USGS/EGRET/blob/main/R/readUserDaily.r

# https://cran.r-project.org/web/packages/EGRET/vignettes/EGRET.html


library(EGRET)


library(tidyverse)
library(lubridate)
library(plotly)
library(devtools)
# remotes::install_github("nrlottig/nrlmetab")
library(zoo)
library(suncalc)
library(readxl)
library(patchwork)
library(gridExtra)
library(dplyr)
library(reshape)
library(lubridate)
library(ggplot2)
library(scales)

site_colors <- c(
  "Ophir" = "#7d5ba6",
  "Winters" = "#4287f5",
  "Winters upstream" = "#7fc5f0",
  "Davis" = "#d9cb7c",
  "Browns" = "#c94d00", 
  "Browns sub"  = "#bd772d"
)

############################
## KAL sample data:
############################


library(readxl)
davis_data <- read_excel("C:/Users/kloria/Documents/Davis_data_exploration/davis_data_pre_analysis.xlsx")
#View(davis_data)
str(davis_data)

davis_data$datetime_T <-(as.POSIXct(round_date(
  as.POSIXct(davis_data$datetime, format="%Y-%m-%dT%H:%M:%SOZ"), unit="5 minutes")))

davis_data1 <- davis_data %>%
  mutate(date= as.Date(datetime_T))%>%
  mutate(year = year(date))%>%
  mutate(jday = yday(date))%>%
  mutate(mon = month(date))

### Water quality ##:

davis_data1 %>% 
  ggplot(aes(x = date, y = temp, color = site))+
  #geom_abline(slope = 0, intercept = 0, width = 1, alpha = 0.6)+
  geom_line(alpha = 0.8)+
  geom_point(aes(x = date, y = temp),
             alpha = 0.5, size = 2)+
  scale_color_manual(values = site_colors) + theme_minimal()
 # facet_wrap(~site, scales="free", nrow = 4)+


davis_data1 %>% 
  ggplot(aes(x = date, y = turb, color = site))+
  #geom_abline(slope = 0, intercept = 0, width = 1, alpha = 0.6)+
  geom_line(alpha = 0.8)+
  geom_point(alpha = 0.5, size = 2)+
  scale_color_manual(values = site_colors) + theme_minimal()

davis_data1 %>% 
  ggplot(aes(x = date, y = EC, color = site))+
  #geom_abline(slope = 0, intercept = 0, width = 1, alpha = 0.6)+
  geom_line(alpha = 0.8)+
  geom_point(alpha = 0.5, size = 2)+
  scale_color_manual(values = site_colors) + theme_minimal()


## metals ##

davis_data1 %>% 
  ggplot(aes(x = date, y = as.numeric(BC_DL_approx), color = site))+
  #geom_abline(slope = 0, intercept = 0, width = 1, alpha = 0.6)+
  geom_line(alpha = 0.8)+
  geom_point(alpha = 0.5, size = 2)+
  scale_color_manual(values = site_colors) + theme_minimal()






unique(davis_data$site)

bg_nuts <- read_excel("C/Users/kloria/Documents/Davis_data_exploration/davis_data_pre_analysis.xlsx") %>%
  filter(location=="stream") %>%
  mutate(NO3_mgL_dl = if_else(NO3_mgL_dl < 0.0015, 0.0015, NO3_mgL_dl))%>%
  mutate(NH4_mgL_dl=if_else(NH4_mgL_dl < 0.001, 0.001, NH4_mgL_dl))%>%
  mutate(PO4_ugL_dl=if_else(PO4_ugL_dl < 0.201, 0.201, PO4_ugL_dl)) %>%
  mutate(PO4_ugL_dl=if_else(DOC_mgL_dl < 0.25, 0.125, DOC_mgL_dl)) %>%
  group_by(site, shore, date, depth, location, substrate) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = "drop")
##### pivot wider 
bg_nuts_wide <- bg_nuts %>%
  dplyr::select(site, date, substrate, NO3_mgL_dl, NH4_mgL_dl, PO4_ugL_dl, DOC_mgL_dl, pH_infill) %>%
  group_by(site, date,substrate) %>% 
  summarise(across(c(NO3_mgL_dl, NH4_mgL_dl, PO4_ugL_dl, DOC_mgL_dl, pH_infill), mean, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = substrate, values_from = c(NO3_mgL_dl, NH4_mgL_dl, PO4_ugL_dl, DOC_mgL_dl, pH_infill), names_sep = "_")

BWL_NO3 <- bg_nuts_wide %>%
  filter(site=="BWL") %>%
  dplyr::select(date, NO3_mgL_dl_sw)
