#### Possible nutrient data infill from EGRET package

# https://github.com/DOI-USGS/EGRET/blob/main/R/readUserDaily.r

# https://cran.r-project.org/web/packages/EGRET/vignettes/EGRET.html


water_year <- function(data) {
  data %>%
    mutate(date = ymd(date)) %>%
    mutate(WaterYear = if_else(month(date) >= 10, year(date) + 1, year(date)))
}

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

site_lab_colors <- c(
  "ophir" = "#7d5ba6",
  "winters_usgs" = "#4287f5",
  "winters_up" = "#7fc5f0",
  "davis" = "#d9cb7c",
  "browns" = "#c94d00", 
  "browns_sub"  = "#bd772d"
)

############################
## KAL sample data:
############################

davis_data <- read.csv("/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Processes_data/ERDC_water_chem_dat_20251015_processed.csv") %>%
  mutate(
    datetime = as.POSIXct(
      round_date(as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles"), 
                 unit = "5 minutes")),
    date = as.Date(date, format = "%Y-%m-%d")
  )

str(davis_data)


### Water quality ##:
davis_data %>% 
  ggplot(aes(x = date, y = temp, color = site_lab))+
  #geom_abline(slope = 0, intercept = 0, width = 1, alpha = 0.6)+
  geom_line(alpha = 0.8)+
  geom_point(aes(x = date, y = temp),
             alpha = 0.5, size = 2)+
  scale_color_manual(values = site_lab_colors) + theme_minimal()
 # facet_wrap(~site, scales="free", nrow = 4)+


davis_data %>% 
  ggplot(aes(x = date, y = turb, color = site_lab))+
  #geom_abline(slope = 0, intercept = 0, width = 1, alpha = 0.6)+
  geom_line(alpha = 0.8)+
  geom_point(aes(x = date, y = turb),
             alpha = 0.5, size = 2)+
  scale_color_manual(values = site_lab_colors) + theme_minimal()
# facet_wrap(~site, scales="free", nrow = 4)+


davis_data %>% 
  filter(!(site_lab=="davis"))%>%
  ggplot(aes(x = date, y = log(BC_QC), color = site_lab))+
  #geom_abline(slope = 0, intercept = 0, width = 1, alpha = 0.6)+
  geom_line(alpha = 0.8)+
  geom_point(aes(x = date, y = log(BC_QC)),
             alpha = 0.5, size = 2)+
  scale_color_manual(values = site_lab_colors) + theme_minimal()
# facet_wrap(~site, scales="free", nrow = 4)+


### WRTD ###
## Start with just winters and ophir 

siteNo_Winters <- "10348570"
siteNo_Ophir <- "10348520"


INFO_winters <- readNWISInfo(siteNo_Winters,"00060")
INFO_winters$shortName <- "winters"






############################
## MY sites + my chem 

############################
# Gather discharge data:
siteNumber <- 10348570 
startDate <- "2023-09-30" #Gets earliest date
endDate <- "2025-10-30"
# Gather sample data:
parameter_cd<-"00631" # nitrate + nitrite as nitrogen is 00631, and for nitrate as nitrogen alone, it is 00618.
Sample <- readNWISSample(siteNumber,parameter_cd,startDate,endDate)
Sample$source <- "nwis"


Winters_USGS_df <- davis_data%>%
  filter(site_lab=="winters_usgs")


Daily <- readNWISDaily(siteNumber,"00060",startDate,endDate)
# Gather site and parameter information:

Daily$Q <- (Daily$Q*0.69)
Daily$LogQ <- log(Daily$Q)

###

### BWU NH4

###########################
###########################
# Gather discharge data:
siteNumber <- "10348570" #Choptank River near Greensboro, MD
startDate <- "2023-09-30" #Gets earliest date
endDate <- "2025-09-30"
# Gather sample data:
parameter_cd<-"00608" 
Sample <- readNWISSample(siteNumber,parameter_cd,startDate,endDate)
Sample$source <- "nwis"

WNT_TDN <- davis_data %>%
  filter(site_lab=="winters_usgs") %>%
  select(date, TDN_QC) %>%
  mutate(source = "KAL")

# Ensure date columns are in Date format
Sample$Date <- as.Date(Sample$dateTime)
WNT_TDN$Date <- (WNT_TDN$date)
WNT_TDN$ConcAve <- (WNT_TDN$TDN_QC)
WNT_TDN$ConcLow <- (WNT_TDN$TDN_QC)
WNT_TDN$ConcHigh <- (WNT_TDN$TDN_QC)


# Perform the full join by matching the date columns
Sample <- full_join(Sample, WNT_TDN, by = c("Date", "source", 
                                            "ConcAve", "ConcLow", "ConcHigh"))


Sample <-WNT_TDN  %>%
  filter(!is.na(Date)) %>%
  filter(Date < as.Date("2025-10-01"))

Sample$Julian <- as.numeric(Sample$Date - as.Date("1960-01-01"))
Sample$Day <- lubridate::yday(Sample$Date)
Sample$DecYear <- lubridate::year(Sample$Date) + (lubridate::yday(Sample$Date) - 1) / 365.25
Sample$SinDY <- sin(2 * pi * Sample$Julian / 365.25)
Sample$CosDY <- cos(2 * pi * Sample$Julian / 365.25)
Sample$Month <- month(Sample$Date)
Sample$dateTime <- month(Sample$Date)
Sample$CharacteristicName <- "Ammonia and ammonium"
Sample$USGSPCode <- 00608
Sample$ActivityMediaName <- "Water"
Sample$ActivityStartDateTime <- as.POSIXct(paste(Sample$Date, "13:00:00"), tz = "America/Los_Angeles")
Sample$ResultSampleFractionText <- "Filtered field and/or lab"
Sample$ResultStatusIdentifier <- "Accepted"
Sample$ResultValueTypeName <- "Actual"
Sample$date <- as.Date(Sample$Date)
Sample <- water_year(Sample)
Sample$waterYear <- c(Sample$water_year)
Sample$MonthSeq <- (year(Sample$Date) - 1800) * 12 + month(Sample$Date)
Sample$Uncen <- 1
Sample$ActivityMediaSubdivisionName <- NA

# Create the plot
logQ_plt3 <- ggplot(Sample, aes(x = Date, colour = source, shape=source)) +
  # Sample data as black points
  geom_point(aes(y = ConcAve), alpha = 0.9) + 
  # Adding titles and labels
  labs(
    title = "Comparison of Modeled vs. Sampled Nitrogen Concentrations",
    x = "Date",
    y = "TIN Nitrogen Concentration (mg/L)"
  ) +
  # scale_color_manual(values = c("Sampled TIN" = "black", "Modeled TIN" = "#D62828")) +  # Custom colors for the legend
  scale_x_date(date_breaks = "4 months", date_labels = "%b %Y") +  # Set breaks every 4 months
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate date labels for better visibility

# Print the plot
print(logQ_plt3)

Sample <- Sample %>%
  filter(Date< as.Date("2025-10-01")) %>%
  dplyr::select("Date", "ConcLow", "ConcHigh", "Uncen", "ConcAve",
                "Julian", "Month", "Day", "DecYear", "waterYear",
                "MonthSeq", "SinDY", "CosDY", "dateTime", "CharacteristicName",
                "USGSPCode", "ActivityStartDateTime", "ActivityMediaSubdivisionName",
                "ActivityMediaName", "ResultSampleFractionText", "ResultStatusIdentifier",
                "ResultValueTypeName") %>%
  group_by(Date, CharacteristicName, ActivityStartDateTime, 
           ActivityMediaSubdivisionName, ActivityMediaName, ResultSampleFractionText,
           ResultStatusIdentifier,ResultValueTypeName) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = "drop")

startDate <- min(as.character(Sample$Date)) 
# Gather discharge data:
Daily <- readNWISDaily(siteNumber,"00060",startDate,endDate)
# Gather site and parameter information:
Daily$Q <- c(Daily$Q*0.444)
Daily$LogQ <-c(log(Daily$Q))
Daily$Julian <- as.numeric(Daily$Date - as.Date("1960-01-01"))
Daily$Day <- lubridate::yday(Daily$Date)
Daily$DecYear <- lubridate::year(Daily$Date) + (lubridate::yday(Daily$Date) - 1) / 365.25
Daily$Month <- month(Daily$Date)
Daily$MonthSeq <- (year(Daily$Date) - 1800) * 12 + month(Daily$Date)

# Create the plot
logQ_plt3 <- ggplot(Sample, aes(x = Date, colour = source, shape=source)) +
  # Sample data as black points
  geom_point(aes(y = ConcAve), alpha = 0.9) + 
  # Adding titles and labels
  labs(
    title = "Comparison of Modeled vs. Sampled Nitrogen Concentrations",
    x = "Date",
    y = "TIN Nitrogen Concentration (mg/L)"
  ) +
  # scale_color_manual(values = c("Sampled TIN" = "black", "Modeled TIN" = "#D62828")) +  # Custom colors for the legend
  scale_x_date(date_breaks = "4 months", date_labels = "%b %Y") +  # Set breaks every 4 months
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate date labels for better visibility

# Print the plot
print(logQ_plt3)


startDate <- min(as.character(Sample$Date)) 
# Gather discharge data:
Daily <- readNWISDaily(siteNumber,"00060",startDate,endDate)
# Gather site and parameter information:

# Here user must input some values:
INFO<- readNWISInfo(siteNumber,parameter_cd)
INFO$shortName <- "WNT"

# Merge discharge with sample data:
# eList_BWUa <- mergeReport(INFO, Daily, Sample)

## look at dublicates
dup_dates <- eList_BWUa$Sample$Date[duplicated(eList_BWUa$Sample$Date)]
eList_BWUa$Sample[eList_BWUa$Sample$Date %in% dup_dates, ]

Sample_clean <- eList_BWUa$Sample[!duplicated(eList_BWUa$Sample$Date), ]

library(dplyr)

# Sample_clean <- Sample %>%
#   group_by(Date) %>%
#   summarise(
#     ConcLow = mean(ConcLow, na.rm = TRUE),
#     ConcAve = mean(ConcAve, na.rm = TRUE),
#     ConcHigh = mean(ConcHigh, na.rm = TRUE),
#     Uncen = ifelse(all(Uncen == 1), 1, 0),
#     .groups = "drop"
#   )

eList_BWUa <- mergeReport(eList_BWUa$INFO, eList_BWUa$Daily, Sample_clean)

############################

############################
# Check sample data:
boxConcMonth(eList_BWUa)
boxQTwice(eList_BWUa)
plotConcTime(eList_BWUa)
plotConcQ(eList_BWUa)
multiplot <- multiPlotDataOverview(eList_BWUa)

# ggsave("/Users/kellyloria/Documents/Publications/CH1\ biogeochem\ linkages/Sup_Figures/EGRET_BWL_TIN_plot_sample.png", plot = multiplot, width = 5.25, height = 6, units = "in")



###########################
# Run WRTDS model:
nrow(eList_BWUa$Sample)

eList_BWUa$INFO$minNumObs <- nrow(eList_BWUa$Sample)

# 
# range(eList_BWUa$Sample$Date)
# # Suppose: "2024-10-16" to "2025-05-15"
# 
# eList_BWUa$Daily <- subset(
#   eList_BWUa$Daily,
#   Date >= min(eList_BWUa$Sample$Date) & Date <= max(eList_BWUa$Sample$Date)
# )

## debug Error in `$<-.data.frame`(`*tmp*`, "FNConc", value = c(0.164343907414783,  : 
# replacement has 323 rows, data has 34
# 
# summary(eList_BWUa$Daily$Q)
# sum(is.na(eList_BWUa$Daily$Q))
# sum(eList_BWUa$Daily$Q <= 0)
# 
# 
# range(eList_BWUa$Daily$Date)
# range(eList_BWUa$Sample$Date)
# 
# test_mod <- try(modelEstimation(eList_BWUa, windowY = 2, windowQ = 1, windowS = 0.5,
#                                 minNumObs = 10, minNumUncen = 10, edgeAdjust = TRUE,
#                                 verbose = FALSE))
# 
# 
# # 1. Extract ranges from sample and daily data
# range(eList_BWUa$Sample$LogQ)
# range(eList_BWUa$Daily$LogQ)
# 
# # 2. Identify days outside the sample LogQ range
# out_of_range <- with(eList_BWUa$Daily, LogQ < min(eList_BWUa$Sample$LogQ) |
#                        LogQ > max(eList_BWUa$Sample$LogQ))
# 
# sum(out_of_range)            # Count of Daily rows out of range
# which(out_of_range)[1:20]    # First few indices
# eList_BWUa$Daily$Date[out_of_range][1:10]  # Dates of out-of-range days
# 
# 
# 
# logq_range <- range(eList_BWUa$Sample$LogQ, na.rm = TRUE)
# eList_BWUa$Daily <- subset(eList_BWUa$Daily,
#                            LogQ >= logq_range[1] & LogQ <= logq_range[2])


eList_mod_BWUa <- modelEstimation(eList_BWUa
                                  # windowY = 1,
                                  # windowQ = 1,
                                  # windowS = 0.1,
                                  # minNumObs = 10,
                                  # minNumUncen = 10,
                                  # edgeAdjust = TRUE,
                                  # verbose = TRUE
)


# Run modelEstimation with adjusted arguments
eList_mod_BWUa <- modelEstimation(
  eList_BWUa,
  windowY = 2,  # Adjust year window as needed
  windowQ = 1,  # Adjust flow window as needed
  windowS = 0.5,  # Adjust stage window as needed
  minNumObs = 10,  # Minimum number of observations
  minNumUncen = 10,  # Minimum uncensored observations
  edgeAdjust = TRUE,  # Edge adjustment flag
  verbose = TRUE,  # Verbosity flag
  run.parallel = FALSE  # Parallel processing flag
)

eList_BWUa$Daily
summary(eList_BWUa$Daily)

plotConcTime(eList_BWUa)



#Require Sample + INFO:
plotConcTimeDaily(eList_mod_BWUa) # Plot Modeled Concentration over Time
plotFluxTimeDaily(eList_mod_BWUa) # Plot Modeled Flux over Time

plotConcPred(eList_mod_BWUa)
plotFluxPred(eList_mod_BWUa)
plotResidPred(eList_mod_BWUa) # Plots model residuals (differences between observed and predicted values) against predicted concentrations.
plotResidQ(eList_mod_BWUa) # Plots residuals against discharge (Q) to assess whether model errors vary with flow.
plotConcHist(eList_mod_BWUa) # Flow-Normalized Concentration Trends

plotResidTime(eList_mod_BWUa) # Plots residuals over time to check for temporal trends in model errors.
boxResidMonth(eList_mod_BWUa)
boxConcThree(eList_mod_BWUa) # Plots boxplots of concentration grouped by flow conditions (low, mid, high).


#Require Daily + INFO:
plotConcHist(eList_mod_BWUa)
plotFluxHist(eList_mod_BWUa)

### for me 
# Merge the sample and modeled data
# Convert Sample Date to Date format (if not already)
Sample$Date <- as.Date(Sample$Date)

# Aggregate modeled data to match the sample data's Date range
eList_mod_BWUa[["Daily"]]$Date <- as.Date(eList_mod_BWUa[["Daily"]][["Date"]])  # Ensure the modeled data has a Date column
eList_mod_BWU_agga <- eList_mod_BWUa[["Daily"]] %>%
  group_by(Date) %>%
  summarize(ConcDay_mod = mean(ConcDay, na.rm = TRUE),
            FluxDay_mod = mean(FluxDay, na.rm = TRUE)
  )

# Now merge the aggregated model data with the sample data
combined_data_BWU <- merge(Sample, eList_mod_BWU_agga, by = "Date", all.x = TRUE)


# Create the plot
logQ_plt <- ggplot(combined_data_BWU, aes(x = Date)) +
  # Sample data as black points
  geom_point(aes(y = ConcAve, color = "Sampled N"), alpha = 0.9) + 
  geom_line(aes(y = ConcAve, color = "Sampled N")) +
  
  # Modeled data as red triangles
  geom_point(aes(y = ConcDay_mod, color = "Modeled N"), shape = 2, alpha = 0.9) + 
  geom_line(aes(y = ConcDay_mod, color = "Modeled N"), alpha = 0.9) + 
  
  # Adding titles and labels
  labs(
    title = "Comparison of Modeled vs. Sampled Nitrogen Concentrations",
    x = "Date",
    y = "N Nitrogen Concentration (mg/L)"
  ) +
  scale_color_manual(values = c("Sampled N" = "black", "Modeled N" = "#D62828")) +  # Custom colors for the legend
  scale_x_date(date_breaks = "4 months", date_labels = "%b %Y") +  # Set breaks every 4 months
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate date labels for better visibility

# Print the plot
print(logQ_plt)


# ggsave("/Users/kellyloria/Documents/Publications/CH1\ biogeochem\ linkages/Sup_Figures/EGRET_BWU_NH4_plot_sample_TS.png", plot = logQ_plt, width = 8, height = 4, units = "in")
# write.csv(combined_data_BWU, "/Users/kellyloria/Documents/Publications/CH1\ biogeochem\ linkages/EGRET_BWU_NH4.csv")
