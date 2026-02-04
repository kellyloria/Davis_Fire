## 2025-12-02 water chemistry QA'QC

# DATE of last clean : 2025-12-23

# I.  First cleaning pipe: 
#     - Line up with USGS gage 
#     - Add site_lab to get consistent site names across different sample days 

# II.  Second cleaning pipe: 
#     - Get detection limits for each chemical  
#     - _QC column to be 1/2 DL value when observation is less than the DL
#     - _lab column where "yes" is sample is below DL and "no" is sample of DL

# III.  Third thing:
#     - Bring in YSI data to fill in Lat/long and water quality observations for each date 

# save as "ERDC_water_chem_dat_"date last of last cleaning YYYY_MM_DD"_processed.csv"

## Load packages:
library(dplyr)
library(purrr)
library(readr)
library(lubridate)
library(readxl)
library(rlang)
library(tibble)
library(dataRetrieval)
library(stringr)
library(data.table)


# Chem data
davis_data <- read_excel("/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Raw_data/davis_data_water_chem.xlsx") 
#View(davis_data)
str(davis_data)

davis_data$datetime <-(as.POSIXct(round_date(
  as.POSIXct(davis_data$datetime, format="%Y-%m-%dT%H:%M:%SOZ"), unit="5 minutes")))

## Step 1: ##
# I.  First cleaning pipe: 
#     - Line up with USGS gage 
#     - Add site_lab to get consistent site names across different sample days 

davis_data <- davis_data %>%
  mutate(date= as.Date(datetime))%>%
  mutate(year = year(date))%>%
  mutate(jday = yday(date))%>%
  mutate(mon = month(date)) %>%
  # create dummy column for Creek to line up hydroclimate data
  mutate(USGS_gage = case_when(
    site== "Davis" ~ "davis", 
    site == "Ophir" ~ "ophir",
    site == "Winters" ~ "winters",
    site == "Winters USGS" ~ "winters",
    site == "Winters upstream" ~ "winters",
    site == "Browns" ~ "browns", 
    site == "Browns sub" ~ "browns",
    site == "Browns Sub" ~ "browns")) %>%
  mutate(site_lab = case_when(
    site== "Davis" ~ "davis", 
    site == "Ophir" ~ "ophir",
    site == "Winters" ~ "winters_usgs",
    site == "Winters USGS" ~ "winters_usgs",
    site == "Winters upstream" ~ "winters_up",
    site == "Browns" ~ "browns", 
    site == "Browns sub" ~ "browns_sub",
    site == "Browns Sub" ~ "browns_sub"))
names(davis_data)

summary(davis_data)

# I. grab relevant data columns 

davis_df <- davis_data%>%
  dplyr::select("site", "site_lab", "USGS_gage", "datetime", "year", "jday", "mon", "Lat", "Long", 
         "temp", "DO", "EC", "pH", 
         "ORP", "turb", "notes", "BC", "BC run DL", "TSS", "NO3-N", "NO3-NO2-N", 
         "NO2-N", "NH4-N", "TKN", "PO4-P", "PO4-ortho", "TDN-calc", "Alkalinity",
         "Cl", "SO4", "DOC", "TDN", "TOC", "Al", "As", "B", "Ba", "Ca", "Cr",
         "Fe", "Mn", "Pb", "Sr", "Zn")

davis_df <-as.data.frame(davis_df)
str(davis_df)


## Step 2: ##
# II. # Flexible helper function
#    - Need to swap out "BDL" in each column with a flag for when the value is "BDL"
#    - Add columns with actual BDL values
#    - Add number column for plotting and stats where when observations are "BDL" they transformed to "1/2 the detection limit"

names(davis_df)

# CORRECTED: Proper BDL handling with flag tracking
handle_BDL <- function(df, var, DL) {
  var_sym <- sym(var)
  
  if (!var %in% names(df)) {
    warning(paste("Variable", var, "not found in dataframe. Skipping."))
    return(df)
  }
  
  # Try to interpret DL as numeric if possible
  if (!is.na(suppressWarnings(as.numeric(DL)))) {
    DL <- as.numeric(DL)
  }
  
  if (is.character(DL) && DL %in% names(df)) {
    # dynamic column
    dl_sym <- sym(DL)
    df %>%
      mutate(
        !!paste0(var, "_DL_lab") := if_else(!!var_sym == "BDL", "yes", "no"),
        !!var_sym := if_else(!!var_sym == "BDL", as.character(!!dl_sym), as.character(!!var_sym)),
        !!var_sym := as.numeric(!!var_sym),
        !!paste0(var, "_DL_lab") := case_when(
          !!sym(paste0(var, "_DL_lab")) == "yes" ~ "yes",
          !!var_sym <= !!dl_sym ~ "yes",
          TRUE ~ "no"
        ),
        !!paste0(var, "_QC") := case_when(
          !!sym(paste0(var, "_DL_lab")) == "yes" ~ !!dl_sym / 2,
          TRUE ~ !!var_sym
        )
      )
    
  } else if (is.numeric(DL)) {
    # constant numeric DL
    df %>%
      mutate(
        !!paste0(var, "_DL_lab") := if_else(!!var_sym == "BDL", "yes", "no"),
        !!var_sym := if_else(!!var_sym == "BDL", as.character(DL), as.character(!!var_sym)),
        !!var_sym := as.numeric(!!var_sym),
        !!paste0(var, "_DL_lab") := case_when(
          !!sym(paste0(var, "_DL_lab")) == "yes" ~ "yes",
          !!var_sym <= DL ~ "yes",
          TRUE ~ "no"
        ),
        !!paste0(var, "_QC") := case_when(
          !!sym(paste0(var, "_DL_lab")) == "yes" ~ DL / 2,
          TRUE ~ !!var_sym
        )
      )
    
  } else {
    warning(paste("Invalid DL type for", var, "- must be numeric or existing column name."))
    df
  }
}



# Define detection limits without list() wrapper
# Only use character string for dynamic DL (column reference)
dl_table <- tribble(
  ~var,          ~DL,
  "BC",          list("BC run DL"),    # dynamic DL (column name) - NO list()
  "NO3-N",       list(0.02),            # numeric DL - NO list()
  "NO3-NO2-N",   list(0.02),
  "NO2-N",       list(0.01),
  "NH4-N",       list(0.1),
  "TKN",         list(0.2),
  "TDN-calc",    list(0.1),
  "PO4-P",       list(0.03),
  "PO4-ortho",   list(0.01),
  "SO4",         list(0.9),
  "TDN",         list(0.05),
  "DOC",         list(0.05),
  "TOC",         list(0.05),
  "Cl",          list(0.05),
  "Al",          list(1),
  "As",          list(0.1),
  "B",           list(1),
  "Ba",          list(1),
  "Ca",          list(10),
  "Cr",          list(0.1),
  "Fe",          list(1),
  "Mn",          list(0.1),
  "Pb",          list(0.1),
  "Sr",          list(0.1),
  "Zn",          list(1),
  "Alkalinity",  list(2)
)

# Apply detection limit handling - NO unlist() needed
davis_df2 <- reduce(
  seq_len(nrow(dl_table)),
  ~ handle_BDL(.x, dl_table$var[.y], dl_table$DL[[.y]]),
  .init = davis_df
)

## Just for BC
davis_df2 <- davis_df2 %>%
  mutate(
    # Convert BC to numeric safely (handles scientific notation as character)
    BC_num = suppressWarnings(as.numeric(BC)),
    
    # Create DL flag: "yes" if BC value >= DL, "no" otherwise
    BC_DL_lab = case_when(
      is.na(BC_num) ~ NA_character_,
      BC_num <= `BC run DL` ~ "yes",
      TRUE ~ "no"
    ),
    
    # Create QC column: replace values below DL with half DL
    BC_QC = case_when(
      is.na(BC_num) ~ NA_real_,
      BC_num < `BC run DL` ~ `BC run DL` / 2,
      TRUE ~ BC_num
    )
  ) %>%
  dplyr::select(-BC_num)  # remove helper column if desired


str(davis_df2)


# Verification step - check that DLs were applied correctly
cat("Verification of Detection Limit Application:\n")
cat("===========================================\n\n")

# Check a few variables
for (i in 1:min(68, nrow(dl_table))) {
  var_name <- dl_table$var[i]
  dl_val <- dl_table$DL[i]
  
  if (paste0(var_name, "_DL_lab") %in% names(davis_df2)) {
    n_bdl <- sum(davis_df2[[paste0(var_name, "_DL_lab")]] == "yes", na.rm = TRUE)
    cat(sprintf("%s: DL = %s, # below DL = %d\n", 
                var_name, 
                ifelse(is.character(dl_val), paste0("column '", dl_val, "'"), dl_val),
                n_bdl))
  }
}


### collect QAQC columns 
davis_df <- davis_df2%>%
  dplyr::select("site", "site_lab", "USGS_gage", "datetime", "year", "jday", "mon", "Lat", "Long", 
                "temp", "DO", "EC", "pH", 
                "ORP", "turb", "notes", "TSS", "NO3-N_DL_lab","NO3-N_QC",
                "NO3-NO2-N_DL_lab", "NO3-NO2-N_QC", "NO2-N_DL_lab", "NO2-N_QC",
                "NH4-N_DL_lab", "NH4-N_QC", "TKN_DL_lab", "TKN_QC", 
                "TDN-calc_DL_lab", "TDN-calc_QC", "PO4-P_DL_lab", "PO4-P_QC",
                "PO4-ortho_DL_lab", "PO4-ortho_QC", "SO4_DL_lab", "SO4_QC",
                "TDN_DL_lab", "TDN_QC", "DOC_DL_lab", "DOC_QC", "TOC_DL_lab", 
                "TOC_QC", "Cl_DL_lab", "Cl_QC", "Al_DL_lab", "Al_QC", 
                "As_DL_lab", "As_QC", "B_DL_lab", "B_QC", "Ba_DL_lab", "Ba_QC",
                "Ca_DL_lab", "Ca_QC", "Cr_DL_lab", "Cr_QC", "Fe_DL_lab", "Fe_QC",
                "Mn_DL_lab", "Mn_QC", "Pb_DL_lab", "Pb_QC", "Sr_DL_lab","Sr_QC",
                "Zn_DL_lab", "Zn_QC", "Alkalinity_DL_lab", "Alkalinity_QC", 
                "BC_DL_lab","BC_QC")


## Step 3: ##
### use YSI data to fill in missing GPS
ysi_dat <- read_csv("/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Processes_data/ysi_WQ_QAQC_daily.csv")

ysi_dat <- ysi_dat %>%
   dplyr::select(site_lab, date, ends_with("_mean")) %>%
  rename_with(
    ~ gsub("_mean$", "", .x),  # remove "_mean" at end of column names
    ends_with("_mean")         # apply only to columns ending with "_mean"
  )


ysi_dat <- as.data.frame(ysi_dat) %>%
  rename(
    Lat   = lat.,
    Long  = long.,
    temp  = TEMP_C,
    DO    = DO_sat,
    EC    = SPC_dsmc,
    ORP   = ORP_MV,
    turb  = turbidity_FNU
  )

  
# 1. Prepare date columns for matching
# (extract date part from datetime in davis_df)
davis_df <- davis_df %>%
  mutate(date = as.Date(datetime))

# 2. Select relevant columns from ysi_dat to merge
ysi_cols <- c("site_lab", "date", "Lat", "Long",
              "temp", "DO", "EC", "ORP", "turb", "pH")

ysi_dat_sub <- ysi_dat %>%
   dplyr::select(all_of(ysi_cols))

# 3. Merge datasets by common keys

merged_df <- davis_df %>%
  left_join(ysi_dat_sub, by = c("site_lab", "date"), suffix = c("", ".ysi")) %>%
  mutate(
    Lat  = coalesce(Lat,  Lat.ysi),
    Long = coalesce(Long, Long.ysi),
    temp = coalesce(temp, temp.ysi),
    DO   = coalesce(DO,   DO.ysi),
    EC   = coalesce(EC,   EC.ysi),
    ORP  = coalesce(ORP,  ORP.ysi),
    pH   = coalesce(pH,   pH.ysi),
    turb = coalesce(turb, turb.ysi)
  ) %>%
   dplyr::select(-ends_with(".ysi"))  # drop extra ysi columns after merge


# 5. Clean up redundant columns (those from ysi_dat)

merged_df <- merged_df%>%
  dplyr::select("site", "site_lab", "USGS_gage", "date","datetime", 
                "Lat", "Long", 
                "temp", "DO", "EC", "pH", 
                "ORP", "turb", "notes", "TSS", "NO3-N_DL_lab","NO3-N_QC",
                "NO3-NO2-N_DL_lab", "NO3-NO2-N_QC", "NO2-N_DL_lab", "NO2-N_QC",
                "NH4-N_DL_lab", "NH4-N_QC", "TKN_DL_lab", "TKN_QC", 
                "TDN-calc_DL_lab", "TDN-calc_QC", "PO4-P_DL_lab", "PO4-P_QC",
                "PO4-ortho_DL_lab", "PO4-ortho_QC", "SO4_DL_lab", "SO4_QC",
                "TDN_DL_lab", "TDN_QC", "DOC_DL_lab", "DOC_QC", "TOC_DL_lab", 
                "TOC_QC", "Cl_DL_lab", "Cl_QC", "Al_DL_lab", "Al_QC", 
                "As_DL_lab", "As_QC", "B_DL_lab", "B_QC", "Ba_DL_lab", "Ba_QC",
                "Ca_DL_lab", "Ca_QC", "Cr_DL_lab", "Cr_QC", "Fe_DL_lab", "Fe_QC",
                "Mn_DL_lab", "Mn_QC", "Pb_DL_lab", "Pb_QC", "Sr_DL_lab","Sr_QC",
                "Zn_DL_lab", "Zn_QC", "Alkalinity_DL_lab", "Alkalinity_QC", 
                "BC_DL_lab","BC_QC")
summary(merged_df)


dl_table <- tribble(
  ~var,          ~DL,
  "BC",          0.0431,
  "NO3-N",       0.02,
  "NO3-NO2-N",   0.02,
  "NO2-N",       0.01,
  "NH4-N",       0.1,
  "TKN",         0.2,
  "TDN-calc",    0.1,
  "PO4-P",       0.03,
  "PO4-ortho",   0.01,
  "SO4",         0.9,
  "TDN",         0.03,
  "DOC",         0.1,
  "TOC",         0.1,
  "Cl",          0.1,
  "Al",          1,
  "As",          0.1,
  "B",           1,
  "Ba",          1,
  "Ca",          10,
  "Cr",          0.1,
  "Fe",          1,
  "Mn",          0.1,
  "Pb",          0.1,
  "Sr",          0.1,
  "Zn",          1,
  "Alkalinity",  2,
  "temp",        0.01,
  "DO",         0.001,
  "turb",       0.01,
  "pH",        0.01,
  "ORP",        0.01,
  "turb",         0.1
  
)


get_decimals <- function(x) {
  if (is.na(x) || x == 0) return(0)
  dec <- abs(floor(log10(x)))  # digits after decimal
  return(dec + 1)  # keep one extra for reporting
}

dl_table <- dl_table %>%
  mutate(round_digits = map_dbl(DL, get_decimals))

# --- Step 1: identify all *_QC columns ---
qc_cols <- names(merged_df)[str_detect(names(merged_df), "_QC$")]

# --- Step 2: for each *_QC column, match chemical name to DL table ---
# Remove suffixes to match against `var` in dl_table
qc_clean <- str_remove(qc_cols, "_QC$")

# --- Step 3: round each numeric column according to DL ---
for (i in seq_along(qc_cols)) {
  chem <- qc_clean[i]
  round_to <- dl_table$round_digits[match(chem, dl_table$var)]
  
  if (!is.na(round_to)) {
    message("Rounding ", qc_cols[i], " to ", round_to, " decimals")
    merged_df[[qc_cols[i]]] <- round(merged_df[[qc_cols[i]]], round_to)
  }
}

# 2. Select relevant columns from ysi_dat to merge
ysi_round <- c("temp", "DO", "EC", "ORP", "turb", "pH")

WQ_cols <- names(merged_df)[8:12]


for (i in seq_along(WQ_cols)) {
  chem <- ysi_round[i]
  round_to <- dl_table$round_digits[match(chem, dl_table$var)]
  
  if (!is.na(round_to)) {
    message("Rounding ", WQ_cols[i], " to ", round_to, " decimals")
    merged_df[[WQ_cols[i]]] <- round(merged_df[[WQ_cols[i]]], round_to)
  }
}



## Step 4: ##
### transform flow 

flow_m_dat <- read.csv("/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Raw_data/ERDC_measured_flow.csv") %>%
  mutate(
    # convert date
    date = as.Date(date, format = "%m/%d/%y"),
    # combine date + time into a single string
    datetime_char = paste(date, time),
    # convert to POSIXct with Reno, NV time zone
    datetime = as.POSIXct(datetime_char, format = "%Y-%m-%d %H:%M", tz = "America/Los_Angeles")
  )

siteNo_winters <- "10348570"
siteNo_ophir <- "10348520"
siteNo_davis <- "10348550"

# Define the parameter codes for flow and stage
pCode_flow <- "00060"
pCode_stage <- "00065"

pcodes <- c(pCode_flow, pCode_stage)

# Set the start date to "1980-01-01" and end date to today (current date)
start.date <- "2024-09-01"
end.date <- Sys.Date()  # Use the current date as the end date

flow_data_WI <- readNWISuv(siteNumbers = siteNo_winters, parameterCd = pcodes, startDate = start.date, endDate = end.date) %>%
  mutate(site = "readNWISuv", catch = "burned") %>%
  dplyr::rename(datetime = dateTime, dischargeCFS = X_00060_00000) %>%
  mutate(
    dischargeCMS_WI = dischargeCFS * 0.0283168) %>%
  dplyr::select(site, datetime, dischargeCMS_WI)


flow_data_OP <- readNWISuv(siteNumbers = siteNo_ophir, parameterCd = pcodes, startDate = start.date, endDate = end.date) %>%
  mutate(site = "ophir", catch = "unburned") %>%
  dplyr::rename(datetime = dateTime, dischargeCFS = X_00060_00000) %>%
  mutate(
    dischargeCMS_OP = dischargeCFS * 0.0283168) %>%
  dplyr::select(site, datetime, dischargeCMS_OP)


flow_data_DA <- readNWISuv(siteNumbers = siteNo_davis, parameterCd = pcodes, startDate = start.date, endDate = end.date) %>%
  mutate(site = "davis", catch = "unburned") %>%
  dplyr::rename(datetime = dateTime, dischargeCFS = X_00060_00000) %>%
  mutate(
    dischargeCMS_DA = dischargeCFS * 0.0283168) %>%
  dplyr::select(site, datetime, dischargeCMS_DA)


library(data.table)

flow_data <- flow_m_dat%>%
  dplyr::filter(site =="browns_sub") %>%
  left_join(flow_data_WI, by=c("datetime")) %>%
  left_join(flow_data_OP, by=c("datetime"))

flow_data_WUSGS <-flow_data_WI%>%
  mutate(site = "winters_usgs",
         flow = dischargeCMS_WI) %>%
  dplyr::select(site, datetime, flow)

flow_data_O <-flow_data_OP%>%
  mutate(site = "ophir",
         flow = dischargeCMS_OP) %>%
  dplyr::select(site, datetime, flow)

flow_data_D <-flow_data_DA%>%
  mutate(site = "davis",
         flow = dischargeCMS_DA) %>%
  dplyr::select(site, datetime, flow)

####
#### Fit modeled flow for Browns Sub. 
####

library(mgcv)
library(ggplot2)

# 1. Explore your data
ggplot(flow_data, aes(x = dischargeCMS_WI, y = discharge_m3.s)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr"), 
              se = TRUE, color = "red") +
  labs(title = "Flow Relationship: Browns Sub vs Winters Creek",
       x = "Winters Creek Flow (m³/s)",
       y = "Browns Sub Flow (m³/s)") +
  theme_minimal()

# 2. Fit multiple models and compare
flow_data_clean <- flow_data %>%
  filter(!is.na(discharge_m3.s) & !is.na(dischargeCMS_WI) & 
           discharge_m3.s > 0 & dischargeCMS_WI > 0)

# Power law model
model_power <- lm(log(discharge_m3.s) ~ log(dischargeCMS_WI), 
                  data = flow_data_clean)

# GAM model
model_gam <- gam(discharge_m3.s ~ s(dischargeCMS_WI, bs = "cr", k = 4), 
                 data = flow_data_clean)

# Compare models
cat("Power Law R²:", summary(model_power)$r.squared, "\n")
cat("GAM R²:", summary(model_gam)$r.sq, "\n")

# 3. Use the best model for prediction
flow_data_BS <- flow_data_WI %>%
  mutate(
    site = "browns_sub",
    # GAM prediction with handling for edge cases
    flow = pmax(0, predict(model_gam, 
                           newdata = data.frame(dischargeCMS_WI = dischargeCMS_WI)))
  ) %>%
  dplyr::select(site, datetime, flow)



###
### Plot the modeled relationship 

library(ggplot2)
library(mgcv)
library(dplyr)
library(gridExtra)

# First, ensure your GAM model is fitted
flow_data_clean <- flow_data %>%
  filter(!is.na(discharge_m3.s) & !is.na(dischargeCMS_WI) & 
           discharge_m3.s > 0 & dischargeCMS_WI > 0)

model_gam <- gam(discharge_m3.s ~ s(dischargeCMS_WI, bs = "cr", k = 4), 
                 data = flow_data_clean)

# Generate predictions for the full range
flow_data_BS <- flow_data_WI %>%
  mutate(
    site = "browns_sub",
    flow = pmax(0, predict(model_gam, 
                           newdata = data.frame(dischargeCMS_WI = dischargeCMS_WI)))
  ) %>%
  dplyr::select(site, datetime, flow, dischargeCMS_WI = dischargeCMS_WI)

# Create a sequence for smooth prediction line
prediction_range <- data.frame(
  dischargeCMS_WI = seq(min(flow_data_WI$dischargeCMS_WI, na.rm = TRUE),
                        max(flow_data_WI$dischargeCMS_WI, na.rm = TRUE),
                        length.out = 200)
)
prediction_range$predicted_flow <- predict(model_gam, newdata = prediction_range)

# ---- PLOT 1: Observed vs Predicted (Rating Curve) ----
p1 <- ggplot() +
  # Add the smooth GAM prediction line
  geom_line(data = prediction_range, 
            aes(x = dischargeCMS_WI, y = predicted_flow),
            color = "red", size = 1.2, linetype = "solid") +
  # Add observed data points
  geom_point(data = flow_data_clean, 
             aes(x = dischargeCMS_WI, y = discharge_m3.s),
             size = 4, color = "black", shape = 21, fill = "lightblue", stroke = 1.5) +
  # Add 1:1 reference line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", 
              color = "gray40", size = 0.8) +
  labs(
    title = "Browns Sub vs Winters Creek USGS",
    subtitle = paste0("GAM Model (R² = ", round(summary(model_gam)$r.sq, 3), ")"),
    x = "Winters Creek USGS Flow (m³/s)",
    y = "Browns Sub Flow (m³/s)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70")
  )

# ---- PLOT 2: Time Series Comparison ----
# Merge observed and predicted data
time_series_compare <- flow_data_clean %>%
  filter(site.x  == "browns_sub") %>%
  select(datetime, observed = discharge_m3.s) %>%
  left_join(
    flow_data_BS %>% select(datetime, predicted = flow),
    by = "datetime"
  )

p2 <- ggplot(time_series_compare, aes(x = datetime)) +
  geom_line(aes(y = predicted, color = "GAM Predicted"), size = 1) +
  geom_point(aes(y = observed, color = "Observed"), size = 3) +
  scale_color_manual(
    name = "Data Type",
    values = c("GAM Predicted" = "red", "Observed" = "blue")
  ) +
  labs(
    title = "Observed vs GAM Predicted Flow",
    x = "Date",
    y = "Flow (m³/s)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# ---- PLOT 3: Residuals Plot ----
residuals_data <- flow_data_clean %>%
  mutate(
    predicted = predict(model_gam),
    residuals = discharge_m3.s - predicted
  )

p3 <- ggplot(residuals_data, aes(x = predicted, y = residuals)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_point(size = 3, color = "darkblue", alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "red", fill = "pink", alpha = 0.3) +
  labs(
    title = "Residuals Plot",
    x = "Predicted Flow (m³/s)",
    y = "Residuals (Observed - Predicted)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

# ---- PLOT 4: Observed vs Predicted 1:1 Plot ----
p4 <- ggplot(residuals_data, aes(x = predicted, y = discharge_m3.s)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  geom_point(size = 4, color = "darkgreen", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "pink", alpha = 0.3) +
  labs(
    title = "Observed vs Predicted (1:1 Plot)",
    x = "GAM Predicted Flow (m³/s)",
    y = "Observed Flow (m³/s)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

# ---- Display all plots ----
# Show them individually
print(p1)
print(p2)
print(p3)
print(p4)

# Or combine into a multi-panel figure
combined_plot <- grid.arrange(p1, p2, p3, p4, ncol = 2)

# Save the combined plot if desired
# ggsave("Browns_sub_flow_model_comparison.png", combined_plot, width = 8, height = 6, dpi = 300)

# ---- Print model diagnostics ----
cat("\n=== GAM Model Summary ===\n")
summary(model_gam)

cat("\n=== Model Performance Metrics ===\n")
cat("R-squared:", round(summary(model_gam)$r.sq, 4), "\n")
cat("RMSE:", round(sqrt(mean(residuals_data$residuals^2)), 4), "m³/s\n")
cat("MAE:", round(mean(abs(residuals_data$residuals)), 4), "m³/s\n")
cat("Bias:", round(mean(residuals_data$residuals), 4), "m³/s\n")



### combine GAMM modeled flow 
flow_data_BS <- flow_data_BS %>%
  dplyr::select(site, datetime, flow)

### 
### Browns 
###

flow_data_B <- flow_m_dat%>%
  dplyr::filter(site =="browns") %>%
  left_join(flow_data_WI, by=c("datetime")) %>%
  left_join(flow_data_OP, by=c("datetime"))


library(mgcv)
library(ggplot2)

# 1. Explore your data
ggplot(flow_data_B, aes(x = dischargeCMS_WI, y = discharge_m3.s)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr"), 
              se = TRUE, color = "red") +
  labs(title = "Flow Relationship: Browns vs Winters Creek",
       x = "Winters Creek Flow (m³/s)",
       y = "Browns Flow (m³/s)") +
  theme_minimal()

# 2. Fit multiple models and compare
flow_data_clean <- flow_data_B %>%
  filter(!is.na(discharge_m3.s) & !is.na(dischargeCMS_WI) & 
           discharge_m3.s > 0 & dischargeCMS_WI > 0)

# Power law model
model_power <- lm(log(discharge_m3.s) ~ log(dischargeCMS_WI), 
                  data = flow_data_clean)

# GAM model
model_gam <- gam(discharge_m3.s ~ s(dischargeCMS_WI, bs = "cr", k = 4), 
                 data = flow_data_clean)

# Compare models
cat("Power Law R²:", summary(model_power)$r.squared, "\n")
cat("GAM R²:", summary(model_gam)$r.sq, "\n")

# 3. Use the best model for prediction
flow_data_Bb <- flow_data_WI %>%
  mutate(
    site = "browns",
    # GAM prediction with handling for edge cases
    flow = pmax(0, predict(model_gam, 
                           newdata = data.frame(dischargeCMS_WI = dischargeCMS_WI)))
  ) %>%
  dplyr::select(site, datetime, flow)



###
### Plot the modeled relationship 

library(ggplot2)
library(mgcv)
library(dplyr)
library(gridExtra)

# First, ensure your GAM model is fitted
flow_data_clean <- flow_data_B %>%
  filter(!is.na(discharge_m3.s) & !is.na(dischargeCMS_WI) & 
           discharge_m3.s > 0 & dischargeCMS_WI > 0)

model_gam <- gam(discharge_m3.s ~ s(dischargeCMS_WI, bs = "cr", k = 4), 
                 data = flow_data_clean)

# Generate predictions for the full range
flow_data_Bb <- flow_data_WI %>%
  mutate(
    site = "browns",
    flow = pmax(0, predict(model_gam, 
                           newdata = data.frame(dischargeCMS_WI = dischargeCMS_WI)))
  ) %>%
  dplyr::select(site, datetime, flow, dischargeCMS_WI = dischargeCMS_WI)

# Create a sequence for smooth prediction line
prediction_range <- data.frame(
  dischargeCMS_WI = seq(min(flow_data_WI$dischargeCMS_WI, na.rm = TRUE),
                        max(flow_data_WI$dischargeCMS_WI, na.rm = TRUE),
                        length.out = 200)
)
prediction_range$predicted_flow <- predict(model_gam, newdata = prediction_range)

# ---- PLOT 1: Observed vs Predicted (Rating Curve) ----
p1 <- ggplot() +
  # Add the smooth GAM prediction line
  geom_line(data = prediction_range, 
            aes(x = dischargeCMS_WI, y = predicted_flow),
            color = "red", size = 1.2, linetype = "solid") +
  # Add observed data points
  geom_point(data = flow_data_clean, 
             aes(x = dischargeCMS_WI, y = discharge_m3.s),
             size = 4, color = "black", shape = 21, fill = "lightblue", stroke = 1.5) +
  # Add 1:1 reference line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", 
              color = "gray40", size = 0.8) +
  labs(
    title = "Browns vs Winters Creek USGS",
    subtitle = paste0("GAM Model (R² = ", round(summary(model_gam)$r.sq, 3), ")"),
    x = "Winters Creek USGS Flow (m³/s)",
    y = "Browns Flow (m³/s)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70")
  )

# ---- PLOT 2: Time Series Comparison ----
# Merge observed and predicted data
time_series_compare <- flow_data_clean %>%
  filter(site.x  == "browns") %>%
  select(datetime, observed = discharge_m3.s) %>%
  left_join(
    flow_data_Bb %>% select(datetime, predicted = flow),
    by = "datetime"
  )

p2 <- ggplot(time_series_compare, aes(x = datetime)) +
  geom_line(aes(y = predicted, color = "GAM Predicted"), size = 1) +
  geom_point(aes(y = observed, color = "Observed"), size = 3) +
  scale_color_manual(
    name = "Data Type",
    values = c("GAM Predicted" = "red", "Observed" = "blue")
  ) +
  labs(
    title = "Observed vs GAM Predicted Flow",
    x = "Date",
    y = "Flow (m³/s)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# ---- PLOT 3: Residuals Plot ----
residuals_data <- flow_data_clean %>%
  mutate(
    predicted = predict(model_gam),
    residuals = discharge_m3.s - predicted
  )

p3 <- ggplot(residuals_data, aes(x = predicted, y = residuals)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_point(size = 3, color = "darkblue", alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "red", fill = "pink", alpha = 0.3) +
  labs(
    title = "Residuals Plot",
    x = "Predicted Flow (m³/s)",
    y = "Residuals (Observed - Predicted)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

# ---- PLOT 4: Observed vs Predicted 1:1 Plot ----
p4 <- ggplot(residuals_data, aes(x = predicted, y = discharge_m3.s)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  geom_point(size = 4, color = "darkgreen", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "pink", alpha = 0.3) +
  labs(
    title = "Observed vs Predicted (1:1 Plot)",
    x = "GAM Predicted Flow (m³/s)",
    y = "Observed Flow (m³/s)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

# ---- Display all plots ----
# Show them individually
print(p1)
print(p2)
print(p3)
print(p4)

# Or combine into a multi-panel figure
combined_plot <- grid.arrange(p1, p2, p3, p4, ncol = 2)

# Save the combined plot if desired
# ggsave("Browns_flow_model_comparison.png", combined_plot, width = 8, height = 6, dpi = 300)

# ---- Print model diagnostics ----
cat("\n=== GAM Model Summary ===\n")
summary(model_gam)

cat("\n=== Model Performance Metrics ===\n")
cat("R-squared:", round(summary(model_gam)$r.sq, 4), "\n")
cat("RMSE:", round(sqrt(mean(residuals_data$residuals^2)), 4), "m³/s\n")
cat("MAE:", round(mean(abs(residuals_data$residuals)), 4), "m³/s\n")
cat("Bias:", round(mean(residuals_data$residuals), 4), "m³/s\n")


### combine GAMM modeled flow 
flow_data_B <- flow_data_Bb %>%
  dplyr::select(site, datetime, flow)

#####
### Other flows 



flow_data_O <-flow_data_OP%>%
  mutate(site = "ophir",
         flow = dischargeCMS_OP) %>%
  dplyr::select(site, datetime, flow)

flow_data_D <-flow_data_DA%>%
  mutate(site = "davis",
         flow = dischargeCMS_DA) %>%
  dplyr::select(site, datetime, flow)


flow_data_WUSGS <-flow_data_WI%>%
  mutate(site = "winters_usgs",
         flow = dischargeCMS_WI) %>%
  dplyr::select(site, datetime, flow)


### Winters up. linear 
flow_data <- flow_m_dat%>%
  dplyr::filter(site =="winters_up") %>%
  left_join(flow_data_WI, by=c("datetime")) %>%
  left_join(flow_data_OP, by=c("datetime"))


#### !!!!!


library(mgcv)
library(ggplot2)

# 1. Explore your data
ggplot(flow_data, aes(x = dischargeCMS_WI, y = discharge_m3.s)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr"), 
              se = TRUE, color = "red") +
  labs(title = "Flow Relationship: Browns vs Winters Creek",
       x = "Winters Creek Flow (m³/s)",
       y = "Winters Up Flow (m³/s)") +
  theme_minimal()

# 2. Fit multiple models and compare
flow_data_clean <- flow_data %>%
  filter(!is.na(discharge_m3.s) & !is.na(dischargeCMS_WI) & 
           discharge_m3.s > 0 & dischargeCMS_WI > 0)

# Power law model
model_power <- lm(log(discharge_m3.s) ~ log(dischargeCMS_WI), 
                  data = flow_data_clean)

# GAM model
model_gam <- gam(discharge_m3.s ~ s(dischargeCMS_WI, bs = "cr", k = 4), 
                 data = flow_data_clean)

# Compare models
cat("Power Law R²:", summary(model_power)$r.squared, "\n")
cat("GAM R²:", summary(model_gam)$r.sq, "\n")

# 3. Use the best model for prediction
flow_data_WU <- flow_data_WI %>%
  mutate(
    site = "winters_up",
    # GAM prediction with handling for edge cases
    flow = pmax(0, predict(model_gam, 
                           newdata = data.frame(dischargeCMS_WI = dischargeCMS_WI)))
  ) %>%
  dplyr::select(site, datetime, flow)



###
### Plot the modeled relationship 

library(ggplot2)
library(mgcv)
library(dplyr)
library(gridExtra)

# First, ensure your GAM model is fitted
flow_data_clean <- flow_data %>%
  filter(!is.na(discharge_m3.s) & !is.na(dischargeCMS_WI) & 
           discharge_m3.s > 0 & dischargeCMS_WI > 0)

model_gam <- gam(discharge_m3.s ~ s(dischargeCMS_WI, bs = "cr", k = 4), 
                 data = flow_data_clean)

# Generate predictions for the full range
flow_data_WU <- flow_data_WI %>%
  mutate(
    site = "winters_up",
    flow = pmax(0, predict(model_gam, 
                           newdata = data.frame(dischargeCMS_WI = dischargeCMS_WI)))
  ) %>%
  dplyr::select(site, datetime, flow, dischargeCMS_WI = dischargeCMS_WI)

# Create a sequence for smooth prediction line
prediction_range <- data.frame(
  dischargeCMS_WI = seq(min(flow_data_WI$dischargeCMS_WI, na.rm = TRUE),
                        max(flow_data_WI$dischargeCMS_WI, na.rm = TRUE),
                        length.out = 200)
)
prediction_range$predicted_flow <- predict(model_gam, newdata = prediction_range)

# ---- PLOT 1: Observed vs Predicted (Rating Curve) ----
p1 <- ggplot() +
  # Add the smooth GAM prediction line
  geom_line(data = prediction_range, 
            aes(x = dischargeCMS_WI, y = predicted_flow),
            color = "red", size = 1.2, linetype = "solid") +
  # Add observed data points
  geom_point(data = flow_data_clean, 
             aes(x = dischargeCMS_WI, y = discharge_m3.s),
             size = 4, color = "black", shape = 21, fill = "lightblue", stroke = 1.5) +
  # Add 1:1 reference line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", 
              color = "gray40", size = 0.8) +
  labs(
    title = "Winters Up vs Winters Creek USGS",
    subtitle = paste0("GAM Model (R² = ", round(summary(model_gam)$r.sq, 3), ")"),
    x = "Winters Creek USGS Flow (m³/s)",
    y = "Winters Up. Flow (m³/s)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70")
  )

# ---- PLOT 2: Time Series Comparison ----
# Merge observed and predicted data
time_series_compare <- flow_data_clean %>%
  filter(site.x  == "winters_up") %>%
  select(datetime, observed = discharge_m3.s) %>%
  left_join(
    flow_data_Bb %>% select(datetime, predicted = flow),
    by = "datetime"
  )

p2 <- ggplot(time_series_compare, aes(x = datetime)) +
  geom_line(aes(y = predicted, color = "GAM Predicted"), size = 1) +
  geom_point(aes(y = observed, color = "Observed"), size = 3) +
  scale_color_manual(
    name = "Data Type",
    values = c("GAM Predicted" = "red", "Observed" = "blue")
  ) +
  labs(
    title = "Observed vs GAM Predicted Flow",
    x = "Date",
    y = "Flow (m³/s)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# ---- PLOT 3: Residuals Plot ----
residuals_data <- flow_data_clean %>%
  mutate(
    predicted = predict(model_gam),
    residuals = discharge_m3.s - predicted
  )

p3 <- ggplot(residuals_data, aes(x = predicted, y = residuals)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_point(size = 3, color = "darkblue", alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "red", fill = "pink", alpha = 0.3) +
  labs(
    title = "Residuals Plot",
    x = "Predicted Flow (m³/s)",
    y = "Residuals (Observed - Predicted)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

# ---- PLOT 4: Observed vs Predicted 1:1 Plot ----
p4 <- ggplot(residuals_data, aes(x = predicted, y = discharge_m3.s)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  geom_point(size = 4, color = "darkgreen", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "pink", alpha = 0.3) +
  labs(
    title = "Observed vs Predicted (1:1 Plot)",
    x = "GAM Predicted Flow (m³/s)",
    y = "Observed Flow (m³/s)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

# ---- Display all plots ----
# Show them individually
print(p1)
print(p2)
print(p3)
print(p4)

# Or combine into a multi-panel figure
combined_plot <- grid.arrange(p1, p2, p3, p4, ncol = 2)

# Save the combined plot if desired
# ggsave("Winters_Up_flow_model_comparison.png", combined_plot, width = 8, height = 6, dpi = 300)

# ---- Print model diagnostics ----
cat("\n=== GAM Model Summary ===\n")
summary(model_gam)

cat("\n=== Model Performance Metrics ===\n")
cat("R-squared:", round(summary(model_gam)$r.sq, 4), "\n")
cat("RMSE:", round(sqrt(mean(residuals_data$residuals^2)), 4), "m³/s\n")
cat("MAE:", round(mean(abs(residuals_data$residuals)), 4), "m³/s\n")
cat("Bias:", round(mean(residuals_data$residuals), 4), "m³/s\n")


### combine GAMM modeled flow 
flow_data_WU <- flow_data_WU %>%
  dplyr::select(site, datetime, flow)
###!!!!

#####
#### 2026-01-26  pause 
flow_dat <- rbind(flow_data_B, flow_data_BS, flow_data_O, flow_data_WUSGS, flow_data_WU, flow_data_D) %>%
  mutate(date =as.Date(datetime)) %>%
  group_by(site, date) %>%
  summarise(
    flow = mean(flow, na.rm=T))



## Save data
# write.csv(flow_dat, "/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Processes_data/ERDC_water_hourlyflow.csv", row.names = FALSE)




# 
# ### browns sub
# 
# flow_data <- flow_m_dat%>%
#   dplyr::filter(site =="browns_sub") %>%
#   left_join(flow_data_WI, by=c("date")) %>%
#   left_join(flow_data_OP, by=c("date"))
# 
# plot(x=flow_data$dischargeCMS_OP, y=flow_data$discharge_m3.s)
# model <- lm(discharge_m3.s ~ dischargeCMS_OP, data = flow_data)
# summary(model)
# 
# ## winters
# flow_data$predicted_discharge_WI <- 0.018422 + 0.846235 * flow_data$dischargeCMS_WI
# 
# flow_data$predicted_discharge_OP <- 0.016623 + 0.253918 * flow_data$dischargeCMS_OP
# 
# 
# flow_data$predicted_discharge
# flow_data$residual_WI <- flow_data$discharge_m3.s - flow_data$predicted_discharge_WI
# 
# flow_data$residual_OP <- flow_data$discharge_m3.s - flow_data$predicted_discharge_OP
# 


merged_dff <-merged_df %>%
  left_join(flow_dat, by=c("site_lab"= "site", "date"))

# Load precipitation data
ppt_dat <- read.csv("/Users/kellyloria/Documents/shinyapp/ShinyERDC/prism_cat_new.csv") %>%
  mutate(
    date = as.Date(Date, format = "%Y-%m-%d"),
    lagPPT = lag(ppt..mm.),
    lag_C_PPT = ppt..mm. + lagPPT) %>%
  dplyr::select(Name, date, ppt..mm., lag_C_PPT)


# Join precipitation and flow to chemistry data
merged_df1 <- merged_dff %>%
  left_join(ppt_dat, by = c("date", "USGS_gage" = "Name"))

## Save data
# write.csv(merged_df, "/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Processes_data/ERDC_water_only_chem_dat_processed.csv", row.names = FALSE)


# write.csv(merged_df1, "/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Processes_data/ERDC_water_only_chem_dat_CQ.csv", row.names = FALSE)
