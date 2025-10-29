## 2025-10-16 water chemistry QA'QC

# DATE of last clean : 2025-10-15

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
    site == "Browns" ~ "winters", 
    site == "Browns sub" ~ "winters",
    site == "Browns Sub" ~ "winters")) %>%
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

# I. grab relevant data columns 

davis_df <- davis_data%>%
  dplyr::select("site", "site_lab", "USGS_gage", "datetime", "year", "jday", "mon", "Lat", "Long", 
         "temp", "DO", "EC", "pH", 
         "ORP", "turb", "notes", "BC", "BC run DL", "TSS", "NO3-N", "NO3-NO2-N", 
         "NO2-N", "NH4-N", "TKN", "PO4-P", "PO4-ortho", "TDN-calc", "Alkalinity",
         "Cl", "SO4", "DOC", "TDN", "TOC", "Al", "As", "B", "Ba", "Ca", "Cr",
         "Fe", "Mn", "Pb", "Sr", "Zn")


str(davis_df)


## Step 2: ##
# II. # Flexible helper function
#    - Need to swap out "BDL" in each column with a flag for when the value is "BDL"
#    - Add columns with actual BDL values
#    - Add number column for plotting and stats where when observations are "BDL" they transformed to "1/2 the detection limit"

names(davis_df)

# Flexible helper function
handle_BDL <- function(df, var, DL) {
  var_sym <- sym(var)
  
  # If DL is a column name (string), use per-row detection limits
  if (is.character(DL) && DL %in% names(df)) {
    dl_sym <- sym(DL)
    
    df %>%
      mutate(
        !!var_sym := if_else(!!var_sym == "BDL", !!dl_sym, as.numeric(!!var_sym)),
        !!paste0(var, "_DL_lab") := case_when(
          !!var_sym <= !!dl_sym ~ "yes",
          TRUE ~ "no"
        ),
        !!paste0(var, "_QC") := case_when(
          !!sym(paste0(var, "_DL_lab")) == "yes" ~ !!dl_sym / 2,
          TRUE ~ !!var_sym
        )
      )
    
  } else {
    # Constant detection limit
    df %>%
      mutate(
        !!var_sym := if_else(!!var_sym == "BDL", DL, as.numeric(!!var_sym)),
        !!paste0(var, "_DL_lab") := case_when(
          !!var_sym <= DL ~ "yes",
          TRUE ~ "no"
        ),
        !!paste0(var, "_QC") := case_when(
          !!sym(paste0(var, "_DL_lab")) == "yes" ~ DL / 2,
          TRUE ~ !!var_sym
        )
      )
  }
}


# Define dectection limits:
library(tibble)

dl_table <- tribble(
  ~var,          ~DL,
  "BC",          list("BC run DL"),  # dynamic DL (column name)
  "NO3-N",       list(0.02),
  "NO3-NO2-N",   list(0.02),
  "NO2-N",       list(0.01),
  "NH4-N",       list(0.1),
  "TKN",         list(0.2),
  "TDN-calc",    list(0.1),
  "PO4-P",       list(0.03),
  "PO4-ortho",   list(0.1),
  "SO4",         list(0.9),
  "TDN",         list(0.05),
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
  "Zn",          list(1)
)


davis_df2 <- reduce(
  seq_len(nrow(dl_table)),
  ~ handle_BDL(.x, dl_table$var[.y], unlist(dl_table$DL[.y])),
  .init = davis_df
)


davis_df2 <- davis_df2 %>%
  mutate(
    Alkalinity_QC = as.numeric(Alkalinity),
    Cl_QC = as.numeric(Cl),
    DOC_QC = as.numeric(DOC),
    TOC_QC = as.numeric(TOC)
  )



### collect QAQC columns 
davis_df <- davis_df2%>%
  dplyr::select("site", "site_lab", "USGS_gage", "datetime", "year", "jday", "mon", "Lat", "Long", 
                "temp", "DO", "EC", "pH", 
                "ORP", "turb", "notes", "TSS", 
                "BC_DL_lab", "BC_QC", "NO3-N_DL_lab", "NO3-N_QC", "NO3-NO2-N_DL_lab", "NO3-NO2-N_QC", 
                "NO2-N_DL_lab", "NO2-N_QC", "NH4-N_DL_lab", "NH4-N_QC", "TKN_DL_lab", "TKN_QC", 
                "TDN-calc_DL_lab", "TDN-calc_QC", "PO4-P_DL_lab", "PO4-P_QC", 
                "PO4-ortho_DL_lab", "PO4-ortho_QC", "Alkalinity_QC","Cl_QC",
                "SO4_DL_lab", "SO4_QC", "DOC_QC", "TOC_QC", "TDN_DL_lab", "TDN_QC",
                "Al_DL_lab", "Al_QC", "As_DL_lab", "As_QC", "B_DL_lab", "B_QC",
                "Ba_DL_lab", "Ba_QC", "Ca_DL_lab", "Ca_QC", "Cr_DL_lab", "Cr_QC",
                "Fe_DL_lab", "Fe_QC", "Mn_DL_lab", "Mn_QC", "Pb_DL_lab", "Pb_QC",
                "Sr_DL_lab" , "Sr_QC", "Zn_DL_lab", "Zn_QC")


## Step 3: ##
### use YSI data to fill in missing GPS
ysi_dat <- read_csv("/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Processes_data/YSI_KOR_dat_20251003_processed.csv")

ysi_dat <- ysi_dat[,c(1:23)]

ysi_dat<- as.data.frame(ysi_dat)

str(ysi_dat)

##
##
library(dplyr)

# 1. Prepare date columns for matching
# (extract date part from datetime in davis_df)
davis_df <- davis_df %>%
  mutate(date = as.Date(datetime))

# 2. Select relevant columns from ysi_dat to merge
ysi_cols <- c("site_lab", "USGS_gage", "date",
              "lat.", "long.", "DO_sat", "DO_mgL", "ORP_MV", "pH", "TEMP_C", "turbidity_FNU")

ysi_dat_sub <- ysi_dat %>%
  select(all_of(ysi_cols))

# 3. Merge datasets by common keys
merged_df <- left_join(davis_df, ysi_dat_sub,
                       by = c("site_lab", "USGS_gage", "date"))

# 4. Replace NA values in davis_df with ysi_dat values where available
merged_df <- merged_df %>%
  mutate(
    Lat  = ifelse(is.na(Lat),  lat.,  Lat),
    Long = ifelse(is.na(Long), long., Long),
    DO   = ifelse(is.na(DO),   DO_mgL, DO),
    ORP  = ifelse(is.na(ORP),  ORP_MV, ORP),
    pH   = ifelse(is.na(pH.x),   pH.y,   pH.x),     # handle name clash with ysi_dat$pH
    temp = ifelse(is.na(temp), TEMP_C, temp),
    turb = ifelse(is.na(turb), turbidity_FNU, turb)
  )

# 5. Clean up redundant columns (those from ysi_dat)

merged_df <- merged_df%>%
  dplyr::select("site", "site_lab", "USGS_gage", "date","datetime", "year", "jday", "mon", "Lat", "Long", 
                "temp", "DO", "EC", "pH", 
                "ORP", "turb", "notes", "TSS", 
                "BC_DL_lab", "BC_QC", "NO3-N_DL_lab", "NO3-N_QC", "NO3-NO2-N_DL_lab", "NO3-NO2-N_QC", 
                "NO2-N_DL_lab", "NO2-N_QC", "NH4-N_DL_lab", "NH4-N_QC", "TKN_DL_lab", "TKN_QC", 
                "TDN-calc_DL_lab", "TDN-calc_QC", "PO4-P_DL_lab", "PO4-P_QC", 
                "PO4-ortho_DL_lab", "PO4-ortho_QC", "Alkalinity_QC","Cl_QC",
                "SO4_DL_lab", "SO4_QC", "DOC_QC", "TOC_QC", "TDN_DL_lab", "TDN_QC",
                "Al_DL_lab", "Al_QC", "As_DL_lab", "As_QC", "B_DL_lab", "B_QC",
                "Ba_DL_lab", "Ba_QC", "Ca_DL_lab", "Ca_QC", "Cr_DL_lab", "Cr_QC",
                "Fe_DL_lab", "Fe_QC", "Mn_DL_lab", "Mn_QC", "Pb_DL_lab", "Pb_QC",
                "Sr_DL_lab" , "Sr_QC", "Zn_DL_lab", "Zn_QC")
summary(merged_df)
## Save data
# write.csv(merged_df, "/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Processes_data/ERDC_water_chem_dat_20251015_processed.csv", row.names = FALSE)
