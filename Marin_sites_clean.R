## 2025-02-09 water chemistry QA'QC


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
davis_data <- read_csv("/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Processes_data/ysi_pack_fire_sites_chem.csv") %>%
  mutate(date = as.Date(date), format="%y-%m-%d")
#View(davis_data)
str(davis_data)


## Step 1: ##
# I.  First cleaning pipe: 
#     - Line up with USGS gage 
#     - Add site_lab to get consistent site names across different sample days 

davis_data <- davis_data %>%
  mutate(year = year(date))%>%
  mutate(jday = yday(date))%>%
  mutate(mon = month(date)) %>%
  # create dummy column for Creek to line up hydroclimate data
  mutate(site_lab = case_when(
    stream == "browns creek" ~ "browns", 
    stream == "browns creek sub" ~ "browns_sub",
    stream == "browns headwaters" ~ "browns_HW",
    stream == "ophir creek" ~ "ophir",
    stream == "ophir headwaters" ~ "ophir_HW",
    stream == "Hilton creek" ~ "hilton",
    stream == "McGee 395" ~ "mcgee_395",
    stream == "McGee creek" ~ "mcgee",
    stream == "McGee park" ~ "mcgee_park"
    ))
names(davis_data)

summary(davis_data)

# I. grab relevant data columns 

davis_df <- davis_data%>%
  dplyr::select("Fire","stream", "site_lab", 
                "date", "lat._mean", "long._mean","altitude_m_mean", "barometer_mmHg_mean",
                "Cond._scm_mean", "SPC_dsmc_mean", "NLF_COND_scm_mean",  "depth_m_mean", "vert_position_m_mean",
                "DO_mgL_mean","DO_sat_mean", "pH_mean",  "pH_MV_mean" , "TEMP_C_mean",
                 "BC", "BC run DL", 
                #"NO3-N", "NO3-NO2-N", 
      #   "NO2-N", "NH4-N", "TKN", "PO4-P", "PO4-ortho", "TDN-calc", "Alkalinity",
       #  "Cl", "SO4", "DOC", "TDN", "TOC", 
      "Al", "As", "B", "Ba", "Ca", "Cr",
         "Fe", "Mn", "Pb", "Sr", "Zn")

davis_df <-as.data.frame(davis_df)
str(davis_df)


### bring in TSS 


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

merged_df <- davis_df %>%
  left_join(tss_mean, by = c("site_lab" ="site", "date")) 





## Step 2: ##
# II. # Flexible helper function
#    - Need to swap out "BDL" in each column with a flag for when the value is "BDL"
#    - Add columns with actual BDL values
#    - Add number column for plotting and stats where when observations are "BDL" they transformed to "1/2 the detection limit"

names(merged_df)

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
  "TSS_mgL",     list(0.01),
  "Alkalinity",  list(2)
)

# Apply detection limit handling - NO unlist() needed
davis_df2 <- reduce(
  seq_len(nrow(dl_table)),
  ~ handle_BDL(.x, dl_table$var[.y], dl_table$DL[[.y]]),
  .init = merged_df
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
  dplyr::select("Fire","stream", "site_lab", 
                "date", "lat._mean", "long._mean","altitude_m_mean", "barometer_mmHg_mean",
                "Cond._scm_mean", "SPC_dsmc_mean", "NLF_COND_scm_mean",  "depth_m_mean", "vert_position_m_mean",
                "DO_mgL_mean","DO_sat_mean", "pH_mean",  "pH_MV_mean" , "TEMP_C_mean",
                "Al_DL_lab", "Al_QC", 
                "As_DL_lab", "As_QC", "B_DL_lab", "B_QC", "Ba_DL_lab", "Ba_QC",
                "Ca_DL_lab", "Ca_QC", "Cr_DL_lab", "Cr_QC", "Fe_DL_lab", "Fe_QC",
                "Mn_DL_lab", "Mn_QC", "Pb_DL_lab", "Pb_QC", "Sr_DL_lab","Sr_QC",
                "Zn_DL_lab", "Zn_QC", 
                "BC_DL_lab","BC_QC")

# 
# 
# dl_table <- tribble(
#   ~var,          ~DL,
#   "BC",          0.0431,
#   "NO3-N",       0.02,
#   "NO3-NO2-N",   0.02,
#   "NO2-N",       0.01,
#   "NH4-N",       0.1,
#   "TKN",         0.2,
#   "TDN-calc",    0.1,
#   "PO4-P",       0.03,
#   "PO4-ortho",   0.01,
#   "SO4",         0.9,
#   "TDN",         0.03,
#   "DOC",         0.1,
#   "TOC",         0.1,
#   "Cl",          0.1,
#   "Al",          1,
#   "As",          0.1,
#   "B",           1,
#   "Ba",          1,
#   "Ca",          10,
#   "Cr",          0.1,
#   "Fe",          1,
#   "Mn",          0.1,
#   "Pb",          0.1,
#   "Sr",          0.1,
#   "Zn",          1,
#   "Alkalinity",  2,
#   "temp",        0.01,
#   "DO",         0.001,
#   "turb",       0.01,
#   "pH",        0.01,
#   "ORP",        0.01,
#   "turb",         0.1
#   
# )
# 
# 
# get_decimals <- function(x) {
#   if (is.na(x) || x == 0) return(0)
#   dec <- abs(floor(log10(x)))  # digits after decimal
#   return(dec + 1)  # keep one extra for reporting
# }
# 
# dl_table <- dl_table %>%
#   mutate(round_digits = map_dbl(DL, get_decimals))
# 
# # --- Step 1: identify all *_QC columns ---
# qc_cols <- names(merged_df)[str_detect(names(merged_df), "_QC$")]
# 
# # --- Step 2: for each *_QC column, match chemical name to DL table ---
# # Remove suffixes to match against `var` in dl_table
# qc_clean <- str_remove(qc_cols, "_QC$")
# 
# # --- Step 3: round each numeric column according to DL ---
# for (i in seq_along(qc_cols)) {
#   chem <- qc_clean[i]
#   round_to <- dl_table$round_digits[match(chem, dl_table$var)]
#   
#   if (!is.na(round_to)) {
#     message("Rounding ", qc_cols[i], " to ", round_to, " decimals")
#     merged_df[[qc_cols[i]]] <- round(merged_df[[qc_cols[i]]], round_to)
#   }
# }
# 
# # 2. Select relevant columns from ysi_dat to merge
# ysi_round <- c("temp", "DO", "EC", "ORP", "turb", "pH")
# 
# WQ_cols <- names(merged_df)[8:12]
# 
# 
# for (i in seq_along(WQ_cols)) {
#   chem <- ysi_round[i]
#   round_to <- dl_table$round_digits[match(chem, dl_table$var)]
#   
#   if (!is.na(round_to)) {
#     message("Rounding ", WQ_cols[i], " to ", round_to, " decimals")
#     merged_df[[WQ_cols[i]]] <- round(merged_df[[WQ_cols[i]]], round_to)
#   }
# }
# 

## Save data
# write.csv(davis_df2, "/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Processes_data/Marin_fire_dat_processed.csv", row.names = FALSE)


# write.csv(merged_df1, "/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Processes_data/ERDC_water_only_chem_dat_CQ.csv", row.names = FALSE)
