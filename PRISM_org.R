# Load libraries
library(tidyverse)

# Path to your folder containing CSVs
folder_path <- "./prism"

# List all CSV files in the folder (and subfolders if needed)
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)

# Read and combine all files, skipping first 10 rows
PRISMdata <- csv_files %>%
  map_df(~ read_csv(.x, skip = 10))

# Peek at data
glimpse(PRISMdata)

## save
# write.csv(PRISMdata, "./prism_cat.csv")


## Temp workflow from mac - 

# Path to your folder containing CSVs
folder_path <- "/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Raw_data/PRISM_aug_oct"

# List all CSV files in the folder (and subfolders if needed)
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)

# Read and combine all files, skipping first 10 rows
PRISMdata <- csv_files %>%
  map_df(~ read_csv(.x, skip = 10))

# Peek at data
glimpse(PRISMdata)


# write.csv(PRISMdata, "/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Raw_data/prism_cat_12_10.csv")


## bring old and new 
old_prism <- read.csv("/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Raw_data/prism_cat.csv")
names(old_prism)

new_prism <- read.csv("/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Raw_data/prism_cat_12_10.csv")%>%
  dplyr::select("X", "Name", "Longitude", "Latitude", "Elevation..m.",  "Date", "ppt..mm.", "tmean..degrees.C.")
names(new_prism)


prism_dat <- rbind(old_prism, new_prism)

prism_dat <- prism_dat %>%
  group_by(Name, Date)%>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# write.csv(prism_dat, "/Users/kellyloria/Documents/DRI/ERDC_Davis_Fire/Raw_data/prism_cat_new.csv")



