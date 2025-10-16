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
write.csv(PRISMdata, "./prism_cat.csv")


