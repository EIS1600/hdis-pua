################################################################################
# PREPARING PUA DATA ###########################################################
################################################################################

# LOAD ALL DATA AND SAVE INTO A SINGLE RDS FILE FOR EASY LOADING AND PROCESSING
# - LOOP APPROACH

# Load required packages
library(tibble)
library(readr)
library(fs)
library(tools)

# Define the directory containing the TSV files
data_dir <- "Data/raw_PUA/"

# List all TSV files in the directory
tsv_files <- dir_ls(data_dir, regexp = "\\.tsv$")

# Initialize an empty list to store the tibbles
tibble_list <- list()

# Loop through the TSV files, read them, and add them to the list with proper names
for (i in seq_along(tsv_files)) {
  file_path <- tsv_files[[i]]
  tibble_data <- read_tsv(file_path)
  
  # Extract the filename without the path and ".tsv" extension
  file_name <- path_file(file_path)
  name_no_ext <- file_path_sans_ext(file_name)
  
  # Add the tibble to the list and assign the name without the extension
  tibble_list[[name_no_ext]] <- tibble_data
}

# Print the list of tibbles
print(tibble_list)

saveRDS(tibble_list, "Data/PUA_processed/PUA_allDataTables_asList.rds")
