################################################################################
# PREPARING PUA DATA ###########################################################
################################################################################

# LOAD ALL DATA AND SAVE INTO A SINGLE RDS FILE FOR EASY LOADING AND PROCESSING
# - AN ALTERNATIVE TO LOOP

# Load required packages
library(tibble)
library(readr)
library(purrr)
library(fs)
library(tools)

# Define the directory containing the TSV files
data_dir <- "Data/raw_PUA/"

# List all TSV files in the directory
tsv_files <- dir_ls(data_dir, regexp = "\\.tsv$")

# Function to read a TSV file and return a tibble
read_tsv_file <- function(file_path) {
  tibble_data <- read_tsv(file_path)
  return(tibble_data)
}

# Read all TSV files: an alternative too loop
tibble_list <- map(tsv_files, read_tsv_file)

# Extract filenames without the path
file_names <- path_file(tsv_files)

# Remove ".tsv" extension from the filenames
file_names_no_ext <- file_path_sans_ext(file_names)

# Assign the filenames without the extension as names for the list items
names(tibble_list) <- file_names_no_ext

saveRDS(tibble_list, "Data/PUA_processed/PUA_allDataTables_asList.rds")