# Basic data prep
# Last edit 2024-01-20
###############################
#
# This script performs the following actions:
# 1. Load dependencies
# 2. Configuration per dataset
# 3. Prepare data for relabelling: 
#   - load raw data
#   - merge together (if necessary) 
#   - check if tibble and convert into tibble if necessary 
#   - create codebook from raw data

### 1. Load dependencies ####
# Generic libraries
library(readxl) 
library(haven) # for working with SPSS files
library(codebook) # for making codebook
library(tidyverse) # multiple purposes; e.g., read_delim
library(labelled)

# Custom functions
source("R/create_codebook.R")
source("R/create_rename_file.R")
source("R/make_lowercase.R")

### 2. Configuration per dataset ####
# Name of the study, to be used in file and folder naming
studyname <- "102.us-lls"

# Path(s) to where the raw data files are that need to be read in and cleaned

# If 1 file
file_paths <- c("data/102.us-lls/2.data-checks/ICITOP/Lehigh_ICITOP.sav")

# Path for writing the raw codebook to. end with xlsx
codebookpath <- paste0("data/102.us-lls/2.data-checks/codebook-raw-102.us-lls.xlsx")

# Folder for writing rename files to
renamepath <- "data/102.us-lls/2.data-checks"

###### Run ###### 
### 3. Prepare data for relabelling ####
# Load data file(s) into a list object, the name of each list item is the file path for trackability
dfs <- sapply(file_paths, 
              function(path) read_sav(path), 
              simplify = FALSE)

# Make sure that ID's are the same; in this case some are upper and other are lowercase
dfs_lower <- lapply(dfs, make_lowercase)

# Merge all dfs into 1 merged_df
merged_df <- reduce(dfs_lower, full_join, by = "id")

# Convert the source_id column to a factor, if neccesary
#merged_df$source_id <- as.factor(merged_df$source_id)

# Check if merged_df is a tbl_df and otherwise convert to tibble
if ("tbl_df" %in% class(merged_df)) {
  print("It's a tibble")
} else {
  merged_df <- as_tibble(data.frame(merged_df))
  print("Converted to tibble")
}

# Make a codebook from the raw dataset and write it to csv
# Note that this takes a long time
data_dictionary <- create_codebook(merged_df, codebookpath)

# Write an empty rename file for this study: one as empty one, and one to be filled
rename_empty <- create_rename_file(data_dictionary, studyname, renamepath)

# Write the merged_df dataframe to an .rda file for later further processing
save(merged_df, file= paste0(renamepath,"/dataprep-",
                             studyname,
                             ".rda"))
