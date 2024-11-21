# Basic data prep
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
source("R/same_id.R")

### 2. Configuration per dataset ####
# Name of the study, to be used in file and folder naming
studyname <- "15.us-oys"

# Path(s) to where the raw data files are that need to be read in and cleaned
# Use the preliminary check excel file
include<-read.xlsx("data/15.us-oys/2.data-check/0_prelim_check_ICPSR_manifests_15.us-oys_filled.xlsx")
include<-include[include$include==1,]


# Path for writing the raw codebook to
codebookpath <- paste0("data/",studyname,"/2.data-check/codebook-raw-", studyname, ".xlsx")

# Folder for writing rename files to
renamepath <- paste0("data/", studyname, "/2.data-check")

###### Run ###### 
### 3. Prepare data for relabelling ####
# Load data file(s) into a list object
# 1 select the folder that contains the first 5 digits of include$filename
# 2. open the folder that contains the first 6 digits of include$content
# 3. load the spss file in this folder
base_dir <- "data/15.us-oys/1.raw-data/Nov 2024"  
substring_to_match <- substr(include$filename, 1, 5)
folder_6_digits <- substr(include$content, 1, 6)

# Function to process each row in `include`
load_spss_files <- function(dataset, basedir) {
  
  # Get all subdirectories in the base directory
  folders <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
  
  # Initialize a list to store results
  spss_data_list <- list()
  
  # Loop through each row in the dataset
  for (i in seq_len(nrow(dataset))) {
    # Step 1: Get the folder matching the first 5 digits of filename
    folder_path_5 <- folders[str_detect(folders, substring_to_match[i])]
    
    if (length(folder_path_5) == 0) {
      warning(paste("No matching folder for", substring_to_match[i]))
      spss_data_list[[i]] <- NULL
      next
    }
    
    # Step 2: Append the folder for the first 6 digits of content
    folder_path_6 <- file.path(folder_path_5, folder_6_digits[i])
    
    # Step 3: Find and load the SPSS file in this folder
    spss_files <- list.files(folder_path_6, pattern = "\\.sav$", full.names = TRUE)
    
    if (length(spss_files) > 0) {
      # Load the first SPSS file found
      spss_data_list[[i]] <- read_sav(spss_files[1])
    } else {
      warning(paste("No SPSS files found in", folder_path_6))
      spss_data_list[[i]] <- NULL
    }
  }
  
  # Return the list of loaded data
  return(spss_data_list)
}

dfs<-load_spss_files(include,basedir = base_dir)

# remove empty lists
dfs <- dfs[!sapply(dfs, is.null)]

# remove empty rows
dfs <- dfs[!sapply(dfs, function(x) is.null(x) || (is.data.frame(x) && nrow(x) == 0))]

# Make sure that ID's are the same; in this case some are upper and other are lowercase
dfs_lower <- lapply(dfs, make_lowercase)

# some files have a family id with the letter of the project in front of them. 
# other files only have the family id
id_check(dfs_lower, "family") # for some reason this code doesn't work anymore

# Continue here: make the id code the same across studies

# due to limited ram capacity: divide the list up into chunks

# Define the chunk size
chunk_size <- 20

# Initialize an empty list to store intermediate merged data frames
merged_chunks <- list()

# Loop through `dfs_lower` in chunks of size 20
for (i in seq(1, length(dfs_lower), by = chunk_size)) {
  # Define the start and end indices for the current chunk
  start_idx <- i
  end_idx <- min(i + chunk_size - 1, length(dfs_lower))  # Handle the last chunk
  
  # Extract the current chunk
  chunk <- dfs_lower[start_idx:end_idx]
  
  # Merge the chunk using reduce and store the result
  merged_chunks[[length(merged_chunks) + 1]] <- reduce(chunk, full_join, by = "family")
}



# Final step: Merge all chunks together
final_merged_df <- reduce(merged_chunks, full_join, by = "family")

# Output the final merged data frame
final_merged_df




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
# all characterers are not displayed properly in codebook. Transfer to numeric for codebook
merged_df_codebook<-lapply(merged_df, as.numeric)
data_dictionary <- create_codebook(dfs, merged_df_codebook, codebookpath)

# Write an empty rename file for this study: one as empty one, and one to be filled
rename_empty <- create_rename_file(data_dictionary, studyname, renamepath)

# Write the merged_df dataframe to an .rda file for later further processing
save(merged_df, file= paste0("data/",studyname, "/2.data-check/dataprep-",
                             studyname,
                             ".rda"))

