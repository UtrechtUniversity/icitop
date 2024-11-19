# Basic data prep - extra preparation
# Last edit 2024-11-19
###############################
#
# This script performs the following actions:
# 1. Load dependencies
# 2. Configuration per dataset
# 3. Evaluate which ICPSR files are needed for data renaming


### 1. Load dependencies ####
# Generic libraries
library(readxl) 
library(haven) # for working with SPSS files
library(tidyverse) # multiple purposes; e.g., read_delim
library(labelled)

### 2. Configuration per dataset ####
# Name of the study, to be used in file and folder naming
studyname <- "15.us-oys"

# 
# Path(s) to where the raw data files are that need to be read in and cleaned
file_paths <- c(paste0("data/", studyname,"/1.raw-data/nov 2024/wave 1-ICPSR_37939/37939-manifest.txt"),
                paste0("data/", studyname,"/1.raw-data/nov 2024/wave 2-ICPSR_38146-V1/38146-manifest.txt"),
                paste0("data/", studyname,"/1.raw-data/nov 2024/wave 3-ICPSR_38198-V1/38198-manifest.txt"),
                paste0("data/", studyname,"/1.raw-data/nov 2024/wave 4-ICPSR_38246-V1/38246-manifest.txt"),
                paste0("data/", studyname,"/1.raw-data/nov 2024/wave 5-ICPSR_38277-V1/38277-manifest.txt"),
                paste0("data/", studyname,"/1.raw-data/nov 2024/wave 6-ICPSR_38256-V1/38256-manifest.txt"),
                paste0("data/", studyname,"/1.raw-data/nov 2024/wave 7-ICPSR_38279-V1/38279-manifest.txt"),
                paste0("data/", studyname,"/1.raw-data/nov 2024/wave 8-ICPSR_38280-V1/38280-manifest.txt"),
                paste0("data/", studyname,"/1.raw-data/nov 2024/wave 9-ICPSR_38281-V1/38281-manifest.txt"),
                paste0("data/", studyname,"/1.raw-data/nov 2024/wave 10-ICPSR_38282-V1/38282-manifest.txt"),
                paste0("data/", studyname,"/1.raw-data/nov 2024/wave 11-ICPSR_38283-V1/38283-manifest.txt"),
                paste0("data/", studyname,"/1.raw-data/nov 2024/wave 12-ICPSR_38284-V1/38284-manifest.txt"),
                paste0("data/", studyname,"/1.raw-data/nov 2024/wave 13-ICPSR_38285-V1/38285-manifest.txt"),
                paste0("data/", studyname,"/1.raw-data/nov 2024/wave 14-ICPSR_38433-V1/38433-manifest.txt"),
                paste0("data/", studyname,"/1.raw-data/nov 2024/wave 15-ICPSR_38434-V1/38434-manifest.txt"),
                paste0("data/", studyname,"/1.raw-data/nov 2024/wave 16-ICPSR_38435-V1/38435-manifest.txt"),
                paste0("data/", studyname,"/1.raw-data/nov 2024/wave 17-ICPSR_38436-V1/38436-manifest.txt"),
                paste0("data/", studyname,"/1.raw-data/nov 2024/wave 18-ICPSR_38437-V1/38437-manifest.txt"),
                paste0("data/", studyname,"/1.raw-data/nov 2024/wave 19-ICPSR_38438-V1/38438-manifest.txt"),
                paste0("data/", studyname,"/1.raw-data/nov 2024/wave 20-ICPSR_38439-V1/38439-manifest.txt"),
                paste0("data/", studyname,"/1.raw-data/nov 2024/wave 21-ICPSR_38449-V1/38449-manifest.txt"),
                paste0("data/", studyname,"/1.raw-data/nov 2024/wave 22-ICPSR_38450-V1/38450-manifest.txt"),
                paste0("data/", studyname,"/1.raw-data/nov 2024/wave 23-ICPSR_38451-V1/38451-manifest.txt"),
                paste0("data/", studyname,"/1.raw-data/nov 2024/wave 25-ICPSR_38452-V1/38452-manifest.txt")
                )
                
# Path for writing the raw codebook to
codebookpath <- paste0("data/",studyname,"/2.data-check/codebook-raw-", studyname, ".xlsx")

# Folder for writing rename files to
renamepath <- paste0("data/", studyname, "/2.data-check")

# load all the text files

# Initialize a list to store results
manifests <- list()

# Loop through each file
for (file in file_paths) {
  # Load the text file
  text <- readLines(file)
  
  # Extract lines starting with "DS0"
  ds0_strings <- grep("^DS0", text, value = TRUE)
  
  # Store the results in a named list
  manifests[[basename(file)]] <- ds0_strings
}

manifests_df <- map_dfr(manifests, ~ tibble(content = .x), .id = "filename")

# include an empty row
manifests_df$include<-NA

# save the output file for evaluation
openxlsx::write.xlsx(manifests_df, paste0(renamepath, "/0_prelim_check_ICPSR_manifests_", studyname, ".xlsx"),rowNames = FALSE)

