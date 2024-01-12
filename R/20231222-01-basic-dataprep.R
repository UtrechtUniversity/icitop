# Basic data prep
# Last edit 2024-01-08
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
studyname <- "24.us-mlsra"

# Path(s) to where the raw data files are that need to be read in and cleaned
file_paths <- c("data/24.us-mlsra/2. data checks/2nd Gen Children Data/Maternal Sensitivity/maternal Sensitivity 2020.06.24.sav",
                "data/24.us-mlsra/2. data checks/2nd Gen Children Data/Caldwell HOME/1st grade/GR1Caldwell_HOME_revised_2019.06.24.sav",
                "data/24.us-mlsra/2. data checks/2nd Gen Children Data/Caldwell HOME/30 months/HOME30.sav",
                "data/24.us-mlsra/2. data checks/2nd Gen Parents Data - 3rd Gen Children/12 month/SG12M Parent Interview 5.19.14 - FINALIZED.sav",
                "data/24.us-mlsra/2. data checks/2nd Gen Parents Data - 3rd Gen Children/24 month/sg24m parent ratings FINALIZED 5.19.14.sav",
                "data/24.us-mlsra/2. data checks/2nd Gen Parents Data - 3rd Gen Children/24 month/SG24m Parent Interview FINALIZED 5.19.14.sav",
                "data/24.us-mlsra/2. data checks/2nd Gen Parents Data - 3rd Gen Children/42 month/SG42M interview revision FINALIZED 5.19.14.sav",
                "data/24.us-mlsra/2. data checks/2nd Gen Parents Data - 3rd Gen Children/42 month/SG42M Parenting Ratings - FINALIZED 5.19.14.sav",
                "data/24.us-mlsra/2. data checks/Moderators/G2 Parents/12 month parent information/SG12M Parent Interview 5.19.14 - FINALIZED.sav",
                "data/24.us-mlsra/2. data checks/Moderators/G2 Parents/24 month parent information/SG24m Parent Interview FINALIZED 5.19.14.sav",
                "data/24.us-mlsra/2. data checks/Moderators/G2 Parents/42 month parent information/SG42M interview revision FINALIZED 5.19.14.sav",
                "data/24.us-mlsra/2. data checks/Moderators/G3 Children/Second Generation Demographics 3-7-09.sav",
                "data/24.us-mlsra/2. data checks/Moderators/G1 Parents/SEI Caregiver TSEI_42m-16y plus composite 2019.05.30.sav",
                "data/24.us-mlsra/2. data checks/Moderators/G1 Parents/marital status at birth 2019.04.23.sav",
                "data/24.us-mlsra/2. data checks/Moderators/G1 Parents/moms_age_at_birth 2019.04.08.sav",
                "data/24.us-mlsra/2. data checks/Moderators/G1 Parents/birthallvariables8-14-14.sav",
                "data/24.us-mlsra/2. data checks/Moderators/G2 Children/RACE Recoded_Binary Variable 2019.04.12.sav",
                "data/24.us-mlsra/2. data checks/Moderators/G2 Children/birthdates_by_id.sav",
                "data/24.us-mlsra/2. data checks/Moderators/G2 Children/Sex/CSEX.sav")

# If 1 file
#file_paths <- c(paths$load_data)

# Path for writing the raw codebook to
codebookpath <- paste0("data/24.us-mlsra/2. data checks/codebook-raw-24.us-mlsra.csv")
# used to be "data/102.us-lls/2.data-checks/codebook-raw-102.us-lls.csv"

# Folder for writing rename files to
renamepath <- "data/24.us-mlsra/2. data checks"
# used to be "data/102.us-lls/2.data-checks"

###### Run ###### 
### 3. Prepare data for relabelling ####
# Load data file(s) into a list object, the name of each list item is the file path for trackability
# Note that now haven::read_sav() is used. We can also write a function that 
# uses a different read function based on the file extension - yes, good idea. discuss
dfs <- sapply(file_paths, 
              function(path) read_sav(path), 
              simplify = FALSE)

# Make sure that ID's are the same; in this case some are upper and other are lowercase
dfs_lower <- lapply(dfs, make_lowercase)

# Merge all dfs into 1 merged_df
merged_df <- bind_rows(dfs_lower, .id = "source_id")

# If you want to convert the source_id column to a factor, you can do this: discuss
merged_df$source_id <- as.factor(merged_df$source_id)

# Check if merged_df is a tbl_df and otherwise convert to tibble
if ("tbl_df" %in% class(merged_df)) {
  print("It's a tibble!")
} else {
  merged_df <- as_tibble(data.frame(merged_df))
  print("Converted to tibble!")
}

# Make a codebook from the raw dataset and write it to csv
# Note that this takes a long time
data_dictionary <- create_codebook(merged_df, codebookpath)

# Write an empty rename file for this study: one as empty one, and one to be filled
rename_empty <- create_rename_file(data_dictionary, studyname, renamepath)

# Write the merged_df dataframe to an .rda file for later further processing
save(merged_df, file= paste0("data/24.us-mlsra/2. data checks/dataprep-",
                             studyname,
                             ".rda"))