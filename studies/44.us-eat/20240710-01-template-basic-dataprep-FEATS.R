# Basic data prep
# Last edit 2024-10-08
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
source("R/2024_10_11_create_codebook_concept.R")
source("R/create_rename_file.R")
source("R/make_lowercase.R")
source("R/same_id.R")

### 2. Configuration per dataset ####
# Name of the study, to be used in file and folder naming
studyname <- "44.us-eat-study-2"

# Path(s) to where the raw data files are that need to be read in and cleaned
# EAT exists out of 2 studies. Make seperate data renaming

# Data is located on 3 locations
# 1. excel file with three tabs, with G1 data
file_paths_1 <-("data/44.us-eat/1.raw-data/ICITOP EAT Files/ICITOP EAT Files/EAT 2010 Variables for ICITOP.xlsx")

# 2. csv file with key to combine G1 and G2 data - NOTE. the n keys is not sufficient for N data. Check with PI's
file_paths_2<-("data/44.us-eat/1.raw-data/ChildFeedingStudy-DataFileForSanne_DATA_LABELS_2024-09-10_1419.csv")

# 3.  csv file with G2 data (has a different structure than 2 and can't be loaded together in a list)
file_paths_3<-("data/44.us-eat/1.raw-data/Data Share - Kids EAT!/Data Share - Kids EAT!/cfs_survey_deidentified_06072024.csv")
  
# get sheet names
sheet_names<- getSheetNames(file_paths_1)

# Path for writing the raw codebook to
codebookpath <- paste0("data/44.us-eat/2.data-check/codebook-raw-44.us-eat-2.xlsx")

# Folder for writing rename files to
renamepath <- "data/44.us-eat/2.data-check"

###### Run ###### 
### 3. Prepare data for relabelling ####
# Load excel data file(s) into a list object
dfs_1 <- lapply(sheet_names, 
              function(sheet) read.xlsx(file_paths_1, sheet = sheet))

# Load csv files
dfs_2 <- read.csv(file_paths_2)
dfs_3<-read.csv(file_paths_3,sep=";")

# the name of each list item is the sheet name for trackability
names(dfs_1) <- paste0(file_paths_1, " sheet ", sheet_names)

# combine dfs_2 and dfs_3 with key
colnames(dfs_2)[colnames(dfs_2) == "Record.Id."] <- "id"
colnames(dfs_3)[colnames(dfs_3) == "record_id"] <- "id"
dfs_4<-full_join(dfs_2, dfs_3, by ="id" )

# Make sure that ID's are the same; in this case some are upper and other are lowercase
dfs_1 <- c(dfs_1, list(dfs_4))
names(dfs_1)[4] <- paste0(file_paths_2, " and ", file_paths_3)

dfs_lower <- lapply(dfs_1, make_lowercase)

# when combining datasets: check if all datafiles have the same id
id_check(dfs_lower, "id")

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
# all characterers are not displayed properly in codebook. Transfer to numeric for codebook
merged_df_codebook<-lapply(merged_df, as.numeric)
data_dictionary <- create_codebook_concept(dfs_1, merged_df, codebookpath)

# Write an empty rename file for this study: one as empty one, and one to be filled
rename_empty <- create_rename_file(data_dictionary, studyname, renamepath)

# Write the merged_df dataframe to an .rda file for later further processing
save(merged_df, file= paste0("data/24.us-mlsra/2. data checks/dataprep-",
                             studyname,
                             ".rda"))
