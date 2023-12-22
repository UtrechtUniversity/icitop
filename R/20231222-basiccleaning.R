# Basic data check and cleaning
# Last edit 2023-12-22
###############################
#
# This script performs the following actions:
# 1. Load dependencies
# 2. Configuration per dataset (path to raw data)
# 3. Prepare data for relabelling: 
#   - load raw data
#   - check if tibble (is that necessary?)
#   - create codebook from raw data
#   - initiate empty rename file and write it in the data check folder 
# 4. Relabel and exclude using the sourced rename-and-exclude functions
# 5. Create new columns based on the new_columns config file and using the new_columns custom function
# 6. Check if variables from the meta analysis are missing
#
#
# DH: I see actually 2 scripts in here:
# 1 script that prepares the data for relabelling and writes an empty renaming/excluding configuration file
# 1 script that requires a filled version of the renaming/excluding configuration file
# Right now this script cannot be run at once!
#
# General: either one function per R file, or all functions in 1 R file? Now rename-and-exclude has 3 functions, add-values has 1
#

### 1. Load dependencies ####
# Generic libraries
library(readxl)
library(haven) # for working with SPSS files
library(codebook) # for making codebook
library(tidyverse) # multiple purposes; e.g., read_delim
library(labelled)

# Custom functions
source("R/make_unique.R")
source("R/new_column.R")
source("R/rename_columns.R")
source("R/rename_labels.R")
source("R/create_codebook.R")
source("R/create-rename-file.R")

### 2. Configuration per dataset ####
paths <- read_delim("data/102.us-lls/2.data-checks/paths-102.us-lls.csv")
studyname <- "102.us-lls"

###### Run ###### 
### 3. Prepare data for relabelling ####

# Load SPSS data file(s) 
df.basic <- read_sav(file = paste0(paths$load_data))

# Check if df.basic is a tbl_df and otherwise convert to tibble
# QUESTION DH: What is the reason that we need to check this? Do we need to check this for every study?
if ("tbl_df" %in% class(df.basic)) {
  print("It's a tibble!")
} else {
  df.basic <- as_tibble(data.frame(df.basic))
  print("Converted to tibble!")
}

# Make a codebook from the raw dataset 
# Note that this takes a long time 
data.dictionary <- create_codebook(df.basic,
                                   "data/102.us-lls/2.data-checks/codebook-raw-102.us-lls.csv")

# Write an empty rename file for this study
rename.empty <- create_rename_file(data.dictionary, studyname, "data/102.us-lls/2.data-checks")
# This function writes the rename file to 2 copies of the csv file so that the 
# second to-be-filled one doesn't have to be created manually.

###### Stop ###### 

# COMMENT DH: the 1 and 2 in the data check files are a bit confusing now,
# perhaps it makes more sense to stick to the order that you are using here, so:
# 1_rename_studyname_empty.csv
# 1_rename_studyname_filled.csv
# 2_recode_studyname_empty.csv
# 2_recode_studyname_filled.csv
# 3_ ...
# (it's better to use _ than . in a filename: https://stackoverflow.com/a/72379888)
# If we decide to change this: change in create_rename_file.R as well

### Instruction for filling the rename file (manual work) ####
# - For each variable, determine the new name 
# - New_name for variables that are not needed are left blank
# - Variables that have no label get a label assigned based on additional info provided
# - Comments are added for all variables that require additional info.
#   these comments need to be addressed before step 4
# - Save the file and then go to step 4

### 4. Relabel and exclude ####
rename.basic <- read_delim(file = paste0(paths$read_rename),
                      delim = ",")

rename.basic <- make_unique(rename.basic) # make all names unique

# If no new name is provided: exclude variable
df.included <- df.basic[, !is.na(rename.basic$new_name)]
df.excluded <- df.basic[, is.na(rename.basic$new_name)]

# Write the file with excluded variables
write.csv(df.excluded, 
          paste0(paths$exclude),
          row.names = FALSE)

# Rename the indicated labels and columns
df.included <- rename_columns(df.included, rename.basic)
df.included <- rename_labels(df.included, rename.basic)


### 5. Add info based on new_columns config file ####
# Open new_columns csv file and provide values in each column, in row 2
# QUESTION DH: Why is the new_columns.csv in another folder than the rest of the config files?

# Read in the new_columns config file
added.values <- read_delim(file = paste0(paths$read_new_columns),
                         delim = ";")

# Create new columns based on the new_columns config file
df.included <- new_column(df.included,
                          added.values)

### 6. Check if variables from MA are missing ####
meta.outcome <- read_excel("config/20231221-g2-parenting.xlsx") 
meta.predictor <- read_excel("config/20231221-g1-parenting.xlsx") 

meta.outcome[meta.outcome$S_ID==102, ]$Outcome_name
meta.outcome[meta.outcome$S_ID==102, ]$Outcome_description

meta.predictor[meta.predictor$S_ID==102, ]$Predictor_name
meta.predictor[meta.predictor$S_ID==102, ]$Predictor_description
