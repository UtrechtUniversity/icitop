# Basic data renaming
# Last edit 2024-01-24
###############################

### Instruction for filling the rename file (manual work) ####
# - For each variable, determine the new name 
# - New_name for variables that are not needed are left blank
# - Variables that have no label get a label assigned based on additional info provided
# - Comments are added for all variables that require additional info.
#   these comments need to be addressed before executing the current script
# - Save the file, add/check the configuration section, and then execute this script
# - Make sure there is also a file that specifies which new columns need to be added to the dataset

### 1. Load dependencies ####
# Generic libraries
library(readxl)
library(haven) # for working with SPSS files
library(tidyverse) # multiple purposes; e.g., read_delim
library(labelled)

# Custom functions
source("R/make_unique.R")
source("R/rename_columns.R")
source("R/rename_labels.R")
source("R/new_column.R")

### 2. Configuration per dataset ####
studyname <- "102.us-lls"
path<-paste0("data/102.us-lls/2.data-checks/")

# load rda file created in step 01
load("data/102.us-lls/2.data-checks/dataprep-102.us-lls.rda")

# add the path to the rename document
renamepath <- paste0("data/102.us-lls/2.data-checks/1_rename_102.us-lls_filled.xlsx")

# add a path to the excluded file
excludedpath <- paste0("data/102.us-lls/2.data-checks/2_exclude_102.us-lls.xlsx")

# Study ID in meta analysis (if present)
study_id_ma <- 102

### 3. Relabel and exclude ####
rename_basic <- read_excel(renamepath)%>%
  make_unique() # make all names unique

# Select excluded variables if no new name is provided
df_excluded <- rename_basic[is.na(rename_basic$new_name),]
# check document manually- done

# Write the file and codebook with excluded variables
openxlsx::write.xlsx(df_excluded, excludedpath,rowNames = FALSE)

# Rename the indicated labels and columns and add new columns
df_included <- merged_df[, !is.na(rename_basic$new_name)] %>%

  # Rename columns according to the rename_basic file
  rename_columns(rename_basic) %>%
  
  # Rename labels according to the rename_basic file
  rename_labels(rename_basic) 
  
# Add new columns provided in added_values
#  new_column(added_values)

### 4. Check if variables from MA are missing ####
# needs to be added

# bundel notes together to send to PI






