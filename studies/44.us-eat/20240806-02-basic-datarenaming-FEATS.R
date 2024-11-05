# Basic data renaming
# Last edit 2024-10-28
###############################

### Instruction for filling the rename file (manual work) ####
# - For each variable, determine the new name 
# - New_name for variables that are not needed are left blank
# - Variables that have no label get a label assigned based on additional info provided
# - Comments are added for all variables that require additional info.
# _ Save the file, add/check the configuration section, and then execute this script
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
source("R/rename_labels_concept.R")
source("R/new_column.R")

### 2. Configuration per dataset ####
studyname0<-"44.us-eat" # because 2 studies in 1 folder
studyname <- "44.us-eat-study-2"

# Study ID in meta analysis (if present)
# not present in MA

#####################

# load rda file created in step 01
load(paste0("data/", studyname0, "/2.data-check/dataprep-",
            studyname,
            ".rda"))

# add the path to the rename document
renamepath <- paste0("data/", studyname0, "/2.data-check/1_rename_", studyname, "_filled.xlsx")

# add a path to the excluded file
excludedpath <- paste0("data/", studyname0, "/2.data-check/", studyname, "_exclude.csv")

## add a path for checking data availability
path<-paste0("data/", studyname0, "/2.data-check/")

# Open new_columns csv file and provide values in each column, in row 2
# this is currently not done
# added_values <- read_delim(file = paste0(paths$read_new_columns),
#                           delim = ";")

### 3. Relabel and exclude ####
rename_basic <- read_excel(renamepath)%>%
  make_unique() # make all names unique

# Select excluded variables if no new name is provided
df_excluded <- merged_df[, is.na(rename_basic$new_name)]

# Write the file with excluded variables
write.csv(df_excluded, excludedpath, row.names = FALSE)

# if needed: change all missing values to NA
merged_df<-merged_df%>%
mutate_all(~ na_if(., "."))

# Rename the indicated labels and columns and add new columns
df_included <- merged_df[, !is.na(rename_basic$new_name)] %>%

# Rename columns according to the rename_basic file
  rename_columns(rename_basic) %>%
  
  # Rename labels according to the rename_basic file
  rename_labels(rename_basic) 

# Add new columns provided in added_values
#  new_column(added_values)

### 4. Check if variables from MA are missing - not applicable  ####

### 5. check data presence ####
source("R/check_data_presence.R")




