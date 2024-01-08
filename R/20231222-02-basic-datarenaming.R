# Basic data renaming
# Last edit 2024-01-08
###############################

# (it's better to use _ than . in a filename: https://stackoverflow.com/a/72379888)
# TO DO SANNE: CHANGE THIS

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
paths <- read_delim("data/102.us-lls/2.data-checks/paths-102.us-lls.csv")

renamepath <- paste0(paths$read_rename)
rawdata <- read_sav(paste0(paths$load_data)) # Perhaps just re-run the code from basic-dataprep?
excludedpath <- paste0(paths$exclude)

# Open new_columns csv file and provide values in each column, in row 2
added_values <- read_delim(file = paste0(paths$read_new_columns),
                           delim = ";")


### 3. Relabel and exclude ####
rename_basic <- read_delim(file = renamepath,
                           delim = ";") %>%
  make_unique() # make all names unique

# Select excluded variables if no new name is provided
df_excluded <- rawdata[, is.na(rename_basic$new_name)]

# Write the file with excluded variables
write.csv(df_excluded, excludedpath, row.names = FALSE)

# Rename the indicated labels and columns
df_included <- rawdata[, !is.na(rename_basic$new_name)] %>%
  # Rename columns according to the rename_basic file
  rename_columns(rename_basic) %>%
  
  # Rename labels according to the rename_basic file
  rename_labels(rename_basic) %>%
  
  # Add new columns provided in added_values
  new_column(added_values)


### 4. Check if variables from MA are missing ####
meta.outcome <- read_excel("config/20231221-g2-parenting.xlsx") 
meta.predictor <- read_excel("config/20231221-g1-parenting.xlsx") 

meta.outcome[meta.outcome$S_ID==102, ]$Outcome_name
meta.outcome[meta.outcome$S_ID==102, ]$Outcome_description

meta.predictor[meta.predictor$S_ID==102, ]$Predictor_name
meta.predictor[meta.predictor$S_ID==102, ]$Predictor_description

g2parenting <- select(df_included, contains("par") & contains("G2"))
g1parenting <- select(df_included, contains("par") & contains("G1"))

# instead use the rows in rename_basic
