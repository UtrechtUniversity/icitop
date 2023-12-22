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
#       > QUESTION DH: Why do we only take the first 2 columns of the data dictionary?
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

# Custom functions
source("R/20231208-rename-and-exclude.R")
source("R/20231208-add-values.R")


### 2. Configuration per dataset ####
paths <- read_delim("data/102.us-lls/2.data-checks/paths-102.us-lls.csv")


###### Run ###### 
### 3. Prepare data for relabelling ####

# Load SPSS data file(s) 
df.basic <- read_sav(file = paste0(paths$load_data))

# Check if df.basic is a tbl_df and otherwise convert to tibble
if ("tbl_df" %in% class(df.basic)) {
  print("It's a tibble!")
} else {
  df.basic <- as_tibble(data.frame(df.basic))
        print("Converted to tibble!")
}

# Make a codebook out from the raw dataset 
# Note that this takes a long time 
data.dictionary <- codebook::codebook_table(df.basic) 

# Remove and add columns
rename.empty <- data.dictionary[, c(1, 2)] %>%
  mutate(
    generation = NA,
    wave = NA,
    reporter = NA,
    target = NA,
    type_instrument = NA,
    name_construct = NA,
    new_name = NA,
    new_label = NA,
    comments = NA
  )					

# Write the first 2 columns data dictionary to csv in 2. data check folder within folder of study
# name file: 1.rename-code+namestudy.csv e.g., rename-102.us-lls.csv
write.csv(rename.empty, 
          paste0(paths$write_rename),
          row.names = FALSE)

###### Stop ###### 

# open csv file
# go to data - text to columns - delimited -comma
# save file with a 2. in front of name: e.g., 2.rename-102.us-lls.csv

# QUESTION DH: Why can we not just write the rename.empty twice with a different name?:
# - Once as "rename-102.us.lls-empty.csv"
# - Once as "rename-102.us.lls-filled.csv" ?
# That would eliminate the step of manually creating a new csv file
#
# ANOTHER COMMENT DH: the 1 and 2 in the data check files are a bit confusing now,
# perhaps it makes more sense to stick to the order that you are using here, so:
# 1_rename_studyname_empty.csv
# 1_rename_studyname_filled.csv
# 2_recode_studyname_empty.csv
# 2_recode_studyname_filled.csv
# 3_ ...
# (it's better to use _ than . in a filename: https://stackoverflow.com/a/72379888)

# for each variable, determine the new name 
# New_name for variables that are not needed are left blank
# variables that have no label get a label assigned based on additional info provided
# comments are added for all variables that require additional info
# these comments need to be addressed before step 4
# save the file and then go to step 4

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
# open new_columns excel and provide values in each column, in row 2

# Read in the new_columns config file
added.values <- read_delim(file = paste0(paths$read_new_columns),
                         delim = ";")
# QUESTION DH: Why is the new_columns.csv in another folder than the rest of the config files?

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
