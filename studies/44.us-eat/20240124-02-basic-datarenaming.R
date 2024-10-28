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
studyname <- "44.us-eat"

# load rda file created in step 01
load(paste0("data/24.us-mlsra/2. data checks/dataprep-",
                             studyname,
                             ".rda"))

# add the path to the rename document
renamepath <- paste0("data/", studyname, "/2.data-check/1_rename_", studyname, "_filled.xlsx")

# add a path to the excluded file
excludedpath <- paste0("data/", studyname, "/2.data-check/", studyname, "_exclude.csv")

# Open new_columns csv file and provide values in each column, in row 2
# this is currently not done
# added_values <- read_delim(file = paste0(paths$read_new_columns),
#                           delim = ";")

# Study ID in meta analysis (if present)
study_id_ma <- 44

### 3. Relabel and exclude ####
rename_basic <- read_excel(renamepath)%>%
  make_unique() # make all names unique

# Select excluded variables if no new name is provided
df_excluded <- merged_df[, is.na(rename_basic$new_name)]

# Write the file with excluded variables
write.csv(df_excluded, excludedpath, row.names = FALSE)

# Rename the indicated labels and columns and add new columns
df_included <- merged_df[, !is.na(rename_basic$new_name)] %>%

  # Rename columns according to the rename_basic file
  rename_columns(rename_basic) %>%
  
  # Rename labels according to the rename_basic file
  rename_labels(rename_basic) 
  
# Add new columns provided in added_values
#  new_column(added_values)

### 4. Check if variables from MA are missing ####
meta_outcome <- read_excel("docs/20231221-g2-parenting.xlsx") 
meta_predictor <- read_excel("docs/20231221-g1-parenting.xlsx") 

# Filter both outcomes and predictors on the study ID (should be there according to the MA)
# TO BE ADJUSTED WHERE USEFUL
present_outcomes <- meta_outcome %>%
  filter(S_ID == study_id_ma) %>% 
  select(S_ID, Outcome_name, Outcome_description) %>% # Could also select more if needed...
  mutate(variable_type = "outcome_ma") %>%
  relocate(variable_type, .after = S_ID) %>%
  rename(name = Outcome_name, description = Outcome_description)

present_predictors <- meta_predictor %>%
  filter(S_ID == study_id_ma) %>%
  select(S_ID, Predictor_name, Predictor_description) %>%
  mutate(variable_type = "predictor_ma") %>%
  relocate(variable_type, .after = S_ID) %>%
  rename(name = Predictor_name, description = Predictor_description)

# Select info from data_dictionary (received data)
# TO BE ADJUSTED WHERE USEFUL
in_received_dataset <- rename_basic %>%
  filter(str_detect(new_name, "par") & str_detect(new_name, "g1")) %>%
  select(name, label, new_name) %>%
  mutate(variable_type = "received_data") %>%
  rename(description = label)

compare_ma_with_received <- bind_rows(present_outcomes,
                                      present_predictors,
                                      in_received_dataset)


# Old
meta.outcome[meta.outcome$S_ID==102, ]$Outcome_name
meta.outcome[meta.outcome$S_ID==102, ]$Outcome_description

meta.predictor[meta.predictor$S_ID==102, ]$Predictor_name
meta.predictor[meta.predictor$S_ID==102, ]$Predictor_description

g2parenting <- select(df_included, contains("par") & contains("G2"))
g1parenting <- select(df_included, contains("par") & contains("G1"))

# instead use the rows in rename_basic

# note: request relation_length_4


