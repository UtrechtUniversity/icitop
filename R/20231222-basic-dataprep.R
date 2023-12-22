# Basic data prep
# Last edit 2023-12-22
###############################
#
# This script performs the following actions:
# 1. Load dependencies
# 2. Configuration per dataset (path to raw data)
# 3. Merge datasets together (if required)
# 3. Prepare data for relabelling: 
#   - load raw data
#   - check if tibble 
#   - create codebook from raw data

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
studyname <- "24.us-mlsra"

###### Run ###### 
### 3. Prepare data for relabelling ####

# Load SPSS data file(s)
# if multiple 
df1<-read_sav("data\24.us-mlsra\2. data checks\2nd Gen Children Data\Maternal Sensitivity/maternal Sensitivity 2020.06.24.sav")
df2<-read_sav("data\24.us-mlsra\2. data checks\2nd Gen Children Data\Caldwell HOME\1st grade/GR1Caldwell_HOME_revised_2019.06.24.sav")
df3<-read_sav("data\24.us-mlsra\2. data checks\2nd Gen Children Data\Caldwell HOME\30 months/HOME30.sav")
df4<-read_sav("data\24.us-mlsra\2. data checks\2nd Gen Parents Data - 3rd Gen Children\12 month/SG12M Parent Interview 5.19.14 - FINALIZED.sav")
df5<-read_sav("data\24.us-mlsra\2. data checks\2nd Gen Parents Data - 3rd Gen Children\24 month/sg24m parent ratings FINALIZED 5.19.14.sav")
df6<-read_sav("data\24.us-mlsra\2. data checks\2nd Gen Parents Data - 3rd Gen Children\24 month/SG24m Parent Interview FINALIZED 5.19.14.sav")
df7<-read_sav("data\24.us-mlsra\2. data checks\2nd Gen Parents Data - 3rd Gen Children\42 month/SG42M interview revision FINALIZED 5.19.14.sav")
df8<-read_sav("data\24.us-mlsra\2. data checks\2nd Gen Parents Data - 3rd Gen Children\42 month/SG42M Parenting Ratings - FINALIZED 5.19.14.sav")
df9<-read_sav("data\24.us-mlsra\2. data checks\Moderators\G2 Parents\12 month parent information/SG12M Parent Interview 5.19.14 - FINALIZED.sav")
df10<-read_sav("data\24.us-mlsra\2. data checks\Moderators\G2 Parents\24 month parent information/SG24m Parent Interview FINALIZED 5.19.14.sav")
df11<-read_sav("data\24.us-mlsra\2. data checks\Moderators\G2 Parents\42 month parent information/SG42M interview revision FINALIZED 5.19.14.sav")
df12<-read_sav("data\24.us-mlsra\2. data checks\Moderators\G3 Children/Second Generation Demographics 3-7-09.sav")
df13<-read_sav("data\24.us-mlsra\2. data checks\Moderators\G1 Parents/SEI Caregiver TSEI_42m-16y plus composite 2019.05.30.sav")
df14<-read_sav("data\24.us-mlsra\2. data checks\Moderators\G1 Parents/marital status at birth 2019.04.23.sav")
df15<-read_sav("data\24.us-mlsra\2. data checks\Moderators\G1 Parents/moms_age_at_birth 2019.04.08.sav")
df16<-read_sav("data\24.us-mlsra\2. data checks\Moderators\G1 Parents/birthallvariables8-14-14.sav")
df17<-("data\24.us-mlsra\2. data checks\Moderators\G2 Children/RACE Recoded_Binary Variable 2019.04.12.sav")
df18<-("data\24.us-mlsra\2. data checks\Moderators\G2 Children/birthdates_by_id.sav")
df19<-("data\24.us-mlsra\2. data checks\Moderators\G2 Children/Sex/CSEX.sav")


# If 1
df.basic <- read_sav(file = paste0(paths$load_data))

# Check if df.basic is a tbl_df and otherwise convert to tibble
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