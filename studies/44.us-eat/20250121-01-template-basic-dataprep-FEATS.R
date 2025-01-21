# Basic data prep
# Last edit 2024-11-05
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

# 4. excel file with the age of participants when the EAT 2017/2018 survey was completed, as well as the date  
file_paths_4<-("data/44.us-eat/1.raw-data/KidsEAT Age.xlsx")

# 5. excel file with date at which KIDS eat was administered. Use file 4 and 5 to calculate age
file_paths_5<-("data/44.us-eat/1.raw-data/KidsEAT Survey Date.xlsx")

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
dfs_4<-read.xlsx(file_paths_4)
dfs_5<-read.xlsx(file_paths_5)

# Load csv files
dfs_2 <- read.csv(file_paths_2)
dfs_3<-read.csv(file_paths_3,sep=";")

# check NA rows for df_3
# the Kids EAT! study has 310 rows, but many are NA 
dfs_3 <- dfs_3[!apply(dfs_3[,11:110], 1, function(x) all(is.na(x))), ]

# the name of each list item is the sheet name for trackability
names(dfs_1) <- paste0(file_paths_1, " sheet ", sheet_names)

# combine dfs_2 and dfs_3 with key
colnames(dfs_2)[colnames(dfs_2) == "Record.Id."] <- "record_id"
colnames(dfs_3)[colnames(dfs_3) == "record_id"] <- "record_id"
colnames(dfs_2)[colnames(dfs_2) == "participant_code"] <- "id"

# See which record id's are missing
only_in_df2 <- setdiff(dfs_2$record_id, dfs_3$record_id) # 314 is all NA
only_in_df3 <- setdiff(dfs_3$record_id, dfs_2$record_id)
filtered_df <- dfs_3[dfs_3$record_id %in% only_in_df3, ]

# duplicate id's
dfs_6<-full_join(dfs_2, dfs_3, by ="record_id" )

dfs <- c(dfs_1, list(subset_dfs_4 <- dfs_4[, c("id", "enddate_18")]), list(dfs_5),list(dfs_6))

names(dfs)[4] <- paste0(file_paths_4)
names(dfs)[5] <- paste0(file_paths_5)
names(dfs)[6] <- paste0(file_paths_2, " and ", file_paths_3)

dfs_lower <- lapply(dfs, make_lowercase)

# when combining datasets: check if all datafiles have the same id
id_check(dfs_lower, "id")

# make sure all id's are of compatible types (factor)
dfs_lower <- lapply(dfs_lower, function(df) {
  df$id <- as.character(df$id)
  return(df)
})

# Merge all dfs into 1 merged_df
 merged_df <- reduce(dfs_lower, full_join, by = "id")
 
 # check if datafiles have duplicates
 find_duplicates <- function(df) {
   df %>%
     filter(duplicated(id) | duplicated(id, fromLast = TRUE))  # Get all duplicates (including the first occurrence)
 }
 
duplicates_in_dfs <- lapply(dfs, find_duplicates)

# there are duplicates due to pid - ask what this variable means

# Convert the source_id column to a factor, if neccesary
# merged_df$id <- as.factor(merged_df$id)

# Check if merged_df is a tbl_df and otherwise convert to tibble
if ("tbl_df" %in% class(merged_df)) {
  print("It's a tibble")
} else {
  merged_df <- as_tibble(data.frame(merged_df))
  print("Converted to tibble")
}
 
# search for duplicates in id
 all_duplicates <- merged_df[duplicated(merged_df$id) | duplicated(merged_df$id, fromLast = TRUE), ]
 
 df_summary <- merged_df %>%
   group_by(id) %>%
   summarize(across(everything(), ~n_distinct(.)))
 
 x<-as.data.frame(describe(df_summary))
 
 df_differences <- df_summary %>%
   group_by(id) %>%
   filter(n() > 1) %>%
   distinct()
 
 print(df_differences)
  
 
# create age variable for KID Eat with parent_survey_data and 

# put dates in correct format 
merged_df$parent_survey_date <- as.Date(merged_df$parent_survey_date, origin = "1899-12-30")
merged_df$enddate_18[merged_df$enddate_18 == "."] <- NA
merged_df$enddate_18 <- as.Date(as.numeric(merged_df$enddate_18), origin = "1899-12-30")

# calculate age
merged_df$datedif<-merged_df$parent_survey_date - merged_df$enddate_18 # difference between dates in days
merged_df$age_kideat<-merged_df$ageyrs_18+as.numeric((merged_df$datedif/365.25)) # add number of days to age at previous assessment
describe(merged_df$age_kideat)
table(merged_df$parent_survey_date, merged_df$age_kideat, useNA = "ifany") 
describe(as.numeric(merged_df$datedif))
describe(as.numeric(merged_df$enddate_18 ))

# Make a codebook from the raw dataset and write it to csv
# Note that this takes a long time
# all characterers are not displayed properly in codebook. Transfer to numeric for codebook
merged_df_codebook<-lapply(merged_df, as.numeric)
data_dictionary <- create_codebook(dfs_1, merged_df, codebookpath)

# Write an empty rename file for this study: one as empty one, and one to be filled
rename_empty <- create_rename_file(data_dictionary, studyname, renamepath)

# Write the merged_df dataframe to an .rda file for later further processing
save(merged_df, file= paste0("data/44.us-eat/2.data-check/dataprep-",
                             studyname,
                             ".rda"))

# Note: various variables are missing - see yellow marked variables in datadictionairy childfeeding study
# or sex of all children?
