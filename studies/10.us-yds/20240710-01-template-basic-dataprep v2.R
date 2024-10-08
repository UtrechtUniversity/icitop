# Basic data prep
# Last edit 2024-07-10
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
source("R/old/create_codebook.R") # because it doesn't work to include names of datafiles

### 2. Configuration per dataset ####
# Name of the study, to be used in file and folder naming
studyname <- "10.us-yds"

# Path(s) to where the raw data files are that need to be read in and cleaned
file_pathsg2g3 <- c("data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0033/24881-0033-Data.sav", #22
                "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0036/24881-0036-Data.sav", #23
                "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0039/24881-0039-Data.sav", #24
                "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0043/24881-0043-Data.sav", #26
                "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/24881-Zipped_package-Waves1-3_G3/famid_W1 G3.sav", #27
                "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/24881-Zipped_package-Waves1-3_G3/famid_W2 G3.sav", #28
                "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/24881-Zipped_package-Waves1-3_G3/famid_W3 G3.sav" #29
                )

file_pathsg2g3 <- c("data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0001/24881-0001-Data.sav", #1
                    "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0002/24881-0002-Data.sav", #2
                    "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0003/24881-0003-Data.sav", #3
                    "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0004/24881-0004-Data.sav", #4
                    "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0005/24881-0005-Data.sav", #5
                    "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0006/24881-0006-Data.sav", #6
                    "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0015/24881-0015-Data.sav", #7
                    "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0016/24881-0016-Data.sav", #8
                    "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0017/24881-0017-Data.sav", #9
                    "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0018/24881-0018-Data.sav", #10
                    "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0019/24881-0019-Data.sav", #11
                    "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0021/24881-0021-Data.sav", #12
                    "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0022/24881-0022-Data.sav", #13
                    "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0023/24881-0023-Data.sav", #14
                    "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0024/24881-0024-Data.sav", #15
                    "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0025/24881-0025-Data.sav", #16
                    "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0026/24881-0026-Data.sav", #17
                    "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0028/24881-0028-Data.sav", #18
                    "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0029/24881-0029-Data.sav", #19
                    "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0030/24881-0030-Data.sav", #20
                    "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0032/24881-0032-Data.sav", #21
                    "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0033/24881-0033-Data.sav", #22
                    "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0036/24881-0036-Data.sav", #23
                    "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0039/24881-0039-Data.sav", #24
                    "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0042/24881-0042-Data.sav", #25
                    "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0043/24881-0043-Data.sav", #26
                    "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/24881-Zipped_package-Waves1-3_G3/famid_W1 G3.sav", #27
                    "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/24881-Zipped_package-Waves1-3_G3/famid_W2 G3.sav", #28
                    "data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/24881-Zipped_package-Waves1-3_G3/famid_W3 G3.sav" #29
)


# Path for writing the raw codebook to
codebookpath <- paste0("data/10.us-yds/2.data-check/codebook-raw-10.us-yds.xlsx")

# Folder for writing rename files to
renamepath <- "data/10.us-yds/2.data-check"

###### Run ###### 
### 3. Prepare data for relabelling ####
# Load data file(s) into a list object, the name of each list item is the file path for trackability
dfsg2g3 <- sapply(file_pathsg2g3, 
              function(path) read_sav(path), 
              simplify = FALSE)

# Make sure that ID's are the same; in this case some are upper and other are lowercase
dfsg2g3_lower <- lapply(dfsg2g3, make_lowercase)

# when combining datasets: check if all datafiles have the same id
id_check(dfsg2g3_lower, "famid") # 3 datasets don't have famid

# add famid to G3 datasets using the keys in the zipped package

# wave 1
dfsg2g3_lower$`data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0033/24881-0033-Data.sav`<-
  merge(dfsg2g3_lower$`data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0033/24881-0033-Data.sav`,
        dfsg2g3_lower$`data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/24881-Zipped_package-Waves1-3_G3/famid_W1 G3.sav`,
        by = "sgid")

# wave 2
dfsg2g3_lower$`data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0036/24881-0036-Data.sav`<-
  merge(dfsg2g3_lower$`data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0036/24881-0036-Data.sav`,
        dfsg2g3_lower$`data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/24881-Zipped_package-Waves1-3_G3/famid_W2 G3.sav`,
        by = "sgid")

# wave 3
dfsg2g3_lower$`data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0039/24881-0039-Data.sav`<-
  merge(dfsg2g3_lower$`data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/DS0039/24881-0039-Data.sav`,
        dfsg2g3_lower$`data/10.us-yds/2.data-check/ICPSR_24881-V5/ICPSR_24881/24881-Zipped_package-Waves1-3_G3/famid_W3 G3.sav`,
        by = "sgid")

# remove id files
dfsg2g3_lower<-dfsg2g3_lower[-(5:7)]
file_pathsg2g3<-file_pathsg2g3[-(5:7)]


dfsg2g3_lower <- lapply(dfsg2g3_lower, function(dfx) {
  dfx$sgid <- as.character(dfx$sgid)
  return(dfx)
})

dfsg2g3_lower <- lapply(dfsg2g3_lower, function(dfy) {
  dfy$famid <- as.character(dfy$famid)
  return(dfy)
})

g1g2 <- lapply(g1g2, function(dfl) {
  dfl$famid <- as.character(dfl$famid)
  return(dfl)
})

# Merge all dfs into 1 merged_df
merged_g1g2 <- reduce(g1g2, full_join, by = "famid") # check this

merged_g2g3 <- reduce(dfsg2g3_lower, full_join, by = c("sgid")) # this requires some additional checking

# Convert the source_id column to a factor, if neccesary
#merged_df$source_id <- as.factor(merged_df$source_id)

# Check if merged_df is a tbl_df and otherwise convert to tibble
if ("tbl_df" %in% class(merged_g2g3)) {
  print("It's a tibble")
} else {
  merged_df <- as_tibble(data.frame(merged_g2g3))
  print("Converted to tibble")
}

# remove all NA columns
merged_g2g3 <- merged_g2g3[, colSums(is.na(merged_g2g3)) < nrow(merged_g2g3)]


# Make a codebook from the raw dataset and write it to csv
# Note that this takes a long time
data_dictionaryg2g3 <- create_codebook(merged_g2g3, codebookpath)

# Write an empty rename file for this study: one as empty one, and one to be filled
rename_empty_g2g3 <- create_rename_file(data_dictionaryg2g3, studyname, renamepath)

# Write the merged_df dataframe to an .rda file for later further processing
save(merged_df, file= paste0("data/24.us-mlsra/2.data-check/dataprep-",
                             studyname,
                             ".rda"))
