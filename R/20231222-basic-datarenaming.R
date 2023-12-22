# Basic data renaming
# Last edit 2023-12-22
###############################

# (it's better to use _ than . in a filename: https://stackoverflow.com/a/72379888)
# TO DO SANNE: CHANGE THIS

### Instruction for filling the rename file (manual work) ####
# - For each variable, determine the new name 
# - New_name for variables that are not needed are left blank
# - Variables that have no label get a label assigned based on additional info provided
# - Comments are added for all variables that require additional info.
#   these comments need to be addressed before step 4
# - Save the file and then go to step 4

# Custom functions
source("R/make_unique.R")
source("R/rename_columns.R")
source("R/rename_labels.R")
source("R/new_column.R")

### 2. Configuration per dataset ####
paths <- read_delim("data/102.us-lls/2.data-checks/paths-102.us-lls.csv")
df.basic <- read_sav(file = paste0(paths$load_data))
studyname <- "102.us-lls"

### 3. Relabel and exclude ####
rename.basic <- read_delim(file = paste0(paths$read_rename),
                           delim = ";")

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

# Read in the new_columns config file
added.values <- read_delim(file = paste0(paths$read_new_columns),
                           delim = ";")

# Create new columns based on the new_columns config file
df.included <- new_column(df.included,
                          added.values)

### 6. Check if variables from MA are missing ####

meta.outcome <- read_excel("config/20231221-g2-parenting.xlsx") 
meta.predictor <- read_excel("config/20231221-g1-parenting.xlsx") 

meta.outcome[meta.outcome$S_ID==102, ]$outcome_name
meta.outcome[meta.outcome$S_ID==102, ]$outcome_description

meta.predictor[meta.predictor$S_ID==102, ]$predictor_name
meta.predictor[meta.predictor$S_ID==102, ]$predictor_description

g2parenting <- select(df.included, contains("par") & contains("G2"))
g1parenting <- select(df.included, contains("par") & contains("G1"))

# instead use the rows in rename.basic
