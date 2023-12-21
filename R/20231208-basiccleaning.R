# Basic data check and cleaning
# Last edit 2023-12-20
###############################

source("R/20231208-rename-and-exclude.R")
source("R/20231208-add-values.R")

library(readxl)

###### 1 prepare data for relabelling ###### 

# Load SPSS data file(s) 
df.basic<-read_sav(file="data/102.us-lls/2.data-checks/ICITOP/Lehigh_ICITOP.sav")

# check if df.basic is a tbl_df and otherwise convert to tibble
if ("tbl_df" %in% class(df.basic)) {
  print("It's a tibble!")
} else {
  df.basic<-as_tibble(data.frame(df.basic))
        print ("Converted to tibble!")
}

# Make an excel out of the provided dataset - Note that this takes a long time 
data.dictionary <- codebook::codebook_table(df.basic) 

# Write the first 2 columns data dictionary to csv in 2. data check folder within folder of study
# name file: 1.rename-code+namestudy.csv e.g., rename-102.us-lls.csv
write.csv(data.dictionary[,c(1,2)], 
          "data/102.us-lls/2.data-checks/1.rename-102.us-lls.csv",
          row.names = FALSE)

# open csv file
# go to data - text to columns - delimeted -comma
# open ~\docs\columnnames.xlsx
# copy the columnnames in the rename file. 
# save file with a 2. in front of name: e.g., 2.rename-102.us-lls.csv

# for each variable, determine the new name 
# New_name for variables that are not needed are left blank
# variables that have no label get a label assigned based on additional info provided
# comments are added for all variables that require additional info
# these comments need to be addressed before step 2

###### 2. relabel and exclude ###### 
rename.basic<-read_delim(file="data/102.us-lls/2.data-checks/2.rename-102.us-lls.csv",
                      delim=";")

rename.basic<-make_unique(rename.basic) # make all names unique

# if no new name is provided: exclude variable
df.included<-df.basic[,!is.na(rename.basic$new_name)]
df.excluded<-df.basic[,is.na(rename.basic$new_name)]

write.csv(df.excluded, 
          "data/102.us-lls/2.data-checks/102.us-lls-excluded.csv",
          row.names = FALSE)

# change with to new names and labels
df.included<-rename_columns(df.included,rename.basic)
df.included<-rename_labels(df.included,rename.basic)

###### 3. add info based on new_columns excel ###### 
# open new_columns excel and provide values in each column, in row 2

added.values<-read_delim(file="data/102.us-lls/2.data-checks/ICITOP/new_columns.csv",
                         delim=";")

df.included<-new_column(df.included,colnames(added.values),added.values[1,])

###### 4. check if variables from MA are missing ###### 
meta.outcome<-read_excel("config/20231221-g2-parenting.xlsx") 
meta.predictor<-read_excel("config/20231221-g1-parenting.xlsx") 

meta.outcome[meta.outcome$S_ID==102,]$Outcome_name
meta.outcome[meta.outcome$S_ID==102,]$Outcome_description

meta.predictor[meta.predictor$S_ID==102,]$Predictor_name
meta.predictor[meta.predictor$S_ID==102,]$Predictor_description
