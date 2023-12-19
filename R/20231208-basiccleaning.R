## Basic data check and cleaning
# Last edit 2023-12-19
###############################

source("R/20231208-rename-and-exclude.R")
source("R/20231208-add-values.R")

##### 1. prepare data for relabelling 

# Load SPSS data file(s)
df.basic<-read_sav(file="data/102.us-lls/2.data-checks/ICITOP/Lehigh_ICITOP.sav")
                     
# Make an excel out of the provided dataset - this takes a long time
#data.dictionary <- codebook::codebook_table(df.basic) 

# Write first 2 columns data dictionary to csv in 2. data check folder within folder of study
# name file: 1.rename-code+namestudy.csv e.g., rename-102.us-lls.csv
write.csv(data.dictionary[,c(1,2)], 
          "data/102.us-lls/2.data-checks/1.rename-102.us-lls.csv",
          row.names = FALSE)

# open csv file
# go to data - text to columns - delimeted -comma
# in column c, paste: new_name
# in column d, paste: new_label
# save file with a 2. in front of name: e.g., 2.rename-102.us-lls.csv

# for each variable, determine name (see ...)
# variables that are not needed are left blank
# variables that have no label get a label assigned based on additional info provided

##### 2. relabel and exclude
rename.basic<-read_delim(file="data/102.us-lls/2.data-checks/2.rename-102.us-lls.csv",
                      delim=";")
rename.basic<-make_unique(rename.basic)

# if no new name is provided: exclude
df.included<-df.basic[,!is.na(rename.basic$new_name)]
df.excluded<-df.basic[,is.na(rename.basic$new_name)]

# change columns to new names
df.included<-rename_columns(df.included,rename.basic)
df.included<-rename_labels(df.included,rename.basic)

##### 3. add info based on new_columns excel
# open new_columns excel and provide values in each column, in row 2

added.values<-read_delim(file="data/102.us-lls/2.data-checks/ICITOP/new_columns.csv",
                         delim=";")

df.included<-new_column(df.included,colnames(added.values),added.values[1,])









