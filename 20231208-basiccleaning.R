## Basic data check and cleaning
# Last edit 2023-12-08
###############################
#### Dependencies ####

#library
library(haven) # for working with SPSS files
library(codebook) # for making codebook

# open the paths excel file
# provide all the paths to the files
# save the file under 2.datacheck

#config
paths<-read_delim(file="O:/Research/FSW/Research_data/JG/SanneG/ICITOP IPD_MA/102.us-lls/2.data-checks/paths-102.us-lls.csv", delim=";")

##### 1. prepare data for relabelling 

# Load SPSS data file(s)
df.basic<-read_sav(file=paste0(paths[1,1]))
                     
# Make an excel out of the provided dataset
data.dictionary <- codebook::codebook_table(df.basic) 

# Write first 2 columns data dictionary to csv in 2. data check folder within folder of study
# name file: 1.rename-code+namestudy.csv e.g., rename-102.us-lls.csv
write.csv(data.dictionary[,c(1,2)], 
          paste0(paths[1,2]),
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
rename.basic<-read_delim(file=paste0(paths[1,3]),
                      delim=";")

# use source code to relabel and excluded data
source("O:/Research/FSW/Research_data/JG/SanneG/ICITOP IPD_MA/icitop/scripts/icpsr/20231208-rename-and-exclude.R")

##### 3. add info based on new_columns excel
# open new_columns excel and provide values in each column, in row 2

added.values<-read_delim(file="O:/Research/FSW/Research_data/JG/SanneG/ICITOP IPD_MA/102.us-lls/2.data-checks/ICITOP/new_columns.csv",
                         delim=";")

source("O:/Research/FSW/Research_data/JG/SanneG/ICITOP IPD_MA/icitop/scripts/icpsr/20231208-add-values.R")










