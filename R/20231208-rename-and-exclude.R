# rename and exclude

#### Dependencies ####
library(tidyverse)
library(labelled)

# Make the new names unique if they are the same (appends a number to the name)
# No number is added if NA
study1_config <- rename.basic %>%
  mutate(new_name = ifelse(is.na(new_name), 
                           new_name, make.unique(new_name, sep = "_")))

# recode variables if a new name is provided
# if no new name is provided: exclude
# if no new label is provided: keep label
# if new label is provided: use new label
df.included<-df.basic[,!is.na(study1_config$new_name)]
df.excluded<-df.basic[,is.na(study1_config$new_name)]

# change columns to new names
# ! It would be nice if I can make sure that none of the columns have been changed in order
colnames(df.included)<-study1_config[!is.na(study1_config$new_name),]$new_name
    
# change labels 





