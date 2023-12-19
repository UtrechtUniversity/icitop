## Reading in from "config"-like files
# Written by Dorien Huijser
# Last edit 2023-12-08
# With many thanks to ChatGPT :)

#### Dependencies ####
library(tidyverse)


#### Read in the fake config file and fake dataset  ####
study1_config <- read.csv("code-snippets/fake-configs/study1_variables.csv")
study1_data <- read.csv("code-snippets/fake-configs/study1_fakedata.csv")


#### Rename the variables using the config ####
# Make the new names unique if they are the same (appends a number to the name)
study1_config <- study1_config %>%
  mutate(new_name = make.unique(new_name, 
                                sep = "_"))

# Set the names of the to be renamed columns
rename_cols <- setNames(as.character(study1_config$new_name), 
                        as.character(study1_config$orig_name))

# Rename the columns (code thanks to ChatGPT)
study1_data_renamed <- study1_data %>%
  rename_with(~rename_cols[.], .cols = names(rename_cols))


### Delete variables listed in the config ###
study1_renamed_removeddata <- study1_data_renamed %>%
  select(-study1_config$exclude[nzchar(study1_config$exclude)])
