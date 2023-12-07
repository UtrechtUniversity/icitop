## Reading in from "config"-like files
# Written by Dorien Huijser
# Last edit 2023-12-07
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

# Identify common columns between data and config
common_columns <- intersect(names(study1_data), 
                            study1_config$orig_name)


#### WIP FROM HERE ####
# Rename common columns in "study1_data" based on the mapping in "study1_config"
study1_renameddata <- study1_data %>%
  rename_with(~ study1_config$new_name[match(., study1_config$orig_name)], 
              cols = common_columns)
# This throws an error

### Delete variables listed in the config ###
# NOT TESTED YET
study1_renamed_removeddata <- study1_renameddata %>%
  select(-study1_config$exclude[nzchar(study1_config$exclude)])
