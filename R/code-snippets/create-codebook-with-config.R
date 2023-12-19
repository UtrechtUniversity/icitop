## Creating a codebook with old and new variable name and label
# Written by Dorien Huijser
# Last edit 2023-12-07
# With many thanks to ChatGPT :)

#### Dependencies ####
library(tidyverse)

#### Create fake dataset ####
# Load config
study1_config <- read.csv("code-snippets/fake-configs/study1_variables.csv")

# Create fake dataset
fake_dataset <- df %>% # df was still loaded in the environment
  select(study1_config$orig_name, 
         study1_config$exclude[nzchar(study1_config$exclude)]) %>% # retrieve the study1config$orig_name
  slice_sample(n = 10, replace = FALSE) # Select 10 random rows

# Write data to csv
write.csv(fake_dataset, 
          "code-snippets/fake-configs/study1_fakedata.csv",
          row.names = FALSE)

#### Create codebook from the fake dataset ####
# Grab the labels from the fake dataset
labels_dataset <- sapply(names(fake_dataset), function(x){
  attr(fake_dataset[[x]], "label")
})

# Turn the vector into a dataframe
names_labels <- data.frame(keyName=names(labels_dataset), 
                           value=labels_dataset, 
                           row.names=NULL)

# Name the dataframe columns
names(names_labels) <- c("orig_name", "label")

# Merge with the config file to also include the new variable name
codebook <- full_join(names_labels, 
                      study1_config, 
                      by = "orig_name") %>% 
  select(-exclude)

# Write codebook to csv
write.csv(codebook, "code-snippets/fake-configs/codebook.csv")
