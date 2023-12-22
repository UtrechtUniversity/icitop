# rename and exclude
# S. Geeraerts 19-12-2023
#### Dependencies ####

#library
library(haven) # for working with SPSS files
library(codebook) # for making codebook
library(tidyverse) # multiple purposes; e.g., read_delim
library(labelled)

# Make the new names unique if they are the same (appends a number to the name)
# No number is added if NA
# ChatGPT:
#I added a check to ensure that the input is a data frame.
#I used the mutate function assuming that file is a data frame.
#I converted new_name to a character before applying make.unique to handle cases where it might be a factor or other data type.
#I replaced return(file) with file at the end, which is idiomatic in R. The last evaluated expression is automatically returned.

make_unique <- function(df) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
      }
  
  df <- df %>%
    mutate(new_name = ifelse(!is.na(new_name), make.unique(as.character(new_name), sep = "_"), NA))
  
  return(df)
}

# rename function 
rename_columns <- function(df, rename_data) {
  for (i in 1:length(colnames(df))) {
    matching_row <- rename_data[rename_data$name %in% colnames(df)[i], ]
    
    if (nrow(matching_row) > 0 && !is.na(matching_row$new_name)) {
      colnames(df)[i] <- matching_row$new_name
    }
  }
  return(df)
}

# change labels 
rename_labels <- function(df, rename_data) {
  for (i in 1:length(colnames(df))) {
    matching_row <- rename_data[rename_data$name %in% colnames(df)[i], ]
    
    if (nrow(matching_row) > 0 && !is.na(matching_row$new_label)) {
      attr(df[[colnames(df)[i]]], "label") <- matching_row$new_label
    }
  }
  return(df)
}




