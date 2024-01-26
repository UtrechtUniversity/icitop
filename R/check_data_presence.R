
# part of 02-basic-renaming
# For all waves in which parenting variables are measured, 
# is the moderator  also present? 
# result is a dataframe with the waves for each variable
 
# define g1 variables
names<-c("par", "inc", "occ", "edu", "ses", "add", "car", "sex", "gen", "eth", "age", "rst ", "rso", "inv", "cst", "ris")

# Make an empty data frame
datacheckg1 <- data.frame(matrix(ncol = length(g1moderators), nrow = 0))
colnames(datacheckg1) <- g1moderators

# for each variable in the rename document, select the g1 data and determine the waves
# 
# Filter and assign data to the 'par' column
filtered_data <- rename_basic %>%
  filter(str_detect(new_name, "par") & str_detect(new_name, "g1")) %>%
  distinct(wave)

# Check if there are any rows in the filtered data
if (nrow(filtered_data) > 0) {
    # Assign the data to the 'par' column in 'datacheckg1'
  datacheckg1$par[1:length(filtered_data)] <- filtered_data$wave
} else {
  # If no rows are found, print NA or handle it accordingly
  datacheckg1$par <- NA
}

##
# Filter and assign data to the 'inc' column
filtered_data <- rename_basic %>%
  filter(str_detect(new_name, "inc") & str_detect(new_name, "g1")) %>%
  distinct(wave)

# Check if there are any rows in the filtered data
if (nrow(filtered_data) > 0) {
  # Assign the data to the 'inc' column in 'datacheckg1'
  datacheckg1$inc[1:length(filtered_data)] <- filtered_data$wave
} else {
  # If no rows are found, print NA or handle it accordingly
  datacheckg1$inc <- NA
}

##
# Filter and assign data to the 'occ' column
filtered_data <- rename_basic %>%
  filter(str_detect(new_name, "occ") & str_detect(new_name, "g1")) %>%
  distinct(wave)

# Check if there are any rows in the filtered data
if (nrow(filtered_data) > 0) {
  # Assign the data to the 'occ' column in 'datacheckg1'
  datacheckg1$occ[1:length(filtered_data)] <- filtered_data$wave
} else {
  # If no rows are found, print NA or handle it accordingly
  datacheckg1$occ <- NA
}

# repeat for all G1 moderators
# make this a loop




# OLD attemps 

# Filter and assign data to the 'inc' column
filtered_data <- rename_basic %>%
  filter(str_detect(new_name, "occ") & str_detect(new_name, "g1")) %>%
  distinct(wave)

# Check if there are any rows in the filtered data
if (nrow(filtered_data) > 0) {
  # Get the necessary number of rows from 'filtered_data'
  rows_needed <- min(nrow(filtered_data), nrow(datacheckg1))
  filtered_data <- filtered_data[1:rows_needed, ]
  
  # Assign the data to the 'inc' column in 'datacheckg1'
  datacheckg1[1:rows_needed, "inc"] <- filtered_data$wave
} else {
  # If no rows are found, print NA or handle it accordingly
  datacheckg1$par <- NA
}





# Loop through each moderator
for (l in seq_along(g1moderators)) {
  # Generate the new variable name
  new_var_name <- paste0(g1moderators[l])
  # Filter and process data
  filtered_data <- rename_basic %>%
    filter(str_detect(new_name, paste0(g1moderators[l])) & str_detect(new_name, "g1")) %>%
    distinct(wave)
  
  # Check if there are any rows in the filtered data
  if (nrow(filtered_data) > 0) {
    # Mutate and assign data
    datacheckg1[[new_var_name]] <- filtered_data %>%
      mutate(wave = gsub("w", "", wave))
  } else {
    # Print NA if no rows are found
    print(NA)
  }
}












for (l in seq_along(g1moderators))
{
# Generate the new variable name
  new_var_name <- paste0(g1moderators[l])  

  # Filter and process data
  datacheckg1[[new_var_name]] <- rename_basic %>%
    filter(str_detect(new_name, paste0(g1moderators[l])) & str_detect(new_name, "g1")) %>%
    distinct(wave) %>%
    mutate(wave = gsub("w", "", wave))
  
  # Print NA if no rows are found
  if (nrow(datacheckg1[[new_var_name]]) == 0) {
    print(NA)
  }
}
  
  
  
  
  
  
    
  datacheckg1[[paste0(g1moderators[l])]]<-rename_basic %>%
    filter(str_detect(new_name, paste0(g1moderators[l]) & str_detect(new_name, "g1")) %>%
    distinct(wave) %>%
    {if (nrow(.) > 0) {
      mutate(., wave = gsub("w", "", wave))
    } else {
      print(NA)
    }
    }
 
}

datacheckg1$paste0 <- paste0(datacheckg1$g1moderators[l])


#g1 moderators
datacheckg1$inc<-rename_basic %>%
  filter(str_detect(new_name, "inc") & str_detect(new_name, "g1")) %>%
  distinct(wave) %>%
  {if (nrow(.) > 0) {
    mutate(., wave = gsub("w", "", wave))
  } else {
    print(NA)
  }
  }

datacheckg1$occ<-rename_basic %>%
  filter(str_detect(new_name, "occ") & str_detect(new_name, "g1")) %>%
  distinct(wave) %>%
  {if (nrow(.) > 0) {
    mutate(., wave = gsub("w", "", wave))
  } else {
    print(NA)
  }
  }

datacheckg1$edu<-rename_basic %>%
  filter(str_detect(new_name, "edu") & str_detect(new_name, "g1")) %>%
  distinct(wave) %>%
  {if (nrow(.) > 0) {
    mutate(., wave = gsub("w", "", wave))
  } else {
    print(NA)
  }
  }







# how many waves of G2 parenting?
rename_basic %>%
  filter(str_detect(new_name, "par") & str_detect(new_name, "g2")) %>%
  distinct(wave)

