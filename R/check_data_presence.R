
# part of 02-basic-renaming
# For all waves in which parenting variables are measured, 
# is the moderator  also present? 
# result is a dataframe with the waves for each variable
 
## START New edits Dorien on 29th Jan 2024
# With thanks to ChatGPT :)
# Prompt and full answer can be seen here: https://chat.openai.com/share/abdd4db2-284d-442c-af56-93ace2e177fa

# For testing, add some wave and construct info to rename_basic (remove later)
rename_basic$wave[84] <- "w1"
rename_basic$name_construct[84] <- "par"
rename_basic$wave[85] <- "w2"
rename_basic$name_construct[85] <- "par"

# Filter dataframe for constructs of interest
constructs_of_interest <- c("par", 
                            "inc", 
                            "occ", 
                            "edu", 
                            "ses", 
                            "add", 
                            "car", 
                            "sex", 
                            "gen", 
                            "eth", 
                            "age", 
                            "rst", 
                            "rso", 
                            "inv", 
                            "cst", 
                            "ris")

relevant_data <- rename_basic[rename_basic$name_construct %in% constructs_of_interest, ]

# Create a tabular overview of waves per construct > This actually creates a list
waves_per_construct <- tapply(relevant_data$wave, 
                              relevant_data$name_construct, 
                              function(x) unique(x))

# Identify variables not present in the same wave as "par"
par_wave <- waves_per_construct[["par"]]

# Initialize an empty dataframe to store results
missing_variables_df <- data.frame(construct = character(), 
                                   missing_waves = character(), 
                                   stringsAsFactors = FALSE)

for (construct in names(waves_per_construct)) {
  if (construct != "par") {
    other_construct_wave <- waves_per_construct[[construct]]
    missing_variables <- setdiff(par_wave, other_construct_wave)
    
    if (length(missing_variables) > 0) {
      # Print results to console
      cat("Variables not present in the same wave as 'par' for construct", construct, ":", paste(missing_variables, collapse = ", "), "\n")
      
      # And save the results in the dataframe
      missing_variables_df <- rbind(missing_variables_df, data.frame(construct = construct, missing_waves = paste(missing_variables, collapse = ", ")))
    }
  }
}

# Now the missing_variables_df can be saved for later consultation
## END new edits Dorien



## OLD ##
# define g1 variables
names<-c("par", "inc", "occ", "edu", "ses", "add", "car", "sex", "gen", "eth", "age", "rst ", "rso", "inv", "cst", "ris")

# Make an empty data frame
datacheckg1 <- data.frame(matrix(ncol = length(names), nrow = 30))
colnames(datacheckg1) <- names

# for each variable in the rename document, select the g1 data and determine the waves
# 
# Filter and assign data to the 'par' column

for (l in names){
filtered_data <- rename_basic %>%
  filter(str_detect(new_name, l) & str_detect(new_name, "g1")) %>%
  distinct(wave)

# Check if there are any rows in the filtered data
if (nrow(filtered_data) > 0) {
  # Assign the data to the 'par' column in 'datacheckg1'
  datacheckg1[[l]][1:length(filtered_data$wave)] <- filtered_data$wave
} else {
  # If no rows are found, print NA or handle it accordingly
  datacheckg1[[l]] <- NA
}
}

# select rows that have values, delete rows that are all NA
df_filtered <- datacheckg1[rowSums(is.na(datacheckg1)) < ncol(datacheckg1), ]




filtered_data <- rename_basic %>%
  filter(str_detect(new_name, "par") & str_detect(new_name, "g1")) %>%
  distinct(wave)

# Check if there are any rows in the filtered data
if (nrow(filtered_data) > 0) {
    # Assign the data to the 'par' column in 'datacheckg1'
  datacheckg1$par[1:length(filtered_data$wave)] <- filtered_data$wave
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

