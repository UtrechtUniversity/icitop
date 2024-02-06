# part of 02-basic-renaming
# For all waves in which parenting variables are measured, 
# is the moderator  also present? 
# result is a dataframe with the waves for each variable
 
## START New edits Dorien on 29th Jan 2024
# With thanks to ChatGPT :)
# Prompt and full answer can be seen here: https://chat.openai.com/share/abdd4db2-284d-442c-af56-93ace2e177fa

# Note Dorien: As most of this code has to be repeated 3 times (g1, g2, g3), 
# we could create a function instead and apply it on all 3 generations?

# For testing, add some wave and construct info to rename_basic (remove later)
rename_basic$wave[84:88] <- c("w1", "w2", "v1", "v2", "v2")
rename_basic$name_construct[84:88] <- c("par", "par", "par", "par", "edu")
rename_basic$generation[84:88] <- c("g1", "g1", "g2", "g2", "g2")
rename_basic$target[84:86] <- c("f", "m", "c2")

# Filter dataframe for constructs of interest G1
# I made an excel sheet with information about (a selection of) the moderators and par 
# I made a seperate one for G1 par and G2 par, but script below is only about g1 par

constructs_of_interest_g1 <-read_xlsx("docs/g1-moderators.xlsx")

# THIS IS THE PART WE HAVE BEEN TALKING ABOUT
# variables should be selected when they match the combined information in this sheet
# (I now ignored target; not sure yet if I need it)
# rows should only be selected if they meet ALL criteria

relevant_data_g1 <- inner_join(constructs_of_interest_g1, rename_basic, by = c("generation", "name_construct"))

# this gives a warning, but it does seem to do what I want
# the variable in cell 88 (g2 edu - which is only relevant for G2 par) is ignored, but g1 edu (which IS relevant for g1 par) is included. 

# Create a tabular overview of waves per construct > This actually creates a list. The rest below hasn't been changed
waves_per_construct_g1 <- tapply(relevant_data_g1$wave, 
                                 relevant_data_g1$name_construct, 
                                 function(x) unique(x))

par_wave_g1 <- waves_per_construct_g1[["par"]]

missing_variables_df <- data.frame(construct = character(), 
                                   missing_waves = character(), 
                                   stringsAsFactors = FALSE)

for (construct in names(waves_per_construct_g1)) {
  if (construct != "par") {
    other_construct_wave <- waves_per_construct_g1[[construct]]
    missing_variables <- setdiff(par_wave_g1, other_construct_wave)
    
    if (length(missing_variables) > 0) {
      # Print results to console
      cat("Variables not present in the same wave as 'par' for construct", construct, ":", paste(missing_variables, collapse = ", "), "\n")
      
      # And save the results in the dataframe
      missing_variables_df <- rbind(missing_variables_df, data.frame(construct = construct, missing_waves = paste(missing_variables, collapse = ", ")))
    }
  }
} 



#### OLD



# Filter dataframe for constructs of interest G2
constructs_of_interest_g2 <- c("eth", 
                            "aab", 
                            "sex", 
                            "gen", 
                            "bir", 
                            "cst", 
                            "ris", 
                            "age", 
                            "rst", 
                            "car", 
                            "etp", 
                            "res",
                            "req",
                            "vinc",
                            "occ",
                            "edu",
                            "ses",
                            "alc",
                            "dru",
                            "age",
                            "inv",
                            "sub"
)

# Filter dataframe for constructs of interest G3
constructs_of_interest_g3 <- c("nch",
                            "bir",
                            "age",
                            "sex",
                            "gen"
                       )

# TODO: filter on "g1", "g2", "g3"?
relevant_data_g1 <- rename_basic[rename_basic$name_construct %in% constructs_of_interest_g1, ]
relevant_data_g2 <- rename_basic[rename_basic$name_construct %in% constructs_of_interest_g2, ]
relevant_data_g3 <- rename_basic[rename_basic$name_construct %in% constructs_of_interest_g3, ]

# Alternative
relevant_data_g1 <- rename_basic %>%
  filter(grepl("g1", new_name, 
               ignore.case = TRUE) & name_construct %in% constructs_of_interest_g1)
# Remove if unnecessary


# Create a tabular overview of waves per construct > This actually creates a list
waves_per_construct_g1 <- tapply(relevant_data_g1$wave, 
                              relevant_data_g1$name_construct, 
                              function(x) unique(x))

waves_per_construct_g2 <- tapply(relevant_data_g2$wave, 
                              relevant_data_g2$name_construct, 
                              function(x) unique(x))

waves_per_construct_g3 <- tapply(relevant_data_g3$wave, 
                              relevant_data_g3$name_construct, 
                              function(x) unique(x))


# Identify variables not present in the same wave as "par"
par_wave_g1 <- waves_per_construct_g1[["par"]]
par_wave_g2 <- waves_per_construct_g2[["par"]]
par_wave_g3 <- waves_per_construct_g3[["par"]]


# stopped here; need to add wave into the data
# Initialize an empty dataframe to store results
missing_variables_df <- data.frame(construct = character(), 
                                   missing_waves = character(), 
                                   stringsAsFactors = FALSE)

for (construct in names(waves_per_construct_g1)) {
  if (construct != "par") {
    other_construct_wave <- waves_per_construct_g1[[construct]]
    missing_variables <- setdiff(par_wave_g1, other_construct_wave)
    
    if (length(missing_variables) > 0) {
      # Print results to console
      cat("Variables not present in the same wave as 'par' for construct", construct, ":", paste(missing_variables, collapse = ", "), "\n")
      
      # And save the results in the dataframe
      missing_variables_df <- rbind(missing_variables_df, data.frame(construct = construct, missing_waves = paste(missing_variables, collapse = ", ")))
    }
  }
}

# Now the missing_variables_df can be saved for later consultation

