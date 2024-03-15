# part of 02-basic-renaming
# For all waves in which parenting variables are measured, 
# is the moderator  also present? 
# result is a dataframe with the waves for each variable


# 0. Add test rows to rename_basic (remove later)
rename_basic$generation[90:96] <- c("g2", "g1", "g2", "g2", "g2", "g2", "g3")
rename_basic$target[90:96] <- c("c", "f", "m", "f", "", "c", "c1")
rename_basic$name_construct[90:96] <- c("age", "par", "edu", "ses", "occ", "edu", "age")
rename_basic$wave[90:96] <- c("w2", "w1", "w1", "w2", "w2", "w2", "w2")


### 1. Filter on constructs of interest ####
# I made an excel sheet with information about (a selection of) the moderators and par 
# I made a separate one for G1 par and G2 par

constructs_of_interest_g1 <-read_xlsx("docs/g1-moderators.xlsx")
constructs_of_interest_g2 <-read_xlsx("docs/g2-moderators.xlsx")

# variables should be selected when they match the combined information in this sheet
# rows should only be selected if they meet ALL criteria
relevant_data_g1 <- inner_join(constructs_of_interest_g1, 
                               rename_basic, 
                               by = c("generation", "name_construct", "target"))

relevant_data_g2 <- inner_join(constructs_of_interest_g2,
                               rename_basic,
                               by = c("generation", "name_construct", "target"))


#### 2. Detect missing waves function ####
# Rule: If construct == par, dan moet er voor die wave waarin par gemeten 
# is de andere constructs aanwezig zijn 
detect_missing_waves <- function(df){
  # OLD
  # waves_per_construct <- tapply(relevant_data_df$wave, 
  #                               relevant_data_df$name_construct, # Should be: for the combination of name_construct + target
  #                               function(x) unique(x))
  #waves_per_construct <- with(relevant_data_df, tapply(wave, list(name_construct, generation, target), function(x) unique(x)))
  # select from this variable: waves_per_construct[,,"m"]
  #waves_per_construct_table <- with(relevant_data_df, table(generation, target, name_construct, wave))
  
  # NEW
  # Get unique combinations of name_construct, generation, and target
  combinations <- unique(df[, c("name_construct", "generation", "target")])
  
  # Initialize an empty data frame to store missing waves
  missing_waves_df <- data.frame(name_construct = character(), 
                                 missing_waves = character(), 
                                 target = character(),
                                 stringsAsFactors = FALSE)
  
  # Loop through each combination of name_construct, generation, and target
  for (i in 1:nrow(combinations)) {
    construct <- combinations$name_construct[i]
    generation <- combinations$generation[i]
    target <- combinations$target[i]
    
    # Subset data for the current combination
    # subset_df <- df[df$name_construct == construct &
    #                   df$generation == generation &
    #                   df$target == target, ]
    
    # Subset data for the current combination, taking into account NAs in target
    subset_df <- df[df$name_construct == construct &
                      df$generation == generation &
                      (df$target == target | (is.na(df$target) & is.na(target))),]
    
    # Get unique waves for the current combination
    waves <- unique(subset_df$wave)
    
    # If the construct is "par", continue to the next iteration of the for loop
    if (construct == "par") next
    
    # Get unique waves where "par" is present, taking into account NAs in target
    # TO DISCUSS: 
    # Regel: als er parenting is gemeten in een specifieke wave, dan moeten de 
    # andere constructs uit de g1_moderators ook in die wave zijn gemeten. 
    # Maar doen generation en target er dan toe? Oftewel:
    # als par in g1 en f is gemeten, moet dan de moderator of interest ook in g1 en f zijn gemeten?
    # Of kunnen we in de selectie "par_waves" hieronder de generation en/of target filter
    # eruit halen?
    # Voorbeeld resultaat van huidige code:
    # - subset_df/relevant_data_g1: generation:"g1", name_construct:"sex", target:NA
    # - par_waves: NA, want target is nooit NA (het is alleen m en f op dit moment) in de rijen van relevant_data_g1 waar par is gemeten
    
    par_waves <- unique(df$wave[df$name_construct == "par" &
                                  df$generation == generation &
                                  (df$target == target | (is.na(df$target) & is.na(target)))]) 
  
    # Find missing waves for the current combination
    missing_waves <- setdiff(par_waves, waves)
    
    # If missing waves are found, add them to the missing_waves_df
    if (length(missing_waves) > 0) {
      
      # Print results to console
      cat("Variables not present in the same wave as 'par' for construct", 
          construct, "and target", target, "in generation", generation, ":", 
          paste(missing_waves, collapse = ", "), "\n")
      
      # Add missing waves to the missing_waves_df
      missing_waves_df <- rbind(missing_waves_df, 
                                data.frame(generation = generation,
                                           name_construct = construct, 
                                           missing_waves = paste(missing_waves, collapse = ", "),
                                           target = target))
    }
  }
  return(missing_waves_df)
}


#### 3. Use the function for the relevant data ####
missing_g1 <- detect_missing_waves(relevant_data_g1)
missing_g2 <- detect_missing_waves(relevant_data_g2)

all_missing_waves <- bind_rows(missing_g1, missing_g2)


#### OLD ####
#par_wave <- waves_per_construct[["par"]]

# With thanks to ChatGPT :)
# Prompt and full answer can be seen here: https://chat.openai.com/share/abdd4db2-284d-442c-af56-93ace2e177fa

# Filter dataframe for constructs of interest G2
# constructs_of_interest_g2 <- c("eth", 
#                             "aab", 
#                             "sex", 
#                             "gen", 
#                             "bir", 
#                             "cst", 
#                             "ris", 
#                             "age", 
#                             "rst", 
#                             "car", 
#                             "etp", 
#                             "res",
#                             "req",
#                             "vinc",
#                             "occ",
#                             "edu",
#                             "ses",
#                             "alc",
#                             "dru",
#                             "age",
#                             "inv",
#                             "sub"
# )
# 
# # Filter dataframe for constructs of interest G3
# constructs_of_interest_g3 <- c("nch",
#                             "bir",
#                             "age",
#                             "sex",
#                             "gen"
#                        )
# 
# # TODO: filter on "g1", "g2", "g3"?
# relevant_data_g1 <- rename_basic[rename_basic$name_construct %in% constructs_of_interest_g1, ]
# relevant_data_g2 <- rename_basic[rename_basic$name_construct %in% constructs_of_interest_g2, ]
# relevant_data_g3 <- rename_basic[rename_basic$name_construct %in% constructs_of_interest_g3, ]
# 
# # Alternative
# relevant_data_g1 <- rename_basic %>%
#   filter(grepl("g1", new_name, 
#                ignore.case = TRUE) & name_construct %in% constructs_of_interest_g1)
# # Remove if unnecessary
# 
# 
# # Create a tabular overview of waves per construct > This actually creates a list
# waves_per_construct_g1 <- tapply(relevant_data_g1$wave, 
#                               relevant_data_g1$name_construct, 
#                               function(x) unique(x))
# 
# waves_per_construct_g2 <- tapply(relevant_data_g2$wave, 
#                               relevant_data_g2$name_construct, 
#                               function(x) unique(x))
# 
# waves_per_construct_g3 <- tapply(relevant_data_g3$wave, 
#                               relevant_data_g3$name_construct, 
#                               function(x) unique(x))
# 
# 
# # Identify variables not present in the same wave as "par"
# par_wave_g1 <- waves_per_construct_g1[["par"]]
# par_wave_g2 <- waves_per_construct_g2[["par"]]
# par_wave_g3 <- waves_per_construct_g3[["par"]]
# 
# 
# # stopped here; need to add wave into the data
# # Initialize an empty dataframe to store results
# missing_variables_df <- data.frame(construct = character(), 
#                                    missing_waves = character(), 
#                                    stringsAsFactors = FALSE)
# 
# for (construct in names(waves_per_construct_g1)) {
#   if (construct != "par") {
#     other_construct_wave <- waves_per_construct_g1[[construct]]
#     missing_variables <- setdiff(par_wave_g1, other_construct_wave)
#     
#     if (length(missing_variables) > 0) {
#       # Print results to console
#       cat("Variables not present in the same wave as 'par' for construct", construct, ":", paste(missing_variables, collapse = ", "), "\n")
#       
#       # And save the results in the dataframe
#       missing_variables_df <- rbind(missing_variables_df, data.frame(construct = construct, missing_waves = paste(missing_variables, collapse = ", ")))
#     }
#   }
# }
# 
# # Now the missing_variables_df can be saved for later consultation