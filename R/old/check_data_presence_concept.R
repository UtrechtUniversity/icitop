# part of 02-basic-renaming
# For all waves in which parenting variables are measured, 
# is the moderator  also present? 
# result is a dataframe with the waves for each variable


# output:
# targets for par per wave
# missing g1 (but complete)
# present g1 (same as missing but opposite)


# 0. Add test rows to rename_basic (remove later)
#rename_basic$generation[90:96] <- c("g2", "g1", "g2", "g2", "g2", "g2", "g3")
#rename_basic$target[90:96] <- c("c", "f", "m", "f", "", "c", "c1")
#rename_basic$name_construct[90:96] <- c("age", "par", "edu", "ses", "occ", "edu", "age")
#rename_basic$wave[90:96] <- c("w2", "w1", "w1", "w2", "w2", "w2", "w2")


### 1. Filter on constructs of interest ####
# I made an excel sheet with information about (a selection of) the moderators and par 
# I made a separate one for G1 par and G2 par

constructs_of_interest_g1 <-read_xlsx("docs/20240319-g1-moderators.xlsx")
constructs_of_interest_g2 <-read_xlsx("docs/g2-moderators.xlsx")

# variables should be selected when they match the combined information in this sheet
# rows should only be selected if they meet ALL criteria
relevant_data_g1 <- inner_join(constructs_of_interest_g1, 
                              rename_basic, 
                              by = c("generation", "name_construct"))

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
                                 stringsAsFactors = FALSE)
  
  # determine the waves for par
  par_waves <- unique(df$wave[df$name_construct == "par" &
                                df$generation == generation]) 
  
  # Loop through each combination of name_construct, generation, and target
  for (i in 1:nrow(constructs_of_interest_g1)) {
    construct <- constructs_of_interest_g1$name_construct[i]
    generation <- constructs_of_interest_g1$generation[i]
   
    
    # Subset data for the current combination
    # subset_df <- df[df$name_construct == construct &
    #                   df$generation == generation &
    #                   df$target == target, ]
    
    # Subset data for the current combination generation and construct
    subset_df <- df[df$name_construct == construct &
                      df$generation == generation,]
    
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
   
      # Find missing waves for the current combination
    missing_waves <- setdiff(par_waves, waves)
    
    # If missing waves are found, add them to the missing_waves_df
    if (length(missing_waves) > 0) {
      
      # Print results to console
      cat("Variables not present in the same wave as 'par' for construct", 
          construct, "in generation", generation, ":", 
          paste(missing_waves, collapse = ", "), "\n")
      
      # Add missing waves to the missing_waves_df
      missing_waves_df <- rbind(missing_waves_df, 
                                data.frame(generation = generation,
                                           name_construct = construct, 
                                           missing_waves = paste(missing_waves, collapse = ", ")))
    }
  }
  return(list(missing_waves_df,combinations))
  
}

#### 3. Use the function for the relevant data ####
missing_g1 <- detect_missing_waves(relevant_data_g1)
missing_g1 <- detect_missing_waves(relevant_data_g1)[[1]]
combinations<-detect_missing_waves(relevant_data_g1)[[2]]
missing_g2 <- detect_missing_waves(relevant_data_g2)

all_missing_waves <- bind_rows(missing_g1, missing_g2)


# maak overzicht van targets per wave 

wave1 <- unique(relevant_data_g1$target[relevant_data_g1$name_construct == "par" &
                                          relevant_data_g1$generation == "g1" &
                                          relevant_data_g1$wave == "w1"]) 

wave2 <- unique(relevant_data_g1$target[relevant_data_g1$name_construct == "par" &
                                            relevant_data_g1$generation == "g1" &
                                            relevant_data_g1$wave == "w2"]) 

combinations <- unique(df[, c("name_construct", "generation", "target")])


