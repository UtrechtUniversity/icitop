# part of 02-basic-renaming
# For all waves in which parenting variables are measured, 
# is the moderator  also present? 
# result is a dataframe with the waves for each variable

# output:
# targets for par per wave
# missing g1 (but complete)
# present g1 (same as missing but opposite)

### 1. Filter on constructs of interest ####

# upload file with moderators
constructs_of_interest_g1 <-read_xlsx("docs/g1-moderators.xlsx") # g1 moderators - for each wave G1 parenting is measured
constructs_of_interest_g2 <-read_xlsx("docs/g2-moderators.xlsx") # g2 moderators- for each wave G2 parenting is measured
constructs_of_interest_g2_static<-read_xlsx("docs/g2-moderators-static.xlsx") # g2 moderators -One measure for each G2 participant

# variables should be selected when they match the combined information in this sheet
relevant_data_g1 <- inner_join(constructs_of_interest_g1, 
                              rename_basic, 
                              by = c("generation", "name_construct"))

relevant_data_g2 <- inner_join(constructs_of_interest_g2,
                               rename_basic,
                               by = c("generation", "name_construct"))

relevant_data_g2_static <- inner_join(constructs_of_interest_g2_static, 
                                rename_basic, 
                                by = c("generation", "name_construct"))

# For static G2 moderators: determine missing variables:
available_g2_static <- unique(relevant_data_g2_static[, c("name_construct", "wave", "target","generation"  )])
missing_g2_static <- as.data.frame(constructs_of_interest_g2_static$name_construct[!constructs_of_interest_g2_static$name_construct %in% available_g2_static$name_construct])
colnames(missing_g2_static)<-"name_construct"

#### 2. Detect missing waves function ####
detect_missing_waves <- function(df, generation, constructs){
 
  # For available data: Get unique combinations of name_construct, generation, and target
  available_data <- unique(df[, c("name_construct", "wave", "target","generation"  )])
  
  # For missing data: Initialize an empty data frame to store missing waves
  missing_waves_df <- data.frame(name_construct = character(), 
                                 missing_waves = character(), 
                                 stringsAsFactors = FALSE)
  
  # Determine the waves for par
  par_waves <- unique(df$wave[df$name_construct == "par" &
                                df$generation == generation]) 
  
  # Loop through each combination of name_construct, generation, and target
  for (i in 1:nrow(constructs)) {
    construct <- constructs$name_construct[i]
    generation <- constructs$generation[i]
   
    # Subset data for the current combination generation and construct
    subset_df <- df[df$name_construct == construct &
                      df$generation == generation,]
    
    # Get unique waves for the current combination
    waves <- unique(subset_df$wave)
    
    # If the construct is "par", continue to the next iteration of the for loop
    if (construct == "par") next
    
    # Find missing waves for the current combination
    missing_waves <- setdiff(par_waves, waves)
    
    # If missing waves are found, add them to the missing_waves_df
    if (length(missing_waves) > 0) {
      
      # Print results to console
      cat("Not present in the same wave as 'par'", 
          construct, "in generation", generation, ":", 
          paste(missing_waves, collapse = ", "), "\n")
      
      # Add missing waves to the missing_waves_df
      missing_waves_df <- rbind(missing_waves_df, 
                                data.frame(generation = generation,
                                           name_construct = construct, 
                                           missing_waves = paste(missing_waves, collapse = ", ")))
      
    }
  }
  return(list(missing_waves_df,available_data))
  
}

#### 3. Use the function for the relevant data ####

missing_g1 <- detect_missing_waves(relevant_data_g1, "g1",constructs_of_interest_g1)[[1]]
available_g1<-detect_missing_waves(relevant_data_g1, "g1",constructs_of_interest_g1)[[2]]
missing_g2 <- detect_missing_waves(relevant_data_g2, "g2", constructs_of_interest_g2)[[1]]
available_g2 <- detect_missing_waves(relevant_data_g2, "g2",constructs_of_interest_g2)[[2]]

openxlsx::write.xlsx(missing_g1, paste0(path,"missing_g1_",studyname,".xlsx"))
openxlsx::write.xlsx(available_g1, paste0(path,"available_g1",studyname,".xlsx"))
openxlsx::write.xlsx(missing_g2, paste0(path,"missing_g2_",studyname,".xlsx"))
openxlsx::write.xlsx(available_g2, paste0(path,"available_g2_",studyname,".xlsx"))
openxlsx::write.xlsx(missing_g2_static, paste0(path,"missing_g2_static_",studyname,".xlsx"))
openxlsx::write.xlsx(available_g2_static, paste0(path,"available_g2_static_",studyname,".xlsx"))




