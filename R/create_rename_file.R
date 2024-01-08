#' Create an empty dataframe to be filled when looking which variables should be
#' renamed, excluded, relabelled, etc. Outputs an empty and a to-be-filled csv file
#'  
#' @param codebook A codebook dataframe with columns "name" and "label" as the first 2 columns.
#' @param study The name of the study which should be saved in the file name, e.g., "102.us-lls"
#' @param path A path to the folder where the rename files should be written, e.g., "data/102.us-lls/2.data-checks".
#' @returns An empty rename object, with columns for name, label, generation, wave, etc.
#' @examples
#' rename.empty <- create_rename_file(data.dictionary, "102.us-lls", "data/102.us-lls/2.data-checks")
# Dependencies
if (!require(dplyr, quietly = TRUE)) {
  install.packages("dplyr")
  library(dplyr)
}

create_rename_file <- function(codebook, study, path){
  renameobject <- codebook[, c(1, 2)] %>%
    mutate(
      generation = NA,
      wave = NA,
      reporter = NA,
      target = NA,
      type_instrument = NA,
      name_construct = NA,
      new_name = NA,
      new_label = NA,
      comments = NA
    )
  
  # Write the first 2 columns of the codebook to csv
  # Save the empty version
  write.csv(renameobject,
            paste0(path, "/1_rename_", study, "_empty.csv"),
            row.names = FALSE)
  
  # And save the to be filled version if it's not already there
  to_be_filled_file <- paste0(path, "/1_rename_", study, "_filled.csv")
  
  if (!file.exists(to_be_filled_file)) {
    # Write the CSV file only if it doesn't exist
    write.csv(renameobject, to_be_filled_file, row.names = FALSE)
    cat("CSV file written:", to_be_filled_file, "\n")
  } else {
    cat("CSV file already exists:", to_be_filled_file, "\n", "File not overwritten")
  }
  
  # Save the object in the Environment
  return(renameobject)
}