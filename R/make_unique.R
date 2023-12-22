#' Make values in the new_name column of a dataframe unique if they are the same 
#' using the base R make.unique() function. This custom function checks whether 
#' the input is a dataframe, and then appends a number to names from the new_name 
#' column in the provided dataframe. No number is added if the name is NA.
#' 
#' @param df The dataframe/tibble which contains a new_name character column.
#' @returns The dataframe/tibble with unique values for the new_name column.
#' @examples
#' mtcars$new_name <- rep("examplename", length(mtcars$mpg))
#' make_unique(mtcars)
# Dependencies
if (!require(dplyr, quietly = TRUE)) {
  install.packages("dplyr")
  library(dplyr)
}

make_unique <- function(df) {
  # Check if the df is a dataframe
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
      }
  
  # Make sure new_name is a character variable
  df$new_name <- as.character(df$new_name)
  
  # Append numbers to non-unique new_name values
  df <- df %>%
    dplyr::mutate(new_name = ifelse(!is.na(new_name), 
                                    make.unique(as.character(new_name), sep = "_"), NA))
  
  return(df)
}




