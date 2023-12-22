#' Rename columns in a dataframe to values specified in the rename_data dataframe.
#'  
#' @param df The dataframe/tibble in which columns will be renamed.
#' @param rename_data The dataframe/tibble which contains a name (the old name corresponding to a column in df) and a new_name column (the new name of the column).
#' @returns The dataframe/tibble with renamed columns.
#' @examples
#' rename_mtcars <- data.frame(name = c("mpg", "cyl"),
#'                           new_name = c("milespergal","cylinder"))
#' rename_columns(mtcars, rename_mtcars)
rename_columns <- function(df, rename_data) {
  
  # For every column in df
  for (i in 1:length(colnames(df))) {
    
    # Select matching names between the df colnames and the values in rename_data$name
    matching_row <- rename_data[rename_data$name %in% colnames(df)[i], ]
    
    # If the match is not 0 or NA
    if (nrow(matching_row) > 0 && !is.na(matching_row$new_name)) {
      
      # Rename the column to the new name
      colnames(df)[i] <- matching_row$new_name
    }
  }
  return(df)
}
