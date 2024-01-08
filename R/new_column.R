#' Add new columns to a dataframe/tibble based on a new_columns dataframe/tibble
#' The new_columns dataframe/tibble should have a number of column names,
#' and a value for each column in the first row.
#' 
#' @param dataset The dataframe/tibble to which the columns should be added.
#' @param new_columns_df The dataframe/tibble that contains the columns to be added and their value(s).
#' @returns The dataframe/tibble with the added columns and values.
#' @examples
#' add_cols <- data.frame(Family = 2, ID = 14)
#' new_column(mtcars, add_cols)
new_column <- function(dataset, new_columns_df) {
  
  # For every column in new_columns_df
  for(col in seq_along(colnames(new_columns_df))){
    
    # Create that same column in the dataset and assign it the values from new_columns_df
    dataset[[colnames(new_columns_df[col])]] <- new_columns_df[1, col]
  }
  return(dataset)
}