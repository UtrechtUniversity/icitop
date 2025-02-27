#' Rename labels of a dataframe to values specified in the rename_data dataframe.
#'  
#' @param df The dataframe/tibble in which labels will be renamed.
#' @param rename_data The dataframe/tibble which contains a name (the old name corresponding to a column in df) and a new_label column (the new label of the variable).
#' @returns The dataframe/tibble with renamed columns.
#' @examples
#' new_labels_mtcars <- data.frame(name = c("mpg", "cyl"),
#'                           new_label = c("miles per gallon","cylinder size"))
#' relabelled <- rename_labels(mtcars, new_labels_mtcars)
rename_labels <- function(df, rename_data) {
  
  # For every column in df
  for (i in 1:length(colnames(df))) {
    
    # Select matching names between the df colnames and the values in rename_data$name
    matching_row <- rename_data[rename_data$new_name %in% colnames(df)[i], ]
    
    # If the match is not 0 or NA
    if (nrow(matching_row) > 0 && !is.na(matching_row$new_label)) {
      
      # Rename the variable label to the new label
      attr(df[[colnames(df)[i]]], "label") <- matching_row$new_label
    }
  }
  return(df)
}
