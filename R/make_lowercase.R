#' Function to convert column names  to lowercase.
#'  
#' @param df The dataframe/tibble in which columnnames are converted to lowercase
#' @returns The dataframe/tibble with renamed columns.
#' @examples
#' lower_case_mtcars <- data.frame(name = c("mpg", "cyl"),
#'                           new_label = c("miles per gallon","cylinder size"))
#' lowercase <- convert_to_lowercase(lower_case_mtcars)

make_lowercase <- function(df) {
  # Convert column names to lowercase
  colnames(df) <- tolower(colnames(df))
  return(df)
}