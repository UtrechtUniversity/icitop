#' Determines whether all dataframes in a list have the same specified id variable.
#' If not all dataframes have the specified id variable, provides a list of dataframes without specified id variable
#' 
#' @param dfs A list with dataframes. Each list item should have be named.
#' @param id the name of the id variable.
#' @returns a statement whether id variable is present. If not all dataframes have the  id variable: a list of dataframes without id variable
#' @examples
#' dfs <- list(mtcars = mtcars, iris = iris)
#' id_check(dfs, "mpg") # All dataframes have the specified variable.
#' id_check(dfs, "id")

id_check <- function(dfs, id) {
  # Check if all dataframes have the variable 'id'
  dataframes_without_id <- names(dfs)[sapply(dfs, function(df) !(id %in% names(df)))]
  
  # Print the result
  if (length(dataframes_without_id) == 0) {
    cat("All dataframes have the specified variable.\n")
  } else {
    cat("Dataframes without specified id variable: ", paste(dataframes_without_id, collapse = "\n\n"), "\n")
  }
  
  # Return the result
  return(dataframes_without_id)
}
