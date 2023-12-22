#' Create a rectangular codebook using the codebook::codebook_table function.
#' This function also writes the codebook to a csv file in a specified path.
#' 
#' @param df The dataframe/tibble for which to create a codebook.
#' @param path The path to where the codebook should be written.
#' @returns The codebook for the dataframe/tibble
#' @examples
#' attr(mtcars$mpg, "label") <- "Miles per Gallon"
#' create_codebook(mtcars)
# Dependencies
if (!require(codebook, quietly = TRUE)) {
  install.packages("codebook")
  library(codebook)
}

create_codebook <- function(df, path) {
  codebook_df <- codebook::codebook_table(df) 
  write.csv(codebook_df, path)
  return(codebook_df)
}