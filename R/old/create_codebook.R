#' Create a rectangular codebook using the codebook::codebook_table function.
#' This function also writes the codebook to an excel file in a specified path.
#' 
#' @param df The dataframe/tibble for which to create a codebook.
#' @param path The path to where the codebook should be written.
#' @returns The codebook for the dataframe/tibble
#' @examples
#' attr(mtcars$mpg, "label") <- "Miles per Gallon"
#' codebookpath <- paste0("data/codebook.xlsx")
#' create_codebook(mtcars, codebookpath)
# Dependencies
if (!require(codebook, quietly = TRUE)) {
  install.packages("codebook")
  library(codebook)
}

if (!require(openxlsx, quietly = TRUE)) {
  install.packages("openxlsx")
  library(openxlsx)
}

create_codebook <- function(df, path) {
  codebook_df <- codebook::codebook_table(df) 
  openxlsx::write.xlsx(codebook_df, path)
  return(codebook_df)
}