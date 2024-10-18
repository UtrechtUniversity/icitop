#' Create a rectangular codebook using the codebook::codebook_table function.
#' This function also writes the codebook to an excel file in a specified path.
#' This function also adds the names of the source files to the codebook
#' 
#' @param dfs the name of the list with dataframes
#' @param df The dataframe/tibble for which to create a codebook.
#' @param path The path to where the codebook should be written.
#' @returns The codebook for the dataframe/tibble
#' @examples
#' attr(mtcars$mpg, "label") <- "Miles per Gallon"
#' dfs<-list(mtcars)
#' codebookpath <- paste0("data/codebook.xlsx")
#' create_codebook(dfs, mtcars, codebookpath)
# Dependencies
if (!require(codebook, quietly = TRUE)) {
  install.packages("codebook")
  library(codebook)
}

if (!require(openxlsx, quietly = TRUE)) {
  install.packages("openxlsx")
  library(openxlsx)
}

create_codebook_concept <- function(dfs, df, path) {
  codebook_df <- codebook::codebook_table(df) 
  
  # Create a list with the name of the source files
  # id is repeated, so the length should be shortened by (n datasets - 1)
  total_columns <- sum(sapply(dfs, ncol)) - length(dfs) + 1
  
  # Create a dataframe called 'source' with the length of the sum of columns
  source <- data.frame(source = rep(NA, total_columns))
  
  # Assign names to the list elements based on the dataframes and their columns
  source$source[1]<-names(dfs)[1] # the name of dataframe +1 longer, because it contains id
  
  counter <- 2
  for (i in seq_along(dfs)) {
    dataset <- dfs[[i]]
    df_name <- names(dfs)[i]
    col_names <- names(dataset)
    num_cols <- length(col_names)-1
    
    repeated_names <- rep(df_name, num_cols)
    source$source[counter:(counter + num_cols - 1)] <- repeated_names
    counter <- counter + num_cols
  }
 
  codebook_df[,ncol(codebook_df)+1]<-source$source
  names(codebook_df)[ncol(codebook_df)]<-"source"
  
  # if the file does not have labels, these columns are missing. They should be added.  
  
  # Define the required columns in the desired order
  required_columns <- c(  "name",	"label",	"data_type",	"value_labels",	"n_missing",	"complete_rate",	
                          "min",	"median",	"max",	"mean",	"sd",	"n_value_labels",	"hist",	"format.spss",	"display_width",	"source")
  
  # Find which columns are missing
  missing_columns <- setdiff(required_columns, colnames(codebook_df))
  
  # Add missing columns (if any), ensuring they are added in the correct order
  for (col in missing_columns) {
    codebook_df[[col]] <- NA  # Add the missing column with NA values
  }
  
  # Reorder the columns to match the required order
  codebook_df <- codebook_df[required_columns]
  
  openxlsx::write.xlsx(codebook_df, path)
  return(codebook_df)
}

