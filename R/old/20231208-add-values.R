# add columns
new_column <- function(dataset, new_columns_df) {
  
  # For every column in new_columns_df
  for(col in seq_along(colnames(new_columns_df))){
    
    # Create that same column in the dataset and assign it the values from new_columns_df
    dataset[[colnames(new_columns_df[col])]] <- new_columns_df[1, col]
  }
  return(dataset)
  }