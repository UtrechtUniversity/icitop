# add columns

new_column<-function(dataset, new_name, input){
  for (i in seq_along(new_name)){
    dataset[[new_name[i]]] <- input[i]
  }
  return(dataset)
} 

df.included<-new_column(df.included,colnames(added.values),added.values[1,])
