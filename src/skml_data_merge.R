library(dplyr)

skml_data_merge_function <- function(dataframe_list){
  
  merged_dataframe <- bind_rows(dataframe_list)
  return(merged_dataframe)
  
}


