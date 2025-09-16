library(dplyr)
library(tidyr)

skml_data_merge_function <- function(dataframe_list){
  
  dataframe_pivot <-list()
  
  
  for(i in 1:length(dataframe_list)){
    
    indvidual_dataframe <- dataframe_list[[i]]
    
    dataframe_pivoted <- pivot_longer(indvidual_dataframe,
                                   names(indvidual_dataframe)[grepl("\\d{4}[.]\\d{1}\\D{1}",
                                                                    names(indvidual_dataframe), 
                                                                    perl = TRUE)])
    dataframe_pivot[[i]] <- dataframe_pivoted
  }
  
  
  merged_dataframe <- dplyr::bind_rows(dataframe_pivot)
  return(merged_dataframe)
  
}
