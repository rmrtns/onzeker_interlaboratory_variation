skml_data_class_correction_function <- function(dataframe_list){
  
  dataframe_corrected <-list()
  
  for(i in 1:length(dataframe_list)){
    
    indvidual_dataframe <- dataframe_list[[i]]
    last_columns <- (ncol(indvidual_dataframe)-5):ncol(indvidual_dataframe) 
    
    for(j in last_columns){
      indvidual_dataframe[[j]] <- as.numeric(indvidual_dataframe[[j]])
    }
    
    dataframe_corrected[[i]] <- indvidual_dataframe
    
  }
  
  return(dataframe_corrected)
}


