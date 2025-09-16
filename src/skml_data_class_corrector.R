skml_data_class_correction_function <- function(dataframe_list){
  
  dataframe_corrected <-list()
  
  for(i in 1:length(dataframe_list)){
    
    indvidual_dataframe <- dataframe_list[[i]]
    measurment_columns <- names(indvidual_dataframe)[grepl("\\d{4}[.]\\d{1}\\D{1}",
                                                           names(indvidual_dataframe), 
                                                           perl = TRUE)]
    
    for(j in measurment_columns){
      
      indvidual_dataframe[[j]] <- as.numeric(indvidual_dataframe[[j]])
      
    }
    
    dataframe_corrected[[i]] <- indvidual_dataframe
    
  }
  
  return(dataframe_corrected)
}
