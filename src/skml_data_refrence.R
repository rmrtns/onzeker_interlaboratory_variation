skml_data_refrence <- function(){
  source("src/skml_data_load.R")
  
  df_list <- skml_data_load_function(2)
  
  df_corrected <-list()
  
  for(i in 1:length(df_list)){
    
    indvidual_df <- df_list[[i]]
    indvidual_df[["Gemiddelde"]] <- as.numeric(indvidual_df[["Gemiddelde"]])
    df_corrected[[i]] <- indvidual_df
    
  }
  
  merged_df <- dplyr::bind_rows(df_corrected)
  Ref_expert_df <- merged_df %>% 
    filter(Methode == "Referentiewaarde" | Methode == "Expertwaarde") %>% 
    select(ctm, Bepaling,Methode, Gemiddelde)
  
  return(Ref_expert_df)
}
