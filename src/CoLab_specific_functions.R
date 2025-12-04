library(dplyr)

CoLab_variable_de_log <- function(data){
  # De-transform log10 variables and removed log10_ from columns names
  
  log10_cols <- grep("log10", names(data), value = T)
  data_nolog <- data %>%
    mutate(across(all_of(log10_cols), ~ 10 ^ .x)) %>% 
    rename_with(~ gsub("log10_", "", log10_cols), .cols = log10_cols) 
  
  return(data_nolog)
  
}


skml_bias_table_colab <- function(skml_long_table, bias_types, skml_names, non_bias_vec){
  non_bias_bias_types_vec <- paste(rep(bias_types, each = length(non_bias_vec)), non_bias_vec, sep="")
  
  tmp <- skml_long_table %>%
    select("name", matches(bias_types)) %>%
    select("name", matches(skml_names)) %>%
    mutate(!!!setNames(as.list(rep(NA, length(non_bias_bias_types_vec))), non_bias_bias_types_vec))
  
  
  return(tmp)
}