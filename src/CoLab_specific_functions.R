library(dplyr)

CoLab_variable_de_log <- function(data){
  # De-transform log10 variables and removed log10_ from columns names
  
  log10_cols <- grep("log10", names(data), value = T)
  data_nolog <- data %>%
    mutate(across(all_of(log10_cols), ~ 10 ^ .x)) %>% 
    rename_with(~ gsub("log10_", "", log10_cols), .cols = log10_cols) 
  
  return(data_nolog)
  
}