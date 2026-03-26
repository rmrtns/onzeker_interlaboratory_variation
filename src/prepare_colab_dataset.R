library(tidyr)

# De-transform function.
colab_variable_de_log <- function(data){
  # De-transform log10 variables and removed log10_ from columns names
  
  log10_cols <- grep("log10", names(data), value = T)
  data_nolog <- data %>%
    mutate(across(all_of(log10_cols), ~ 10 ^ .x)) %>% 
    rename_with(~ gsub("log10_", "", log10_cols), .cols = log10_cols) 
  
  return(data_nolog)
}

# Prepare colab ed dataset (low and high prevalence).
colab_ed_with_log <- read.csv('data/CoLab_externe_validatie_Zuyderland.csv')

colab_ed_low_prevalence_without_log <- colab_variable_de_log(colab_ed_with_log)

colab_ed_low_prevalence_without_log <- colab_ed_low_prevalence_without_log %>%
  group_by(Pat) %>%
  slice(1) %>%
  ungroup()

write.csv(
  colab_ed_low_prevalence_without_log,
  paste0('data/colab_ed_low_prevalence.csv'),
  row.names = FALSE
)

