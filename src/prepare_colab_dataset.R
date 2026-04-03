library(tidyr)
library(dplyr)

# De-transform function.
colab_variable_de_log <- function(data){
  log10_cols <- grep("log10", names(data), value = T)
  data_nolog <- data %>%
    mutate(across(all_of(log10_cols), ~ 10 ^ .x)) %>% 
    rename_with(~ gsub("log10_", "", log10_cols), .cols = log10_cols) 
  
  return(data_nolog)
}

# Prepare colab ed dataset (low prevalence).
colab_ed_low_with_log <- read.csv('data/Data_colab_ED_low_prevalence.csv')

colab_ed_low_prevalence_without_log <- colab_variable_de_log(colab_ed_low_with_log)

write.csv(
  colab_ed_low_prevalence_without_log,
  paste0('data/colab_ed_low_prevalence.csv'),
  row.names = FALSE
)

# Prepare colab ed dataset (high prevalence).
colab_ed_high_with_log <- read.csv('data/Data_colab_ED_high_prevalence.csv')

colab_ed_high_prevalence_without_log <- colab_variable_de_log(colab_ed_high_with_log)

write.csv(
  colab_ed_high_prevalence_without_log,
  paste0('data/colab_ed_high_prevalence.csv'),
  row.names = FALSE
)
