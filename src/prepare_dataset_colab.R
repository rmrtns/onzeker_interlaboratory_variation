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

# Import dataset.
colab_with_log <- read.csv('data/CoLab_externe_validatie_Zuyderland_met_datum.csv')

# Prepare dataset.
colab_wo_log <- colab_variable_de_log(colab_with_log)
colab_prepared <- colab_wo_log %>% 
  mutate(
    ER_date = lubridate::as_date(ER_date),
    visit_id = row_number()
  ) %>%
  filter(age >= 18)

colab_low_prevalence <- colab_prepared 
  
colab_high_prevalence <- colab_prepared %>%
  filter(ER_date >= lubridate::as_date("2020-03-01"))

write.csv(
  colab_low_prevalence,
  paste0('data/colab_ed_low_prevalence.csv'),
  row.names = FALSE
)

write.csv(
  colab_high_prevalence,
  paste0('data/colab_ed_high_prevalence.csv'),
  row.names = FALSE
)
