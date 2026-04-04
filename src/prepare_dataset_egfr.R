library(tidyr)
library(dplyr)

# Set parameters.
source('src/variables_egfr.R', local = variables <- new.env())

# Prepare egfr gp dataset.
egfr_gp <- read.csv('data/JuistheidCKD-HA(Remy)_251017_anonymised_wide_format.csv')
egfr_gp_wo_missing <- egfr_gp %>%
  filter(complete.cases(across(all_of(variables$selected_variables_egfr)))) %>%
  mutate(Gesl = replace_values(Gesl, "M" ~ 1, "V" ~ 0))

write.csv(
  egfr_gp_wo_missing,
  paste0('data/egfr_gp.csv'),
  row.names = FALSE
)

# Prepare egfr int dataset.
egfr_int <- read.csv('data/JuistheidCKD-Int(Remy)_251030_anonymised_wide_format.csv')
egfr_int_wo_missing <- egfr_int %>%
  filter(complete.cases(across(all_of(variables$selected_variables_egfr)))) %>%
  mutate(Gesl = replace_values(Gesl, "M" ~ 1, "V" ~ 0))

write.csv(
  egfr_int_wo_missing,
  paste0('data/egfr_int.csv'),
  row.names = FALSE
)