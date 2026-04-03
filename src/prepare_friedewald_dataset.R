library(tidyr)
library(dplyr)

# Set parameters.
source('src/variables_friedewald.R', local = variables <- new.env())

# Prepare friedewald gp dataset.
friedewald_gp <- read.csv('data/JuistheidFRIE-HA(Remy)_251029_anonymised_wide_format.csv')
friedewald_gp_wo_missing <- friedewald_gp %>%
  filter(complete.cases(across(all_of(variables$selected_variables_friedewald))))

write.csv(
  friedewald_gp_wo_missing,
  paste0('data/friedewald_gp.csv'),
  row.names = FALSE
)

# Prepare friedewald cardio dataset.
friedewald_cardio <- read.csv('data/JuistheidFRIE-Card(Remy)_251017_anonymised_wide_format.csv')
friedewald_cardio_wo_missing <- friedewald_cardio %>%
  filter(complete.cases(across(all_of(variables$selected_variables_friedewald))))

write.csv(
  friedewald_cardio_wo_missing,
  paste0('data/friedewald_cardio.csv'),
  row.names = FALSE
)