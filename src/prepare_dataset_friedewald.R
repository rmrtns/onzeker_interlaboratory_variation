library(tidyr)
library(dplyr)

# Set parameters.
source('src/variables_friedewald.R', local = variables <- new.env())

# Prepare friedewald gp dataset.
friedewald_gp <- read.csv('data/JuistheidFRIE-HA(Remy)_251029_anonymised.csv')
friedewald_gp$X.3 <- as.numeric(friedewald_gp$X.3)
friedewald_gp_wide <- pivot_wider(friedewald_gp,
                                  id_cols = c(ID,AfnD,AfnT,Gesl,Leeft ),
                                  names_from = X,
                                  values_from = X.3,
                                  values_fn = list(X.3 = mean))

friedewald_gp_wo_missing <- friedewald_gp_wide %>%
  rename_with(~ gsub("-", ".", .x)) %>%
  filter(complete.cases(across(all_of(variables$selected_variables_friedewald)))) %>%
  filter(Triglyceriden <= 4.5) %>%
  mutate(Gesl = recode(Gesl, "M" = 1, "V" = 0)) %>%
  mutate(BezoekersID = row_number()) 

write.csv(
  friedewald_gp_wo_missing,
  paste0('data/friedewald_gp.csv'),
  row.names = FALSE
)

# Prepare friedewald cardio dataset.
friedewald_cardio <- read.csv('data/JuistheidFRIE-Card(Remy)_251017_anonymised.csv')
friedewald_cardio$X.3 <- as.numeric(friedewald_cardio$X.3)
friedewald_cardio_wide <- pivot_wider(friedewald_cardio,
                                  id_cols = c(ID,AfnD,AfnT,Gesl,Leeft ),
                                  names_from = X,
                                  values_from = X.3,
                                  values_fn = list(X.3 = mean))
friedewald_cardio_wo_missing <- friedewald_cardio_wide %>%
  dplyr::rename_with(~ gsub("-", ".", .x)) %>%
  filter(complete.cases(across(all_of(variables$selected_variables_friedewald)))) %>%
  filter(Triglyceriden <= 4.5) %>%
  mutate(Gesl = recode(Gesl, "M" = 1, "V" = 0)) %>%
  mutate(BezoekersID = row_number())

write.csv(
  friedewald_cardio_wo_missing,
  paste0('data/friedewald_cardio.csv'),
  row.names = FALSE
)