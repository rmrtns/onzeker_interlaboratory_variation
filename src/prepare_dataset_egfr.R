library(tidyr)
library(dplyr)

# Set parameters.
source('src/variables_egfr.R', local = variables <- new.env())

# Prepare egfr gp dataset.
egfr_gp <- read.csv('data/JuistheidCKD-HA(Remy)_251017_anonymised.csv')
egfr_gp$X.3 <- as.numeric(egfr_gp$X.3)
egfr_gp_wide <- pivot_wider(egfr_gp,
                                  id_cols = c(ID,AfnD,AfnT,Gesl,Leeft ),
                                  names_from = X,
                                  values_from = X.3,
                                  values_fn = list(X.3 = mean))

egfr_gp_wo_missing <- egfr_gp_wide %>%
  filter(complete.cases(across(all_of(variables$selected_variables_egfr)))) %>%
  mutate(Gesl = recode(Gesl, "M" = 1, "V" = 0)) %>%
  mutate(BezoekersID = row_number()) 

write.csv(
  egfr_gp_wo_missing,
  paste0('data/egfr_gp.csv'),
  row.names = FALSE
)

# Prepare egfr int dataset.
egfr_int <- read.csv('data/JuistheidCKD-Int(Remy)_251030_anonymised.csv')
egfr_int$X.3 <- as.numeric(egfr_int$X.3)
egfr_int_wide <- pivot_wider(egfr_int,
                            id_cols = c(ID,AfnD,AfnT,Gesl,Leeft ),
                            names_from = X,
                            values_from = X.3,
                            values_fn = list(X.3 = mean))
egfr_int_wo_missing <- egfr_int_wide %>%
  filter(complete.cases(across(all_of(variables$selected_variables_egfr)))) %>%
  mutate(Gesl = recode(Gesl, "M" = 1, "V" = 0)) %>%
  mutate(BezoekersID = row_number()) 

write.csv(
  egfr_int_wo_missing,
  paste0('data/egfr_int.csv'),
  row.names = FALSE
)