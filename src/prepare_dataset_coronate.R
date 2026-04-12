library(tidyr)
library(dplyr)

# Import dataset.
coronate <- read.csv('data/20210322_databestand_CORONATE_study_anonymised_healthcareworkers.csv', sep = ";", dec = ",")

# Prepare dataset.
colab_prepared <- coronate %>% 
  mutate(
    leeft = floor(as.numeric(leeft)),
    PCR.uitslag = case_when(
      PCR.uitslag == "neg" ~ 0, 
      PCR.uitslag == "pos" ~ 1
    ),
    across(
      all_of(c("AF", "Alb", "Bil", "CRP", "GGT", "LDH", "Leu", "Ery", "Eo", "Baso")),
      ~ as.numeric(.x)
    )
  )

colab_healthcare <- colab_prepared
  
write.csv(
  colab_healthcare,
  paste0('data/colab_healthcare.csv'),
  row.names = FALSE
)