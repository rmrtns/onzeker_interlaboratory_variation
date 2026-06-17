library(mcr)
library(dplyr)

# Year
year <- "2024"

# minimal amount of observations for paba method
N_min = 16 

# read the merged skml file 
#   If file doesn't exist, run:
#   source("src/new skml import.R")
skml <- read.csv(paste0("data/skml_merged_",year,".csv"))


paba.reg.fun <- function(ReferenceMethod, TestMethod){
  # Minimum sample size is 16
  df <- data.frame(ReferenceMethod, TestMethod)
  df2 <- na.omit(df)
  N <- nrow(df2)
  PB.reg <- try(mcr::mcreg(df2$ReferenceMethod, df2$TestMethod,
                           method.reg = "PBequi", na.rm = T,
                           method.ci = "analytical"), silent = T)
  if (class(PB.reg) == "MCResultAnalytical") {
    coef <- getCoefficients(PB.reg)
    cusum.stats <- calcCUSUM(PB.reg)
    H <- with(cusum.stats, max.cusum/sqrt(nNeg + 1))
    n <- cusum.stats$nNeg + 1
    p_val <- 1 - stats:::pkolmogorov(H/sqrt(n), size = n, exact = F)
    return(data.frame(Intercept = coef["Intercept", "EST"],
                      Slope = coef["Slope", "EST"],
                      Lin_test_p = p_val,
                      N_metingen = N))
  }
  else {
    return(data.frame(Intercept = NA, Slope = NA, Lin_test_p = NA, 
                      N_metingen = N))
  }
}

# calculate the passing bablock estimates of the data
paba_data <- skml %>%
  group_by(Bepaling, ptp, ctr, Methode) %>% 
  do(paba.reg.fun(.$ConsensusWaarde, .$Resultaat)) 

# add number of methods  
paba_data <- paba_data %>% 
  group_by(Bepaling, ptp, ctr) %>% 
  mutate(N_methodes = n_distinct(Methode)) %>% 
  ungroup()

# filtering criteria
paba_data_filt <- paba_data %>% 
  filter(N_metingen >= N_min) %>%   # meer dan of gelijk aan N_min metingen
  group_by(Bepaling, ptp, ctr) %>%  # indien meerdere methodes selecteer eerste
  slice(1)

write.csv(paba_data_filt, paste0("data/paba_data_", year, ".csv"), 
          row.names = FALSE)


# Correctie op methode niveau ---------------------------------------------


# Resultaten centrum 11 (referentie)
ptp11 <- skml %>% filter(ptp == 11) %>% 
  select(Bepaling, ctm, Resultaat) %>% 
  rename(Resultaat_11 = Resultaat) 

skml_w_ptp11 <- left_join(skml, ptp11, by = c("Bepaling", "ctm"))

# PaBa regressie per bepaling/methode
paba_data_per_methode_ptp11 <- skml_w_ptp11 %>%
  group_by(Bepaling, Methode) %>%
  do(paba.reg.fun(.$Resultaat, .$Resultaat_11)) 


# PaBa regressie data met methode zoals gekozen door methode per ctr
paba_correctiefactoren <- left_join(select(paba_data_filt, Bepaling, 
                                           ptp, ctr, Methode),
                                    paba_data_per_methode_ptp11,
                                    by = c("Bepaling", "Methode")) 

# # Combineren met PaBa data per methode
# paba_data_joined <- left_join(select(paba_data_eerste_methode,
#                                      -Lin_test_p, -N_methodes),
#                               select(paba_data_per_methode_filt, -Lin_test_p),
#                               by = c("Methode", "Bepaling"))
# 
# # Gesimuleerde PaBa data met correctie berekenen
# paba_joined_methode_corrected <- paba_data_joined %>%
#   mutate(Intercept = Slope.y*Intercept.x + Intercept.y) %>%
#   mutate(Slope = Slope.y*Slope.x) %>%
#   select(Bepaling, ptp, ctr, Methode, Intercept, Slope)
# 

write.csv(paba_correctiefactoren,
          paste0("data/paba_data_", year, "_correctiefactoren.csv"),
          row.names = FALSE)
