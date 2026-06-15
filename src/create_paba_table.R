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
  N <- length(ReferenceMethod)
  PB.reg <- try(mcr::mcreg(ReferenceMethod, TestMethod,
                           method.reg = "PBequi",
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



paba_methode <- skml %>%
  group_by(Bepaling, Methode) %>%
  do(paba.reg.fun(.$ConsensusWaarde, .$Resultaat, N_min)) 


methode_per_ctr <- skml %>% group_by(ptp, ctr, Bepaling) %>% 
  filter(Methode == Methode[1]) %>% 
  slice(1) %>% 
  select(ptp, ctr, Bepaling, Methode)


paba_data_met_methode <- left_join(paba_data, methode_per_ctr,
                                   by = c("Bepaling", "ptp", "ctr"))

paba_joined_methode <- left_join(paba_data_met_methode, 
                                 select(paba_methode, -N, -Lin_test_p), 
                                 by = c("Methode", "Bepaling"))

paba_joined_methode_corrected <- paba_joined_methode %>% 
  mutate(Intercept = Slope.y*Intercept.x + Intercept.y) %>% 
  mutate(Slope = Slope.y*Slope.x) %>% 
  select(Bepaling, ptp, ctr, N, Intercept, Slope, Lin_test_p)

write.csv(paba_joined_methode_corrected, 
          "data/paba_data_corrected.csv", row.names = FALSE)
