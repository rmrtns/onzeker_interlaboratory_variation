library(mcr)
library(dplyr)

# minimal amount of observations for paba method
N_min = 16 

# read the merged skml file 
#   If file doesn't exist, run:
#   source("src/new skml import.R")
skml <- read.csv("data/skml_merged.csv")


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

write.csv(paba_data, "data/paba_data.csv", row.names = FALSE)