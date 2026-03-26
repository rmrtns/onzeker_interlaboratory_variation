library(dplyr)
library(tidyr)
library(mcr)
library(pROC)

# Minimal amount of observations for paba method.
N_min = 16 

# Read the merged skml file.
#   If file doesn't exist, run:
#   source("src/new skml import.R")
skml <- read.csv("data/skml_merged.csv")


paba.reg.fun <- function(ReferenceMethod, TestMethod){
  # Minimum sample size is 16
  N <-length(ReferenceMethod)
  if (N >= N_min) {
    PB.reg <- mcr::mcreg(ReferenceMethod,
                         TestMethod,
                         method.reg = "PBequi",
                         method.ci = "analytical")
    
    coef <- getCoefficients(PB.reg)
    cusum.stats <- calcCUSUM(PB.reg)
    H <- with(cusum.stats, max.cusum/sqrt(nNeg + 1))
    n <- cusum.stats$nNeg + 1
    p_val <- 1 - stats:::pkolmogorov(H/sqrt(n), size = n, exact = F)
    return(data.frame(
      Intercept = coef["Intercept", "EST"],
      Slope = coef["Slope", "EST"],
      Lin_test_p = p_val,
      N = N))
    } else {
    return(data.frame(Intercept = NA, Slope = NA, Lin_test_p = NA, N = NA))
  }
}

# Perfom the paba regression.
paba_regs_wide <- skml %>% 
  group_by(Bepaling, ptp, ctr) %>% 
  do(paba.reg.fun(.$ConsensusWaarde, .$Resultaat)) %>%
  pivot_wider(names_from = Bepaling, values_from = c(Intercept, Slope, 
                                                     Lin_test_p, N)) %>%
  mutate(name = paste("ptp", ptp, "ctr", ctr, sep = "_"))

# Output in csv file.
write.csv(paba_regs_wide, "data/skml_bias_table.csv", row.names = FALSE)

rm(list = ls())