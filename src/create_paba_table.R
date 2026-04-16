library(mcr)
library(dplyr)

# minimal amount of observations for paba method
N_min = 16 

# read the merged skml file 
#   If file doesn't exist, run:
#   source("src/new skml import.R")
skml <- read.csv("data/skml_merged.csv")


paba.reg.fun <- function(ReferenceMethod, TestMethod, N_min){
  # Minimum sample size is 16
  N <- length(ReferenceMethod)
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
    return(data.frame(N = N,
                      Intercept = coef["Intercept", "EST"],
                      Slope = coef["Slope", "EST"],
                      Lin_test_p = p_val))
  } else {
    return(data.frame(Intercept = NA, Slope = NA, Lin_test_p = NA, N = NA))
  }
}


# calculate the passing bablock estimates of the data
paba_data <- skml %>%
  group_by(Bepaling, ptp, ctr) %>%
  do(paba.reg.fun(.$ConsensusWaarde, .$Resultaat, N_min)) 

write.csv(paba_data, "data/paba_data.csv", row.names = FALSE)