library(dplyr)
library(tidyr)
library(mcr)
library(pROC)

# functions 
paba.reg.fun <- function(ReferenceMethod, TestMethod){
  # Minimum sample size is 16
  if (length(ReferenceMethod) >= N) {
    PB.reg <- mcr::mcreg(ReferenceMethod,
                         TestMethod,
                         method.reg = "PBequi",
                         method.ci = "analytical")
    
    coef <- getCoefficients(PB.reg)
    cumsum.stats <- calcCUSUM(PB.reg)
    H <- with(cumsum.stats, max.cusum/sqrt(nNeg + 1))
    return(data.frame(
      Intercept = coef["Intercept", "EST"],
      Slope = coef["Slope", "EST"],
      H = round(H, 2),
      Lin.test.reject = H >= 1.36))     # H <= 1.36 for linearity
  } else {
    return(data.frame(Intercept = NA, Slope = NA))
  }
}

# read the merged skml file 
#   If file doesn't exist, run:
#   source("src/new skml import.R")
skml <- read.csv("data/skml_merged.csv")

# minimal amount of observations for paba method
N = 16 

# perfom the paba regression
paba_regs_wide <- skml %>% 
  group_by(Bepaling, ptp, ctr) %>% 
  do(paba.reg.fun(.$ConsensusWaarde, .$Resultaat)) %>%
  pivot_wider(names_from = Bepaling, values_from = c(Intercept, Slope, H, Lin.test.reject)) %>%
  mutate(name = paste("ptp", ptp, "ctr", ctr, sep = "_"))

# output in csv file
write.csv(paba_regs_wide, "data/skml_bias_table.csv", row.names = FALSE)

rm(list = ls())





