library(mcr)
library(dplyr)



paba.reg.fun <- function(ReferenceMethod, TestMethod,N){
  # Minimum sample size is N
  
  if (length(ReferenceMethod) >= N) {
    PB.reg <- mcr::mcreg(ReferenceMethod,
                         TestMethod,
                         method.reg = "PBequi",
                         method.ci = "analytical")
    
    coef <- getCoefficients(PB.reg)
    
    return(data.frame(
      Intercept = coef["Intercept", "EST"],
      Slope = coef["Slope", "EST"]))
  } else {
    return(data.frame(Intercept = NA, Slope = NA))
  }
}



# parameters 
N <- 16
data <- read.csv("data/skml_merged.csv")

# calculate the passing bablock estimates of the data
paba_data <- data %>%
  group_by(Bepaling, ptp, ctr) %>%
  do(paba.reg.fun(.$ConsensusWaarde, .$Resultaat, N)) 

write.csv(paba_data, "data/paba_data.csv", row.names = FALSE)