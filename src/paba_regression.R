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


# paba.reg.fun <- function(ReferenceMethod, TestMethod,N){
#   # Minimum sample size is N
#   if (length(ReferenceMethod) >= N) {
#     PB.reg <- mcr::mcreg(ReferenceMethod,
#                          TestMethod,
#                          method.reg = "PBequi",
#                          method.ci = "analytical")
#     
#     coef <- getCoefficients(PB.reg)
#     
#     return(data.frame(
#       var = paste("y ~ ", Slope = round(coef["Slope", "EST"],3), "*x+", round(coef["Intercept", "EST"],3), sep = ""
#       )))
#   } else {
#     return(data.frame(Intercept = NA, Slope = NA))
#   }
# }

