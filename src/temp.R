library(mcr)
library(dplyr)

tom <- readRDS("data/SKML_bias_data_Tom.RDS")
skml_merged <- read.csv("data/skml_merged.csv")






paba.reg.fun <- function(ReferenceMethod, TestMethod){
  
  PB.reg <- mcr::mcreg(ReferenceMethod,
                       TestMethod,
                       method.reg = "PBequi",
                       method.ci = "analytical")
  
  coef <- getCoefficients(PB.reg)
  
  return(data.frame(
    Intercept = coef["Intercept", "EST"],
    Slope = coef["Slope", "EST"]
  ))
  
}

ruben <- ruben_skml %>% 
  group_by(Bepaling, ptp, ctr) %>% 
  do(paba.reg.fun(.$ConsensusWaarde, .$Resultaat))

tom <- subset(tom, !is.na(B))

with(ruben, paste(ptp, ctr, Bepaling))[!with(ruben, paste(ptp, ctr, Bepaling)) %in% with(tom, paste(ptp, ctr, Bepaling))]

merged <- merge(ruben, tom, by = c("Bepaling", "ptp", "ctr"))


merged[which(merged$A != merged$Slope),]

a <- subset(ruben_skml, Bepaling == "LDL-Cholesterol" & ptp == 6 & ctr == 1)




a_new <- a %>% filter(ConsensusMethode != "ALTM")

paba.reg.fun(a_new$ConsensusWaarde, a_new$Resultaat)