source('src/paba_regression.R')
library(mcr)

# parameters 
N <- 16
data <- read.csv("data/skml_merged.csv")

# calculate the passing bablock estimates of the data
paba_data <- data %>%
  group_by(Bepaling, ptp, ctr) %>%
  do(paba.reg.fun(.$ConsensusWaarde, .$Resultaat, N)) 

write.csv(paba_data, "data/paba_data.csv", row.names = FALSE)