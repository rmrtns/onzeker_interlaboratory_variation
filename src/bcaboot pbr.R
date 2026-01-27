library(pROC)
library(mcr)
library(bcaboot)
source("src/create_test_data.R")

skml_merged <- read.csv("data/skml_merged.csv")
testdat <- read.csv("data/test_data.csv")


# Define functions --------------------------------------------------------


# Functions we need later
paba.reg.fun <- function(ReferenceMethod, TestMethod){
  
  PB.reg <- mcr::mcreg(ReferenceMethod,
                       TestMethod,
                       method.reg = "PBequi",
                       method.ci = "analytical")
  
  return(t(getCoefficients(PB.reg))[1,])
}

getStats <- function(outcome, prediction, threshold, ...){
  roc_obj <- roc(outcome, prediction, ...)
  auc_value <- auc(roc_obj)
  coords_values <- coords(roc_obj, threshold, 
                          ret = c("sensitivity", "specificity", "ppv", "npv"))
  stats <- c(auc = auc_value, coords_values)
  return(unlist(stats))
}

bootStats <- function(df, ind) {
  stats <- getStats(df$mgfr_60[ind], df$egfr[ind], 60,
                    direction = ">", levels = c(0,1))
  return(unlist(stats))
}

addBias <- function(orig_dat, skml_dat, sample_pbreg = F) {
  if (sample_pbreg) {
    skml_dat <- skml_dat[sample(nrow(skml_dat), nrow(skml_dat), 
                                replace = TRUE), ]
  }
  pbreg <- paba.reg.fun(skml_dat$ConsensusWaarde, 
                        skml_dat$Resultaat)
  creat_biased <- pbreg[1] + pbreg[2] * orig_dat$creatinine
  egfr <- calculate_egfr(data.frame(creatinine = creat_biased, 
                                    age = orig_dat$age, 
                                    sex = orig_dat$sex))
  biased_dat <- orig_dat
  biased_dat$creatinine <- creat_biased
  biased_dat$egfr <- egfr
  biased_dat$egfr_60 <- ifelse(egfr < 60, 1, 0)
  return(biased_dat)
}

bootBias <- function(orig_dat, ind, skml_dat, sample_pbreg = F){
  biased_dat <- addBias(orig_dat[ind, ], skml_dat, sample_pbreg)
  stats <- getStats(biased_dat$mgfr_60, biased_dat$egfr, 60,
                    direction = ">", levels = c(0,1))
  return(unlist(stats))
}


# Bootstrap samples -------------------------------------------------------

library(boot)
set.seed(123)
ptp1190_skml <- subset(skml_merged, ptp == 1190 & 
                       ctr == 1 & Bepaling == "Kreatinine")

boottest <- boot(testdat, bootStats, R = 2000)
bootptp <- boot(testdat, bootBias, R = 2000, skml_dat = ptp1190_skml, 
                sample_pbreg = F)
bootptp_pbreg <- boot(testdat, bootBias, R = 2000, skml_dat = ptp1190_skml, 
                      sample_pbreg = T)

boot.ci(boottest)
boot.ci(bootptp)
boot.ci(bootptp_pbreg)


library(ggplot2)
tAUC <- rbind(data.frame(AUC = bootptp$t[,1], type = "ptpdat"),
              data.frame(AUC = bootptp_pbreg$t[,1], type = "ptp_pbreg"))


ggplot(tAUC, aes(x = AUC, fill = type)) + 
  geom_density(alpha = 0.5) + 
  scale_fill_brewer(palette = "Dark2", name = "Type",
                    labels = list("ptpdat" = "W/o PaBa sampling", 
                                  "ptp_pbreg" = "With PaBa sampling")) + 
  theme_minimal()


set.seed(1)
testdat100 <- testdat[sample(nrow(testdat), 100),]
boottest100 <- boot(testdat100, bootStats, R = 2000)
bootptp100 <- boot(testdat100, bootBias, R = 2000, skml_dat = ptp1190_skml, 
                   sample_pbreg = F)
bootptp_pbreg100 <- boot(testdat100, bootBias, R = 2000, skml_dat = ptp1190_skml, 
                         sample_pbreg = T)
boot.ci(bootptp)
boot.ci(bootptp100)
boot.ci(bootptp_pbreg100)


tAUC100 <- rbind(data.frame(AUC = bootptp$t[,1], type = "ptpdat"),
                 data.frame(AUC = bootptp100$t[,1], type = "ptpdat100"),
                 data.frame(AUC = bootptp_pbreg100$t[,1], type = "ptpdat100_sample"))

ggplot(tAUC100, aes(x = AUC, fill = type)) + 
  geom_density(alpha = 0.5) + 
  scale_fill_brewer(palette = "Set1", name = "Type",
                    labels = list("ptpdat" = "500 patients in population", 
                                  "ptpdat100" = "100 patients in population",
                                  "ptpdat100_sample" = "100 patients in population\n with PaBa sampling")) + 
  theme_minimal()

