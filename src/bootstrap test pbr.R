library(pROC)
library(mcr)
source("src/create_test_data.R")
skml_merged <- read.csv("data/skml_merged.csv")
testdat <- read.csv("data/test_data.csv")


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
# Bootstrap function for testdat
bootstrapStats <- function(mgfr_60, egfr, N_samples = 100) {
  boot_mat <- sapply(seq_len(N_samples), function(i) {
    idx <- sample(length(mgfr_60), replace = TRUE)
    stats <- getStats(mgfr_60[idx], egfr[idx], 60,
                      direction = ">", levels = c(0,1))
    unlist(stats)
  })
  # sapply returns a matrix of shape (n_stats × N_samples), so transpose:
  boot_df <- as_tibble(t(boot_mat))
  return(boot_df)
}

# Bootstrap function including pbreg
bootstrapStats_inclPbreg <- function(i, skml_dat, testdat, N) {
  idx   <- sample(nrow(skml_dat), replace = TRUE)
  samp  <- skml_dat[idx, ] 
  pbreg <- paba.reg.fun(skml_dat[idx, ]$ConsensusWaarde, 
                        skml_dat[idx, ]$Resultaat)
  creat_biased <- pbreg[1] + pbreg[2] * testdat$creatinine
  egfr <- calculate_egfr(data.frame(creatinine = creat_biased, 
                                    age = testdat$age, 
                                    sex = testdat$sex))
  stats <- bootstrapStats(testdat$mgfr_60, egfr, N_samples = N)
}



# Run procedure for single participant -------------------------------------

set.seed(123)
skml_ptp_kreat <- skml_merged %>% filter(ptp == 1190, ctr == 1,
                                         Bepaling == "Kreatinine")

ptp_pbreg_kreat <- with(skml_ptp_kreat, paba.reg.fun(ConsensusWaarde,
                                                     Resultaat))
ptp_kreat <- ptp_pbreg_kreat[1]  + ptp_pbreg_kreat[2] * testdat$creatinine

ptp_egfr <- calculate_egfr(data.frame(creatinine = ptp_kreat, 
                                      age = testdat$age, 
                                      sex = testdat$sex))

ptp_boot_df <- bootstrapStats(testdat$mgfr_60, ptp_egfr, N_samples = 5000)
ind <- sample(length(ptp_egfr), 200)
ptp_boot_df_downsampled <- bootstrapStats(testdat$mgfr_60[ind], ptp_egfr[ind], 
                                          N_samples = 5000)

# Now we also bootstrap the pbreg
N_samples = 500
ptp_boot_pbreg_mat <- lapply(seq_len(N_samples), bootstrapStats_inclPbreg,
                             skml_dat = skml_ptp_kreat, 
                             testdat = testdat[ind,], N = 500)

ptp_boot_pbreg_df <- do.call(rbind, ptp_boot_pbreg_mat)




# Combine all three methods
ptp_boot_combined <- rbind(
  cbind(ptp_boot_df, method = "no_pbreg"),
  cbind(ptp_boot_pbreg_df, method = "pbreg"),
  cbind(ptp_boot_df_downsampled, method = "no_pbreg_downsampled")
)
saveRDS(ptp_boot_combined, file = "data/ptp1190_boot_combined.rds")


# Bootstrap ---------------------------------------------------------------


library(boot)

addBias <- function(orig_dat, skml_dat) {
  pbreg <- paba.reg.fun(skml_dat$ConsensusWaarde, 
                        skml_dat$Resultaat)
  creat_biased <- pbreg[1] + pbreg[2] * orig_dat$creatinine
  egfr <- calculate_egfr(data.frame(creatinine = creat_biased, 
                                    age = orig_dat$age, 
                                    sex = orig_dat$sex))
  orig_dat$creatinine <- creat_biased
  orig_dat$egfr <- egfr
  orig_dat$egfr_60 <- ifelse(egfr < 60, 1, 0)
  return(orig_dat)
  
}

bootStats <- function(df, ind) {
  stats <- getStats(df$mgfr_60[ind], df$egfr[ind], 60,
                    direction = ">", levels = c(0,1))
  return(unlist(stats))
}

bootStats_w_pbReg <- function(df, ind, skml_dat) {
  df <- addBias(df, skml_dat)
  stats <- getStats(df$mgfr_60[ind], df$egfr[ind], 60,
                    direction = ">", levels = c(0,1))
  return(unlist(stats))
}




ptp1190_dat <- addBias(testdat, skml_merged %>% filter(ptp == 1190, ctr == 1))


boot(testdat, bootStats, R = 500)
boot(testdat, bootStats, R = 500, skml_dat = skml_merged %>% filter(ptp == 1190, ctr == 1))

