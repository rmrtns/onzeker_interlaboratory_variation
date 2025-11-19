library(dplyr)
library(mcr)
library(pROC)
source("src/predict_CoLab.R")


# Import and pre-process --------------------------------------------------
                             
dataZMC <- read.csv("data/CoLab_externe_validatie_Zuyderland.csv")
dataZMC$CoLab_binary <- get_binary_prediction_CoLab(dataZMC$CoLab_lp)

# De-transform log10 variables and removed log10_ from columns names
log10_cols <- grep("log10", names(dataZMC), value = T)
dataZMC_nolog <- dataZMC %>%
  mutate(across(all_of(log10_cols), ~ 10 ^ .x)) %>% 
  rename_with(~ gsub("log10_", "", log10_cols), .cols = log10_cols) %>% 
  select(!c("CoLab_lp", "CoLab_score"))

skml <- read.csv("data/skml_merged.csv")
# Select one cluster from each participant
skml_one_ptp <- skml %>% 
  group_by(Bepaling, ptp) %>% 
  filter(ctr == min(ctr))

# PaBa regression ---------------------------------------------------------


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

paba_regs <- skml_one_ptp %>% 
  group_by(Bepaling, ptp, ctr) %>% 
  do(paba.reg.fun(.$ConsensusWaarde, .$Resultaat))


# Create datasets ---------------------------------------------------------


# Add bias (slope and intercept) based on Passing-Bablok regression
#
# orig_dataset         is the (reference) dataset
# pb_reg_data_ctr      is the PaBa regression data from a specific participant 
#                      & cluster
# variable_dictionary  is a named vector where the elements are equal to the
#                      Bepaling string in the SKML data and the names correspond
#                      to the covariate string in the orig_dataset, e.g.
#                      "AlkFosf_BV" = "Alk. Fosfatase", if the covariate
#                      does not appear in the SKML data it should be NA, e.g.
#                      "EosAbs_BV" = NA.

createBiasedData <- function(orig_dataset, skml_paba_reg_ptp_ctr,
                             variable_dictionary) {
  
  biased_dataset <- orig_dataset
  # Do for each variable 
  for (variable in names(variable_dictionary)) {
    sklm_name <- variable_dictionary[[variable]]
    # Check if variable is present in SKML data
    if (!is.na(sklm_name) & sklm_name %in% skml_paba_reg_ptp_ctr$Bepaling) {
      pbreg_bepaling <- subset(skml_paba_reg_ptp_ctr, Bepaling == sklm_name)
      orig_results <- orig_dataset[, variable]
      # Add center specific bias (slope & intercept )
      biased_dataset[, variable] <- pbreg_bepaling[['Intercept']] + 
        pbreg_bepaling[['Slope']] * orig_results
    }
  }
  return(biased_dataset)
}

# Translation of CoLab variables to SKML bepalingen

covars_w_skml_name <- c("Erytrocyten_BV" = "Erytrocyten", 
                        "Leukocyten_BV" = "Leukocyten",
                        "EosAbs_BV" = NA,
                        "BasoAbs_BV" = NA, 
                        "BilirubineTotaal_BV" = "Bilirubine",
                        "LD_BV" = "LD",
                        "AlkFosf_BV" = "Alk. Fosfatase",
                        "GGT_BV" = "Gamma-GT",
                        "Albumine_BV" = "Albumine",
                        "CRP_BV" = "CRP",
                        "age" = NA)

# Split PaBa-regression results by participant 
paba_regs_split <- split(paba_regs, paba_regs$ptp)

# Create biased data by participant
ptp_biasedData <- lapply(paba_regs_split, createBiasedData, 
                         orig_dataset = dataZMC_nolog,
                         variable_dictionary = covars_w_skml_name)

# Calculate CoLab-lp and score by dataset
ptp_results <- lapply(ptp_biasedData, function(x){ 
  x$CoLab_lp <- get_lp_prediction_CoLab(x)
  x$CoLab_score <- get_ordinal_prediction_CoLab(x$CoLab_lp)
  x$CoLab_binary <- get_binary_prediction_CoLab(x$CoLab_lp)
  x})


ptp_rocs <- lapply(ptp_results, function(x) 
  roc(x, "outcome", "CoLab_score", direction = "<"))

ptp_aucs <- sapply(ptp_rocs, function(x) x$auc)
quantile(unlist(ptp_aucs), probs = c(0.025, 0.5, 0.975))

ptp_coords <- lapply(ptp_rocs, coords, x = 5)
ptp_coords <- do.call(rbind, ptp_coords)
quantile(ptp_coords$specificity, probs = c(0.025, 0.5, 0.975))
quantile(ptp_coords$sensitivity, probs = c(0.025, 0.5, 0.975))

orig_roc <- roc(dataZMC, "outcome", "CoLab_score", direction = "<")
orig_roc$auc
coords(orig_roc, x = 5)
