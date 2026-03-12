library(dplyr)
library(mcr)
library(pROC)
source("src/predict_CoLab.R")


# Import and pre-process --------------------------------------------------

                             
skml <- read.csv("data/skml_merged.csv")
# If file doesn't exist, run:
# source("src/new skml import.R")


dataZMC <- read.csv("data/CoLab_externe_validatie_Zuyderland.csv")
dataZMC$CoLab_binary <- get_binary_prediction_CoLab(dataZMC$CoLab_lp)


# De-transform log10 variables and removed log10_ from columns names
log10_cols <- grep("log10", names(dataZMC), value = T)
dataZMC_nolog <- dataZMC %>%
  mutate(across(all_of(log10_cols), ~ 10 ^ .x)) %>% 
  rename_with(~ gsub("log10_", "", log10_cols), .cols = log10_cols) 

dataZMC_nolog$CoLab_lp <- get_lp_prediction_CoLab(dataZMC_nolog)
dataZMC_nolog$CoLab_score <- get_ordinal_prediction_CoLab(dataZMC_nolog$CoLab_lp)
dataZMC_nolog$CoLab_binary <- get_binary_prediction_CoLab(dataZMC_nolog$CoLab_lp)




# Select one cluster for each bepaling for each center
skml_one_ptp <- skml %>% 
  group_by(Bepaling, ptp) %>% 
  filter(ctr == min(ctr))


# PaBa regression ---------------------------------------------------------


paba.reg.fun <- function(ReferenceMethod, TestMethod){
  # Minimum sample size is 16
  if (length(ReferenceMethod) >= 16) {
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

paba_regs <- skml_one_ptp %>% 
  group_by(Bepaling, ptp, ctr) %>% 
  do(paba.reg.fun(.$ConsensusWaarde, .$Resultaat))


# Paba regs wrt ZMC -------------------------------------------------------

# ZMC is ptp 11, use as reference
ref_ptp_no <- 11

# Convert PaBa regression to using the ZMC dataset as reference
ref_cent <- subset(paba_regs, ptp == ref_ptp_no)

ref_cent <- ref_cent %>% 
  rename("Ref_Intercept" = "Intercept") %>% 
  rename("Ref_Slope" = "Slope") %>% 
  ungroup() %>% 
  select(-ptp, -ctr)

# New intercept = (int_ptp - int_ref / slope_ref)
# New slope = slope_ptp/slope_ref
paba_regs_new <- left_join(subset(paba_regs, ptp != ref_ptp_no), 
                           ref_cent, by = "Bepaling") %>% 
  mutate(New_Intercept = -((Slope*Ref_Intercept)/Ref_Slope) + Intercept) %>% 
  mutate(New_Slope = Slope/Ref_Slope)


# Create datasets ---------------------------------------------------------


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
paba_regs_split <- split(paba_regs_new, paba_regs_new$ptp)

# Check if all CoLab vars are in SKML paba reg set
N_bepalingen_uit_CoLab <- sapply(paba_regs_split, function(x) {
  bepalingen <- x$Bepaling[!is.na(x$Intercept)]
  sum(covars_w_skml_name[!is.na(covars_w_skml_name)] %in% bepalingen)})

# Centra met alle bepalinge uit CoLab (8)
compl_ptp <- names(which(N_bepalingen_uit_CoLab == 8))


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
                             variable_dictionary, 
                             int_colname, 
                             slope_colname) {
  
  biased_dataset <- orig_dataset
  # Do for each variable 
  for (variable in names(variable_dictionary)) {
    sklm_name <- variable_dictionary[[variable]]
    # Check if variable is present in SKML data
    if (!is.na(sklm_name) & sklm_name %in% skml_paba_reg_ptp_ctr$Bepaling) {
      pbreg_bepaling <- subset(skml_paba_reg_ptp_ctr, Bepaling == sklm_name)
      orig_results <- orig_dataset[, variable]
      # Add center specific bias (slope & intercept )
      biased_dataset[, variable] <- pbreg_bepaling[[int_colname]] + 
        pbreg_bepaling[[slope_colname]] * orig_results
    }
  }
  return(biased_dataset)
}



# Create biased data by participant
ptp_biasedData <- lapply(paba_regs_split[compl_ptp], createBiasedData, 
                         orig_dataset = dataZMC_nolog,
                         variable_dictionary = covars_w_skml_name,
                         int_colname = "New_Intercept",
                         slope_colname = "New_Slope")

# Calculate CoLab-lp and score by dataset
ptp_results <- lapply(ptp_biasedData, function(x){ 
  x$CoLab_lp <- get_lp_prediction_CoLab(x)
  x$CoLab_score <- get_ordinal_prediction_CoLab(x$CoLab_lp)
  x$CoLab_binary <- get_binary_prediction_CoLab(x$CoLab_lp)
  x})


# Stats -------------------------------------------------------------------


# Stats ontwikkelcentrum
orig_roc <- roc(dataZMC, "outcome", "CoLab_score", direction = "<")
orig_roc$auc
coords(orig_roc, x = 5,
       ret =  c("sensitivity", "specificity", "ppv", "npv", 
                "tp", "tn", "fp", "fn"))

# Stats alle centra
ptp_rocs <- lapply(ptp_results, function(x) 
  roc(x, "outcome", "CoLab_score", direction = "<", levels = c(F, T)))

ptp_aucs <- sapply(ptp_rocs, function(x) x$auc)
q_auc <- quantile(unlist(ptp_aucs), probs = c(0.025, 0.5, 0.975))
cat(sprintf("%.3f [%.3f - %.3f]", q_auc[2], q_auc[1], q_auc[3]))

ptp_coords <- lapply(ptp_rocs, coords, x = 5, 
                     ret =  c("sensitivity", "specificity", "ppv", "npv", 
                              "tp", "tn", "fp", "fn"))
df.ptp_coords <- do.call(rbind, ptp_coords)

fancy_print_qs <- function(result_vec){
  qs <- quantile(result_vec, probs = c(0.025, 0.5, 0.975))
  cat(sprintf("%.3f [%.3f - %.3f]", qs[2], qs[1], qs[3]))
}

fancy_print_qs(df.ptp_coords$sensitivity)
fancy_print_qs(df.ptp_coords$specificity)
fancy_print_qs(df.ptp_coords$ppv)
fancy_print_qs(df.ptp_coords$npv)
fancy_print_qs(df.ptp_coords$tp)
fancy_print_qs(df.ptp_coords$tn)
fancy_print_qs(df.ptp_coords$fp)
fancy_print_qs(df.ptp_coords$fn)

getDiscordantPreds <- function(orig_score, new_score){
  (sum(orig_score != new_score)/length(orig_score))*100
}

disc_pairs <- sapply(ptp_results, function(x) 
  getDiscordantPreds(dataZMC_nolog$CoLab_binary, x$CoLab_binary))

fancy_print_qs(disc_pairs)


# Case study --------------------------------------------------------------


low_sens <- ptp_results[[1]]

low_sens$CoLab_binary_ZMC <- dataZMC$CoLab_binary
low_sens$CoLab_score_ZMC <- dataZMC$CoLab_score
roc_low_sens <- roc(low_sens, "outcome", "CoLab_score", direction = "<")
roc_low_sens$auc

coords(roc_low_sens, x = 5, 
       ret = c("sensitivity", "specificity", "ppv", "npv", 
               "tp", "tn", "fp", "fn"))

getDiscordantPreds(dataZMC_nolog$CoLab_binary, low_sens$CoLab_binary)

