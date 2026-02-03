# Functions to calculate the CoLab-score, either continous (linear predictor)
# ordinal (0 to 5) or binary (using a score of 5 as positive)

get_continuous_prediction_colab <- function(data, dots_arguments){
  
  RBC <- dots_arguments[["model_variables"]]["Erytrocyten_BV"]
  WBC <- dots_arguments[["model_variables"]]["Leukocyten_BV"]
  EOS <- dots_arguments[["model_variables"]]["EosAbs_BV"]
  BASO <- dots_arguments[["model_variables"]]["BasoAbs_BV"]
  BILI <- dots_arguments[["model_variables"]]["BilirubineTotaal_BV"]
  LD <- dots_arguments[["model_variables"]]["LD_BV"]
  AF <- dots_arguments[["model_variables"]]["AlkFosf_BV"]
  GGT <- dots_arguments[["model_variables"]]["GGT_BV"]
  ALB <- dots_arguments[["model_variables"]]["Albumine_BV"]
  CRP <- dots_arguments[["model_variables"]]["CRP_BV"]
  age <- dots_arguments[["model_variables"]]["age"]
  
  formula <- expression(
    -6.885000+
    data[[RBC]]*0.937900+
    data[[WBC]]*-0.129800+
    data[[EOS]]*-6.834000+
    data[[BASO]]*-47.70000+
    data[[BILI]]*-1.142000+
    data[[LD]]*5.369000+
    data[[AF]]*-3.114000+
    data[[GGT]]*0.360500+
    data[[ALB]]*-0.115600+
    data[[CRP]]*0.002560+
    data[[age]]*0.002275              
  )
  eval(formula)
}

get_ordinal_prediction_colab <- function(data, dots_arguments){
  as.numeric(data[["continuous_prediction"]]) < dots_arguments[["model_cut_off"]]
}


# get_ordinal_prediction_colab <- function(data, dots_arguments){
#   cut(data[["continuous_prediction"]], c(Inf, dots_arguments[["model_cut_off"]], -Inf), 
#       labels = seq(0, length(dots_arguments[["model_cut_off"]])), 
#       ordered_result = T)
#   
# }

    # get_ordinal_prediction_colab <- function(CoLab_lps,...){
    #   cutoffs <- c( -1.64, -2.34, -3.29, -4.03, -5.83)
    #   cut(CoLab_lps, c(Inf, cutoffs, -Inf), labels = seq(0, length(cutoffs)), 
    #       ordered_result = T)
    # }

get_binary_prediction_colab <- function(CoLab_lps,...){
  CoLab_lps >= -1.64
}





    # 
    # 
    # get_lp_prediction_CoLab <- function(data,dots_arguments){
    #   
    #   coefs <- c("(Intercept)" = -6.885000,
    #              "Erytrocyten_BV" = 0.937900,
    #              "Leukocyten_BV" = -0.129800,
    #              "EosAbs_BV" = -6.834000,
    #              "BasoAbs_BV" = -47.70000,
    #              "log10_BilirubineTotaal_BV" = -1.142000,
    #              "log10_LD_BV" = 5.369000,
    #              "log10_AlkFosf_BV" = -3.114000,
    #              "log10_GGT_BV" = 0.360500,
    #              "Albumine_BV" = -0.115600,
    #              "CRP_BV" = 0.002560,
    #              "age" = 0.002275)
    #   X <- as.data.frame(X.unscaled)
    #   
    #   coef.names.no.int <- names(coefs)["(Intercept)" != names(coefs)]
    #   
    #   # Remove log10_prefix as we input untransformed values
    #   coef.names.no.int.no.log <- sub("^log10_", "", coef.names.no.int)
    #   
    #   if (!all(coef.names.no.int.no.log %in% names(X.unscaled))) {
    #     stop(paste(c("Not all coef variables in X:",
    #                  coef.names.no.int[!coef.names.no.int %in% names(X)]), 
    #                collapse = " "))
    #     paste("Not all coef variables in X:",
    #           coef.names.no.int[!coef.names.no.int %in% names(X)])
    #   }
    #   X.ordered <- subset(X, select = coef.names.no.int.no.log)
    #   
    #   # Do log transform on log transformed variables
    #   log10cols <- coef.names.no.int.no.log[grep("log10", coef.names.no.int)]
    #   X.ordered[, log10cols] <- lapply(X.ordered[, log10cols], log10)
    #   lp <- rowSums(mapply('*', X.ordered, coefs[coef.names.no.int]))
    #   lp + coefs["(Intercept)"]
    # }

