
# Functions to calculate continuous egfr, categorical egfr and dichotomised egfr.
## Ethnicity is considered non-African American for all entries.

get_continuous_prediction_egfr <- function(data, dots_arguments){
  age <- dots_arguments[["model_variables"]]["age"]
  sex <- dots_arguments[["model_variables"]]["sex"]
  creatinine <- dots_arguments[["model_variables"]]["creatinine"]
  egfr_men <- expression(141 * (pmin((data[[creatinine]] / 88.4) / 0.9, 1) ** -0.411) * (pmax((data[[creatinine]] / 88.4) / 0.9, 1) ** -1.209) * (0.993 ** data[[age]]))
  egfr_women <- expression(141 * (pmin((data[[creatinine]] / 88.4) / 0.7, 1) ** -0.329) * (pmax((data[[creatinine]] / 88.4) / 0.7, 1) ** -1.209) * (0.993 ** data[[age]]) * 1.018)
  if_else(data[[sex]] == "1",
          true = eval(egfr_men),
          false = eval(egfr_women),
          missing = NA)
}


get_ordinal_prediction_egfr <- function(data, dots_arguments){
  cut_offs <- c(60, 90)
  
  cut(
    as.numeric(data[["continuous_prediction"]]),
    c(-Inf, cut_offs, Inf),
    labels = seq (0, length(cut_offs)),
    ordered_result = TRUE
  )
}


get_categorical_prediction_egfr <- function(data, dots_arguments){
  as.numeric(data[["continuous_prediction"]]) < 60
}