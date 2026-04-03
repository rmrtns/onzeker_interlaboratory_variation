# Functions to calculate continuous ldl cholesterol and ordinal and dichotomised cholesterol targets.

get_continuous_prediction_ldl <- function(data, dots_arguments){
  tc <- dots_arguments[["model_variables"]]["total_cholesterol"]
  hdl <- dots_arguments[["model_variables"]]["hdl"]
  triglycerides <- dots_arguments[["model_variables"]]["triglycerides"]
  friedewald <-  data[[tc]] - data[[hdl]] - (0.45 * data[[triglycerides]]) # Condition triglycerides < 4.5 mmol/L via dataset
}


get_ordinal_prediction_ldl <- function(data, dots_arguments){
  cut_offs <- c(1.8, 2.6)
  
  cut(
    as.numeric(data[["continuous_prediction"]]),
    c(-Inf, cut_offs, Inf),
    labels = seq (0, length(cut_offs)),
    ordered_result = TRUE
  )
}


get_categorical_prediction_ldl <- function(data, dots_arguments){
  as.numeric(data[["continuous_prediction"]]) < 2.6
}
