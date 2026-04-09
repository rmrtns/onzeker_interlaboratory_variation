# Functions to calculate continuous ldl cholesterol and ordinal and dichotomised cholesterol targets.
# LDL cholesterol is converted from mmol/L to mg/dL.
# LDL targets are not converted but set at 70 and 100 mg/dL according to current guidelines.

ldl_si_to_conventional <- 38.67

get_continuous_prediction_ldl <- function(data, dots_arguments){
  tc <- dots_arguments[["model_variables"]]["total_cholesterol"]
  hdl <- dots_arguments[["model_variables"]]["hdl"]
  triglycerides <- dots_arguments[["model_variables"]]["triglycerides"]
  friedewald <-  (data[[tc]] - data[[hdl]] - (0.45 * data[[triglycerides]])) * ldl_si_to_conventional # Condition triglycerides < 4.5 mmol/L via dataset
}


get_ordinal_prediction_ldl <- function(data, dots_arguments){
  cut_offs <- c(70, 100)
  
  cut(
    as.numeric(data[["continuous_prediction"]]),
    c(-Inf, cut_offs, Inf),
    labels = seq (0, length(cut_offs)),
    ordered_result = TRUE
  )
}


get_categorical_prediction_ldl <- function(data, dots_arguments){
  as.numeric(data[["continuous_prediction"]]) < 100
}
