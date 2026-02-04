
# Functions to calculate continuous ldl cholesterol and ordinal and dichotomised cholesterol targets.

get_continuous_prediction_egfr <- function(data, dots_arguments){
  tc <- dots_arguments[["model_variables"]]["total_cholesterol"]
  hdl <- dots_arguments[["model_variables"]]["hdl"]
  triglycerides <- dots_arguments[["model_variables"]]["triglycerides"]
  friedewald <-  tc - hdl - (0.45 * triglycerides)
  if_else(data[[triglycerides]] < 4.5,
          true = eval(friedewald),
          false = NA,
          missing = NA)
}


get_ordinal_prediction_egfr <- function(data, dots_arguments){
  if_else (as.numeric(data[["continuous_prediction"]]) < 1.8,
           true = 1,
           false = if_else(
             as.numeric(data[["continuous_prediction"]]) >= 1.8 & as.numeric(data[["continuous_prediction"]]) < 2.6,
             true = 2,
             false = if_else(
               as.numeric(data[["continuous_prediction"]]) >= 2.6,
               true = 3,
               false = NA)
           ),
           missing = NA)
}


get_categorical_prediction_egfr <- function(data, dots_arguments){
  as.numeric(data[["continuous_prediction"]]) < 2.6
}