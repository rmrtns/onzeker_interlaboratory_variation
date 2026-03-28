library(dplyr)

create_constant_data <- function(data, variables){
  constant_data <- data %>%
    select(-all_of(variables))
}


simulate_bias <- function(data, identifier, variables, bias_factors, bias_intercepts){
  simulated_bias_per_database <- data[identifier]
  for (variable in variables) {
    simulated_bias_per_variable <- pmax(data[[variables[variable]]]*as.numeric(bias_factors[[variable]]) + as.numeric(bias_intercepts[[variable]]), 0)
    simulated_bias_per_database[[variable]] <- simulated_bias_per_variable
  }
  return(simulated_bias_per_database)
}