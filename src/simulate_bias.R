library(dplyr)

create_constant_data <- function(data, variables){
  constant_data <- data %>%
    select(-all_of(variables))
}


simulate_bias <- function(data, identifier, variables, bias_factors, bias_intercepts){
    if (is.null(names(variables)) || any(names(variables) == "")) {
    variables_map <- setNames(as.character(variables), as.character(variables))
  } else {
    variables_map <- setNames(as.character(unname(variables)), names(variables))
  }
  
  simulated_bias_per_database <- data[identifier]
  for (variable_name in names(variables_map)) {
    input_col <- variables_map[[variable_name]]
    simulated_bias_per_variable <- pmax(data[[variables_map[[variable_name]]]]*as.numeric(bias_factors[[variable_name]]) + as.numeric(bias_intercepts[[variable_name]]), 0)
    simulated_bias_per_database[[input_col]] <- simulated_bias_per_variable
  }
  return(simulated_bias_per_database)
}