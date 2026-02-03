# Controleren:
## Aantal [ bij data[[identifier]]]
## Aantal [] bij data[[variables[variable_index]]]
## Berekeningen per variabele waarbij variabele wordt gezienn als vector


create_constant_data <- function(data, variables){
  constant_data <- data %>%
    select(-all_of(variables))
}

simulate_bias <- function(data, identifier, variables, bias_factors, bias_intercepts){ 
  simulated_bias_per_database <- data.frame(identifier = data[[identifier]])
  simulated_bias_per_database <- simulated_bias_per_database %>% rename(!!sym(identifier) := identifier)
  for (variable_index in 1:length(variables)) {
    simulated_bias_per_variable <- data[[variables[variable_index]]]*bias_factors[variable_index] + bias_intercepts[variable_index]
    simulated_bias_per_database <- simulated_bias_per_database %>% 
      mutate(!!sym(variables[variable_index]) := simulated_bias_per_variable)
  }
  return(simulated_bias_per_database)
}


# Redundant
# multiply_variable_by_factor <- function(original, factor){
#   return(original * factor)
# }

