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

  for (variable in variables) {
    simulated_bias_per_variable <- data[[variables[variable]]]*as.numeric(bias_factors[variable]) + as.numeric(bias_intercepts[variable])
    simulated_bias_per_database <- simulated_bias_per_database %>% 
      mutate(!!sym(variables[variable]) := simulated_bias_per_variable)
  }

  return(simulated_bias_per_database)
}


# Redundant
# multiply_variable_by_factor <- function(original, factor){
#   return(original * factor)
# }

