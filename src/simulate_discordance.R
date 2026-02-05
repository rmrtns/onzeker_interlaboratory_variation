# Wijzigingen:
## Toevoegen simulate_bias_induced_discordance
## Recode_zero verplaatsen: 
### In geval van bias leidt de situatie van 0 waarnemingen voor een specifieke categorie potentieel tot NA en mogelijk een foutieve berekening van het percentage discordante waarnemingen.
### Bij de simulatie van variatie gebeurt dit niet, omdat door het samplen volgens een normale verdeling, de oorspronkelijke waarneming altijd voorkomt.
## Categorical_prediction vervangen door dichotomous_prediction.
## summarise_discordance_measures toevoegen aan markdown.
## get_variable_names_from_summary_of_discordance_measures_as_vector toevoegen aan markdown.
## Hoe macro-rmse berekenen, bij zowel continue als ordinale variabelen?

library(tidyr)
library(boot)
library(stringr)
# library(broom)
library(dplyr) # load after xgboost to prevent masking slice function from dplyr
library(purrr)

source('src/simulate_bias.R', local = bias <- new.env())
source('src/save_database.R', local = save_database <- new.env())


simulate_bias_induced_discordance <- function(data, identifier, variables, bias_factors, bias_intercepts, laboratory, continuous_prediction_function, ordinal_prediction_function, categorical_prediction_function, ...){ 
    stopifnot(nrow(bias_factors) == nrow(bias_intercepts))
    is_prediction_function_entered(continuous_prediction_function, ordinal_prediction_function, categorical_prediction_function)
    predict_continuous <- create_pointer_to_prediction_function(continuous_prediction_function)
    predict_ordinal <- create_pointer_to_prediction_function(ordinal_prediction_function)
    predict_categorical <- create_pointer_to_prediction_function(categorical_prediction_function)
    dots_arguments <- list(...)
    list_of_simulated_discordances <- list()
    reference_predictions <- get_reference_predictions(data, identifier, predict_continuous, predict_ordinal, predict_categorical, dots_arguments)
    constant_data <- bias$create_constant_data(data, variables)
    for (row in 1:nrow(bias_factors)){
      simulated_data <- bias$simulate_bias(data, identifier, variables, bias_factors[[variables]][row], bias_intercepts[[variables]][row]) 
      combined_data <- left_join(constant_data, simulated_data, by = c(identifier))
      combined_data_with_predictions <- get_predictions(combined_data, identifier, predict_continuous, predict_ordinal, predict_categorical, dots_arguments)
      # save_database$save_database_data(combined_data_with_predictions)
      simulated_predictions <- combined_data_with_predictions %>% select(all_of(identifier), continuous_prediction, ordinal_prediction, categorical_prediction)
      simulated_and_reference_predictions <- left_join(simulated_predictions, reference_predictions, by = identifier)
      simulated_discordances_per_record <- get_discordance_measures(simulated_and_reference_predictions, identifier)
      list_of_simulated_discordances[[bias_factors[[laboratory]][row]]] <- cbind(laboratory = bias_factors[[laboratory]][row], simulated_discordances_per_record)
    }
    simulated_discordances <- bind_rows(list_of_simulated_discordances)
    return(simulated_discordances)
  }


is_prediction_function_entered <- function(continuous_prediction_function, ordinal_prediction_function, categorical_prediction_function){
  if (typeof(continuous_prediction_function) != "closure" & typeof(ordinal_prediction_function) != "closure" & typeof(categorical_prediction_function) != "closure"){
    stop("Please enter function for continuous, ordinal and/or categorical predictions", call. = FALSE)
  }
}


create_pointer_to_prediction_function <- function(prediction_function){
  if (typeof(prediction_function) != "closure"){
    return(match.fun(dummy_predict))
  } else {
    return(match.fun(prediction_function))
  }
}


dummy_predict <- function(data, dots_arguments){
  return(NA)
}


get_reference_predictions <- function(data, identifier, predict_continuous, predict_ordinal, predict_categorical, dots_arguments){
  get_predictions(data, identifier, predict_continuous, predict_ordinal, predict_categorical, dots_arguments) %>%
    rename(
      continuous_reference = continuous_prediction,
      ordinal_reference = ordinal_prediction,
      categorical_reference = categorical_prediction
    ) %>%
    select(all_of(identifier), continuous_reference, ordinal_reference, categorical_reference)
}


get_predictions <- function(data, identifier, predict_continuous, predict_ordinal, predict_categorical, dots_arguments){
  data %>%
    mutate(continuous_prediction = predict_continuous(., dots_arguments)) %>%
    mutate(ordinal_prediction = predict_ordinal(., dots_arguments)) %>%
    mutate(categorical_prediction = predict_categorical(., dots_arguments))
}


get_discordance_measures <- function(data, identifier){
  if (any(is.na(data[["continuous_prediction"]])) & any(is.na(data[["ordinal_prediction"]])) & any(is.na(data[["categorical_prediction"]]))){
    stop("No continuous, ordinal and/or categorical predictions could be calculated", call. = FALSE)
  } else if (!any(is.na(data[["continuous_prediction"]])) & any(is.na(data[["ordinal_prediction"]])) & any(is.na(data[["categorical_prediction"]]))){
    discordance_continuous_prediction <- get_discordance_continuous_prediction(data)
  } else if (any(is.na(data[["continuous_prediction"]])) & !any(is.na(data[["ordinal_prediction"]])) & any(is.na(data[["categorical_prediction"]]))){
    discordance_ordinal_prediction <- get_discordance_ordinal_prediction(data)
  } else if (any(is.na(data[["continuous_prediction"]])) & any(is.na(data[["ordinal_prediction"]])) & !any(is.na(data[["categorical_prediction"]]))){
    discordance_dichotomous_prediction <- get_discordance_dichotomous_prediction(data)
  } else if (!any(is.na(data[["continuous_prediction"]])) & !any(is.na(data[["ordinal_prediction"]])) & any(is.na(data[["categorical_prediction"]]))){
    discordance_continuous_prediction <- get_discordance_continuous_prediction(data)
    discordance_ordinal_prediction <- get_discordance_ordinal_prediction(data)
    list(discordance_continuous_prediction, discordance_ordinal_prediction) %>%
      reduce(left_join, by = c(identifier, "continuous_prediction", "ordinal_prediction", "continuous_reference", "ordinal_reference"))
  } else if (!any(is.na(data[["continuous_prediction"]])) & any(is.na(data[["ordinal_prediction"]])) & !any(is.na(data[["categorical_prediction"]]))){
    discordance_continuous_prediction <- get_discordance_continuous_prediction(data)
    discordance_dichotomous_prediction <- get_discordance_dichotomous_prediction(data)
    list(discordance_continuous_prediction, discordance_dichotomous_prediction) %>%
      reduce(left_join, by = c(identifier, "continuous_prediction", "categorical_prediction", "continuous_reference", "categorical_reference"))
  } else if (any(is.na(data[["continuous_prediction"]])) & !any(is.na(data[["ordinal_prediction"]])) & !any(is.na(data[["categorical_prediction"]]))){
    discordance_ordinal_prediction <- get_discordance_ordinal_prediction(data)
    discordance_dichotomous_prediction <- get_discordance_dichotomous_prediction(data)
    list(discordance_ordinal_prediction, discordance_dichotomous_prediction) %>%
      reduce(left_join, by = c(identifier, "ordinal_prediction", "categorical_prediction", "ordinal_reference", "categorical_reference"))
  } else {
    discordance_continuous_prediction <- get_discordance_continuous_prediction(data)
    discordance_ordinal_prediction <- get_discordance_ordinal_prediction(data)
    discordance_dichotomous_prediction <- get_discordance_dichotomous_prediction(data)
    list(discordance_continuous_prediction, discordance_ordinal_prediction, discordance_dichotomous_prediction) %>%
      reduce(left_join, by = c(identifier, "continuous_prediction", "ordinal_prediction", "categorical_prediction", "continuous_reference", "ordinal_reference", "categorical_reference"))
  }
}


get_discordance_continuous_prediction <- function(data){
  data %>% mutate(
    continuous_difference = continuous_prediction - continuous_reference,
    continuous_percentage_difference = continuous_difference / continuous_reference * 100,
    continuous_absolute_error = abs(continuous_reference - continuous_prediction),
    continuous_squared_error = (continuous_reference - continuous_prediction)**2
  )
}


get_discordance_ordinal_prediction <- function(data){
  data %>% mutate(
    ordinal_difference = ordinal_prediction - ordinal_reference,
    ordinal_absolute_error = abs(ordinal_reference - ordinal_prediction),
    ordinal_squared_error = (ordinal_reference - ordinal_prediction)**2,
    ordinal_discordant = case_when(
      ordinal_prediction != ordinal_reference ~ TRUE,
      ordinal_prediction == ordinal_reference ~ FALSE,
      TRUE ~ NA
    )
  )
}


get_discordance_dichotomous_prediction <- function(data){
  data %>% mutate(
    dichotomous_discordant = case_when(
      categorical_prediction != categorical_reference ~ TRUE,
      categorical_prediction == categorical_reference ~ FALSE,
      TRUE ~ NA
    )
  )
}


plot_distribution_per_laboratory <- function(data, variable){
  for (lab in unique(data[["laboratory"]])){
    data_per_laboratory <- data %>% filter(laboratory == lab)
    reference <- median(data_per_laboratory[[variable]])
    histogram <- plot_distribution_histogram(data_per_laboratory, variable, reference)
    qqplot <- plot_distribution_qqplot(data_per_laboratory, variable)
    combined_plots <- ggarrange(histogram, qqplot)
    plot(annotate_figure(combined_plots, top = text_grob(lab, face = "bold", size = 18)))
  }
  
}


plot_distribution_histogram <- function(data, variable, reference){
  ggplot(data = data, aes(x = !!sym(variable), y = after_stat(density))) +
    geom_histogram(bins = ceiling(length(data[[variable]])/25), color = "black", fill = "white") +
    geom_vline(xintercept = reference, color = "black", linetype = "dashed") +
    labs(title = str_wrap(paste0("Histogram ", variable), width = 8),
         x = variable) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
}


plot_distribution_qqplot <- function(data, variable){
  ggplot(data = data, aes(sample = !!sym(variable))) +
    geom_qq() +
    geom_qq_line() +
    labs(title = str_wrap(paste0("QQ plot ", variable), width = 8),
         x = "Theoretical",
         y = variable) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
}


get_summary_of_discordance_measures_as_vector <- function(data, indices){
  data_sample <- data[indices,]
  return(unlist(summarise_discordance_measures(data_sample), use.names = FALSE))
}


summarise_discordance_measures <- function(data){
  if (any(is.na(data[["continuous_prediction"]])) & any(is.na(data[["ordinal_prediction"]])) & any(is.na(data[["categorical_prediction"]]))){
    stop("No continuous, ordinal and/or categorical predictions could be calculated", call. = FALSE)
  } else if (!any(is.na(data[["continuous_prediction"]])) & any(is.na(data[["ordinal_prediction"]])) & any(is.na(data[["categorical_prediction"]]))){
    discordance_continuous_prediction <- summarise_continuous_discordance_measures(data)
  } else if (any(is.na(data[["continuous_prediction"]])) & !any(is.na(data[["ordinal_prediction"]])) & any(is.na(data[["categorical_prediction"]]))){
    discordance_ordinal_prediction <- summarise_ordinal_discordance_measures(data)
  } else if (any(is.na(data[["continuous_prediction"]])) & any(is.na(data[["ordinal_prediction"]])) & !any(is.na(data[["categorical_prediction"]]))){
    discordance_dichotomous_prediction <- summarise_dichotomous_discordance_measures(data)
  } else if (!any(is.na(data[["continuous_prediction"]])) & !any(is.na(data[["ordinal_prediction"]])) & any(is.na(data[["categorical_prediction"]]))){
    discordance_continuous_prediction <- summarise_continuous_discordance_measures(data)
    discordance_ordinal_prediction <- summarise_ordinal_discordance_measures(data)
    return(bind_cols(discordance_continuous_prediction, discordance_ordinal_prediction))
  } else if (!any(is.na(data[["continuous_prediction"]])) & any(is.na(data[["ordinal_prediction"]])) & !any(is.na(data[["categorical_prediction"]]))){
    discordance_continuous_prediction <- summarise_continuous_discordance_measures(data)
    discordance_dichotomous_prediction <- summarise_dichotomous_discordance_measures(data)
    return(bind_cols(discordance_continuous_prediction, discordance_dichotomous_prediction))
  } else if (any(is.na(data[["continuous_prediction"]])) & !any(is.na(data[["ordinal_prediction"]])) & !any(is.na(data[["categorical_prediction"]]))){
    discordance_ordinal_prediction <- summarise_ordinal_discordance_measures(data)
    discordance_dichotomous_prediction <- summarise_dichotomous_discordance_measures(data)
    return(bind_cols(discordance_ordinal_prediction, discordance_dichotomous_prediction))
  } else {
    discordance_continuous_prediction <- summarise_continuous_discordance_measures(data)
    discordance_ordinal_prediction <- summarise_ordinal_discordance_measures(data)
    discordance_dichotomous_prediction <- summarise_dichotomous_discordance_measures(data)
    return(bind_cols(discordance_continuous_prediction, discordance_ordinal_prediction, discordance_dichotomous_prediction))
  }
}


summarise_continuous_discordance_measures <- function(data){
  data %>% summarise(
    continuous_median_prediction = median(continuous_prediction),
    continuous_median_difference = median(continuous_difference),
    continuous_median_percentage_difference = median(continuous_percentage_difference),
    continuous_mae = mean(continuous_absolute_error),
    continuous_rmse = sqrt(mean(continuous_squared_error))
  )
}


summarise_ordinal_discordance_measures <- function(data){
  percentage_per_prediction_class <- get_percentage_per_prediction_class(data, "ordinal_prediction")

  discordance_measures <- data %>% summarise(
    ordinal_median_difference = median(ordinal_difference),
    ordinal_mae = mean(ordinal_absolute_error),
    ordinal_micro_rmse = sqrt(mean(continuous_squared_error)),
    ordinal_macro_rmse = get_macro_rmse(data, "ordinal_squared_error", "ordinal_reference"),
    ordinal_percentage_discordant = get_percentage_discordant(data, ordinal_discordant, ordinal)
  )
  
  return(bind_cols(percentage_per_prediction_class, discordance_measures))
}


summarise_dichotomous_discordance_measures <- function(data){
  percentage_per_prediction_class <- get_percentage_per_prediction_class(data, "categorical_prediction")
  
  discordance_measures <- data %>% summarise(
    dichotomous_percentage_discordant = get_percentage_discordant(data, dichotomous_discordant, dichotomous)
  )
  
  return(bind_cols(percentage_per_prediction_class, discordance_measures))
}


get_macro_rmse <- function(data, variable, group){
  data %>%
    group_by(!!sym(group)) %>%
    summarise(
      rmse = sqrt(mean(!!sym(variable))),
      .groups = "drop"
    ) %>%
    summarise(
      macro_rmse = mean(rmse)
    )
}


get_percentage_per_prediction_class <- function(data, prediction){
  data %>%
    group_by(!!sym(prediction)) %>%
    summarise(percentage_per_prediction_class = n() / nrow(data) * 100) %>%
    pivot_wider(
      names_from = prediction,
      names_prefix = paste0("percentage_", prediction, "_"),
      values_from = "percentage_per_prediction_class"
    ) %>%
    ungroup()
}


get_percentage_discordant <- function(data, variable, prefix){
  data %>% summarise(
    "{{prefix}}_percentage_discordant":= sum(data %>% select({{variable}})) / nrow(.) * 100
  )
}


summarise_distribution_categorical_bias_measures <- function(data, variable){
  data %>% summarise(
    "percentage_{{variable}}" := sum(data %>% select({{variable}})) / nrow(.) * 100
  )
}


get_variable_names_from_summary_of_discordance_measures_as_vector <- function(data){
  return(names(summarise_discordance_measures(data)))
}
