# Wijzigingen:
## Toevoegen simulate_bias_induced_discordance
## Recode_zero verplaatsen: 
### In geval van bias leidt de situatie van 0 waarnemingen voor een specifieke categorie potentieel tot NA en mogelijk een foutieve berekening van het percentage discordante waarnemingen.
### Bij de simulatie van variatie gebeurt dit niet, omdat door het samplen volgens een normale verdeling, de oorspronkelijke waarneming altijd voorkomt.

library(tidyr)
library(boot)
library(stringr)
# library(broom)
library(dplyr) # load after xgboost to prevent masking slice function from dplyr
library(purrr)

source('src/simulate_bias.R', local = bias <- new.env())
source('src/save_database.R', local = save_database <- new.env())


simulate_bias_induced_discordance <-
  function(data, identifier, variables, bias_factors, laboratory, continuous_prediction_function, categorical_prediction_function, ...){ 
    is_prediction_function_entered(continuous_prediction_function, categorical_prediction_function)
    predict_continuous <- create_pointer_to_prediction_function(continuous_prediction_function)
    predict_categorical <- create_pointer_to_prediction_function(categorical_prediction_function)
    dots_arguments <- list(...)
    list_of_simulated_discordances <- list()
    reference_predictions <- get_reference_predictions(data, identifier, predict_continuous, predict_categorical, dots_arguments)
    constant_data <- bias$create_constant_data(data, variables)
    for (row in 1:nrow(bias_factors)){
      simulated_data <- bias$simulate_bias(data, identifier, variables, bias_factors[[variables]][row]) 
      combined_data <- left_join(constant_data, simulated_data, by = c(identifier))
      combined_data_with_predictions <- get_predictions(combined_data, identifier, predict_continuous, predict_categorical, dots_arguments)
      # save_database$save_database_data(combined_data_with_predictions)
      simulated_predictions <- combined_data_with_predictions %>% select(all_of(identifier), continuous_prediction, categorical_prediction)
      simulated_and_reference_predictions <- left_join(simulated_predictions, reference_predictions, by = identifier)
      simulated_discordances_per_record <- get_summary_of_bias_measures(simulated_and_reference_predictions, identifier)
      list_of_simulated_discordances[[bias_factors[[laboratory]][row]]] <- cbind(laboratory = bias_factors[[laboratory]][row], simulated_discordances_per_record)
    }
    simulated_discordances <- bind_rows(list_of_simulated_discordances)
    return(simulated_discordances)
  }


is_prediction_function_entered <- function(continuous_prediction_function, categorical_prediction_function){
  if (typeof(continuous_prediction_function) != "closure" & typeof(categorical_prediction_function) != "closure"){
    stop("Please enter function for continuous and/or categorical predictions", call. = FALSE)
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


get_reference_predictions <- function(data, identifier, predict_continuous, predict_categorical, dots_arguments){
  get_predictions(data, identifier, predict_continuous, predict_categorical, dots_arguments) %>%
    rename(
      continuous_reference = continuous_prediction,
      categorical_reference = categorical_prediction
    ) %>%
    select(all_of(identifier), continuous_reference, categorical_reference)
}


get_predictions <- function(data, identifier, predict_continuous, predict_categorical, dots_arguments){
  data %>%
    mutate(continuous_prediction = predict_continuous(., dots_arguments)) %>%
    mutate(categorical_prediction = predict_categorical(., dots_arguments))
}


get_summary_of_bias_measures <- function(data, identifier){
  if (any(is.na(data[["continuous_prediction"]])) & any(is.na(data[["categorical_prediction"]]))){
    stop("No continuous and/or categorical predictions could be calculated", call. = FALSE)
  } else if (!any(is.na(data[["continuous_prediction"]])) & any(is.na(data[["categorical_prediction"]]))){
    difference_continuous_prediction <- get_difference_continuous_prediction(data)
  } else if (any(is.na(data[["continuous_prediction"]])) & !any(is.na(data[["categorical_prediction"]]))){
    discordance <- is_prediction_discordant(data)
  } else {
    difference_continuous_prediction <- get_difference_continuous_prediction(data)
    discordance <- is_prediction_discordant(data)
    list(difference_continuous_prediction, discordance) %>%
      reduce(left_join, by = c(identifier, "continuous_prediction", "categorical_prediction", "continuous_reference", "categorical_reference"))
  }
}


get_difference_continuous_prediction <- function(data){
  data %>% mutate(
    absolute_difference = continuous_prediction - continuous_reference,
    percentage_difference = absolute_difference / continuous_reference * 100)
}


is_prediction_discordant <- function(data){
  data %>% mutate(
    discordant = case_when(
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
    labs(title = paste0("Histogram ", variable),
         x = variable) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
}


plot_distribution_qqplot <- function(data, variable){
  ggplot(data = data, aes(sample = !!sym(variable))) +
    geom_qq() +
    geom_qq_line() +
    labs(title = paste0("QQ plot ", variable),
         x = "Theoretical",
         y = variable) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
}


get_distribution_of_bias_measures_as_vector <- function(data, indices){
  data_sample <- data[indices,]
  return(unlist(summarise_distribution_bias_measures(data_sample), use.names = FALSE))
}


summarise_distribution_bias_measures <- function(data){
  if (any(is.na(data[["continuous_prediction"]])) & any(is.na(data[["categorical_prediction"]]))){
    stop("No continuous and/or categorical predictions could be calculated", call. = FALSE)
  } else if (!any(is.na(data[["continuous_prediction"]])) & any(is.na(data[["categorical_prediction"]]))){
    distribution_absolute_difference <- summarise_distribution_continuous_bias_measures(data, absolute_difference)
    distribution_percentage_difference <- summarise_distribution_continuous_bias_measures(data, percentage_difference)
    return(bind_cols(distribution_absolute_difference, distribution_percentage_difference))
  } else if (any(is.na(data[["continuous_prediction"]])) & !any(is.na(data[["categorical_prediction"]]))){
    return(summarise_distribution_categorical_bias_measures(data, discordant))
  } else {
    distribution_absolute_difference <- summarise_distribution_continuous_bias_measures(data, absolute_difference)
    distribution_percentage_difference <- summarise_distribution_continuous_bias_measures(data, percentage_difference)
    distribution_discordance <- summarise_distribution_categorical_bias_measures(data, discordant)
    return(bind_cols(distribution_absolute_difference, distribution_percentage_difference, distribution_discordance))
    }
}


summarise_distribution_continuous_bias_measures <- function(data, variable){
  data %>% summarise(
    "{{variable}}_mean" := mean({{variable}}),
    "{{variable}}_sd" := sd({{variable}}),
    "{{variable}}_sem" := sd({{variable}}) / sqrt(nrow(.)),
    "{{variable}}_min" := min({{variable}}),
    "{{variable}}_p2.5" := quantile({{variable}}, 0.025),
    "{{variable}}_p25" := quantile({{variable}}, 0.25),
    "{{variable}}_median" := median({{variable}}),
    "{{variable}}_p75" := quantile({{variable}}, 0.75),
    "{{variable}}_p97.5" := quantile({{variable}}, 0.975),
    "{{variable}}_max" := max({{variable}})
  )
}


summarise_distribution_categorical_bias_measures <- function(data, variable){
  data %>% summarise(
    "percentage_{{variable}}" := sum(data %>% select({{variable}})) / nrow(.) * 100
  )
}


get_variable_names_from_distribution_of_bias_measures_as_vector <- function(data){
  return(names(summarise_distribution_bias_measures(data)))
}
