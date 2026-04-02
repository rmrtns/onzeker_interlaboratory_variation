library(tidyr)
library(boot)
library(stringr)
# library(broom)
library(dplyr) # load after xgboost to prevent masking slice function from dplyr
library(purrr)

source('src/simulate_bias.R', local = bias <- new.env())
source('src/save_output.R', local = save <- new.env())

simulate_bias_induced_discordance <- function(data, identifier, variables, bias_factors, bias_intercepts, laboratory, continuous_prediction_function, ordinal_prediction_function, categorical_prediction_function, base_dir, ...){ 
  is_bias_entered_correctly(bias_factors, bias_intercepts)
  is_prediction_function_entered(continuous_prediction_function, ordinal_prediction_function, categorical_prediction_function)
  predict_continuous <- create_pointer_to_prediction_function(continuous_prediction_function)
  predict_ordinal <- create_pointer_to_prediction_function(ordinal_prediction_function)
  predict_categorical <- create_pointer_to_prediction_function(categorical_prediction_function)
  dots_arguments <- list(...)
  list_of_simulated_predictions_and_discordances <- list()
  reference_predictions <- get_reference_predictions(data, identifier, predict_continuous, predict_ordinal, predict_categorical, dots_arguments)
  constant_data <- bias$create_constant_data(data, variables)
  for (row in 1:nrow(bias_factors)){
    simulated_data <- bias$simulate_bias(data, identifier, variables, bias_factors[row,], bias_intercepts[row,])
    combined_data <- left_join(constant_data, simulated_data, by = c(identifier))
    combined_data_with_predictions <- get_predictions(combined_data, identifier, predict_continuous, predict_ordinal, predict_categorical, dots_arguments)
    # save$save_database_data(combined_data_with_predictions, base_dir, bias_factors[[laboratory]][row])
    simulated_predictions <- combined_data_with_predictions %>% select(all_of(identifier), continuous_prediction, ordinal_prediction, categorical_prediction)
    simulated_and_reference_predictions <- left_join(simulated_predictions, reference_predictions, by = identifier)
    simulated_discordances_per_record <- get_discordance_measures(simulated_and_reference_predictions, identifier)
    simulated_predictions_and_discordances_per_record <- left_join(simulated_and_reference_predictions, simulated_discordances_per_record, by = c(identifier))
    list_of_simulated_predictions_and_discordances[[bias_factors[[laboratory]][row]]] <- cbind(laboratory = bias_factors[[laboratory]][row], simulated_predictions_and_discordances_per_record)
  }
  simulated_predictions_and_discordances <- bind_rows(list_of_simulated_predictions_and_discordances)
  return(simulated_predictions_and_discordances)
}


is_bias_entered_correctly <- function(bias_factors, bias_intercepts){
  stopifnot(nrow(bias_factors) == nrow(bias_intercepts))
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
  results <- list()
  
  if (all(!is.na(data$continuous_prediction))) {
    results[["continuous"]] <- get_discordance_continuous_prediction(data)
  }
  
  if (all(!is.na(data$ordinal_prediction))) {
    results[["ordinal"]] <- get_discordance_ordinal_prediction(data)
  }
  
  if (all(!is.na(data$categorical_prediction))) {
    results[["categorical"]] <- get_discordance_categorical_prediction(data)
  }
  
  if (length(results) == 0) {
    stop("No discordance measures could be calculated", call. = FALSE)
  }
  
  reduce(results, left_join, by = identifier)
}


get_discordance_continuous_prediction <- function(data){
  data %>% mutate(
    continuous_difference = continuous_prediction - continuous_reference,
    continuous_percentage_difference = (continuous_prediction - continuous_reference) / continuous_reference * 100,
    continuous_absolute_error = abs(continuous_reference - continuous_prediction),
    continuous_squared_error = (continuous_reference - continuous_prediction)**2
  ) %>% select(-c(continuous_prediction, ordinal_prediction, categorical_prediction, continuous_reference, ordinal_reference, categorical_reference))
}


get_discordance_ordinal_prediction <- function(data){
  data %>% mutate(
    ordinal_difference = as.numeric(ordinal_prediction) - as.numeric(ordinal_reference),
    ordinal_absolute_error = abs(as.numeric(ordinal_reference) - as.numeric(ordinal_prediction)),
    ordinal_squared_error = (as.numeric(ordinal_reference) - as.numeric(ordinal_prediction))**2,
    ordinal_discordant = case_when(
      ordinal_prediction != ordinal_reference ~ TRUE,
      ordinal_prediction == ordinal_reference ~ FALSE,
      TRUE ~ NA
    )
  ) %>% select(-c(continuous_prediction, ordinal_prediction, categorical_prediction, continuous_reference, ordinal_reference, categorical_reference))
}


get_discordance_categorical_prediction <- function(data){
  data %>% mutate(
    categorical_discordant = case_when(
      categorical_prediction != categorical_reference ~ TRUE,
      categorical_prediction == categorical_reference ~ FALSE,
      TRUE ~ NA
    )
  ) %>% select(-c(continuous_prediction, ordinal_prediction, categorical_prediction, continuous_reference, ordinal_reference, categorical_reference))
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
  ggplot(data = data, aes(x = .data[[variable]], y = after_stat(density))) +
    geom_histogram(bins = ceiling(length(data[[variable]])/25), color = "black", fill = "white") +
    geom_vline(xintercept = reference, color = "black", linetype = "dashed") +
    labs(title = str_wrap(paste0("Histogram ", variable), width = 8),
         x = variable) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
}


plot_distribution_qqplot <- function(data, variable){
  ggplot(data = data, aes(sample = .data[[variable]])) +
    geom_qq() +
    geom_qq_line() +
    labs(title = str_wrap(paste0("QQ plot ", variable), width = 8),
         x = "Theoretical",
         y = variable) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
}


get_summary_of_discordance_measures_all_laboratories <- function(data){
  laboratories <- as.character(unique(data[["laboratory"]]))
  list_of_summaries <- list()
  for (lab in laboratories){
    data_per_lab <- data %>% filter(laboratory == lab)
    summary_per_laboratory <- summarise_discordance_measures(data_per_lab)
    list_of_summaries[[lab]] <- bind_cols(laboratory = lab, summary_per_laboratory)
  }
  return(bind_rows(list_of_summaries))
}


get_summary_of_discordance_measures_as_vector <- function(data, indices){
  data_sample <- data[indices,]
  return(unlist(summarise_discordance_measures(data_sample), use.names = FALSE))
}


summarise_discordance_measures <- function(data){
  results <- list()

  if (all(!is.na(data$continuous_prediction))) {
    results[["continuous"]] <- summarise_continuous_discordance_measures(data)
  }

  if (all(!is.na(data$ordinal_prediction))) {
    results[["ordinal"]] <- summarise_ordinal_discordance_measures(data)
  }

  if (all(!is.na(data$categorical_prediction))) {
    results[["categorical"]] <- summarise_categorical_discordance_measures(data)
  }

  if (length(results) == 0) {
    stop("No continuous, ordinal and/or categorical predictions could be summarised.", call. = FALSE)
  }
  
  bind_cols(results)
}


summarise_continuous_discordance_measures <- function(data){
  data %>% summarise(
    continuous_median_prediction = median(continuous_prediction),
    continuous_median_difference = median(continuous_difference),
    continuous_median_percentage_difference = median(continuous_percentage_difference),
    continuous_mae = mean(continuous_absolute_error),
    continuous_rmse = sqrt(mean(continuous_squared_error)),
    continuous_mape_trimmed = get_mape_trimmed(data, "continuous_reference", "continuous_prediction"),
    continuous_mdape = get_mdape(data, "continuous_reference", "continuous_prediction"),
    continuous_rmspe_trimmed = get_rmspe_trimmed(data, "continuous_reference", "continuous_prediction"),
    continuous_rmdspe = get_rmdspe(data, "continuous_reference", "continuous_prediction")
  )
}


summarise_ordinal_discordance_measures <- function(data){
  percentage_per_prediction_class <- get_percentage_per_prediction_class(data, "ordinal_prediction")

  discordance_measures <- data %>% summarise(
    ordinal_median_difference = median(ordinal_difference),
    ordinal_mae = mean(ordinal_absolute_error),
    ordinal_micro_rmse = sqrt(mean(ordinal_squared_error)),
    ordinal_macro_rmse = get_macro_rmse(data, "ordinal_squared_error", "ordinal_reference"),
    ordinal_percentage_discordant = mean(ordinal_discordant) * 100
  )
  
  return(bind_cols(percentage_per_prediction_class, discordance_measures))
}


summarise_categorical_discordance_measures <- function(data){
  percentage_per_prediction_class <- get_percentage_per_prediction_class(data, "categorical_prediction")
  
  discordance_measures <- data %>% summarise(
    categorical_percentage_discordant = mean(categorical_discordant) * 100
  )

  return(bind_cols(percentage_per_prediction_class, discordance_measures))
}


get_mape_trimmed <- function(data, actual, prediction){
  data %>%
    summarise(
      mape = mean(
        if_else(
          .data[[actual]] == 0,
          0,
          (abs(.data[[prediction]] - .data[[actual]])) / abs(.data[[actual]])
        ),
        trim = 0.05
      ) * 100
    ) %>% unlist()
}


get_mdape <- function(data, actual, prediction){
  data %>%
    summarise(
      mape = median(
        if_else(
          .data[[actual]] == 0,
          0,
          (abs(.data[[prediction]] - .data[[actual]])) / abs(.data[[actual]])
        )
      ) * 100
    ) %>% unlist()
}


get_rmspe_trimmed <- function(data, actual, prediction){
  data %>%
    summarise(
      rmspe = sqrt(
        mean(
          if_else(
            .data[[actual]] == 0,
            0,
            ((.data[[prediction]] - .data[[actual]]) / .data[[actual]])**2
          ),
          trim = 0.05
        )
      ) * 100
    ) %>% unlist()
}


get_rmdspe <- function(data, actual, prediction){
  data %>%
    summarise(
      rmspe = sqrt(
        median(
          if_else(
            .data[[actual]] == 0,
            0,
            ((.data[[prediction]] - .data[[actual]]) / .data[[actual]])**2
          )
        )
      ) * 100
    ) %>% unlist()
}


get_macro_rmse <- function(data, variable, group){
  data %>%
    group_by(.data[[group]]) %>%
    summarise(
      rmse = sqrt(mean(.data[[variable]])),
      .groups = "drop"
    ) %>%
    summarise(
      macro_rmse = mean(rmse)
    ) %>% unlist()
}


get_percentage_per_prediction_class <- function(data, prediction){
  data %>%
    group_by(.data[[prediction]]) %>%
    summarise(percentage_per_prediction_class = n() / nrow(data) * 100) %>%
    pivot_wider(
      names_from = all_of(prediction),
      names_prefix = paste0("percentage_", prediction, "_"),
      values_from = "percentage_per_prediction_class"
    ) %>%
    ungroup()
}


get_variable_names_from_summary_of_discordance_measures_as_vector <- function(data){
  return(names(summarise_discordance_measures(data)))
}
