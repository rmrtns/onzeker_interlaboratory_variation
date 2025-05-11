# Wijzigingen:
## In hoofdfunctie crossover complete_confusion_matrix toegevoegd om foutmelding bij ontbreken category te voorkomen.

library(caret)
library(pROC)
library(broom)
library(tidyr)
library(dplyr)

# Functions to create and summarise results of confusion matrices.

get_summmary_performance_measures_as_vector <- function(data, indices, outcome, outcome_class_TRUE){
  data_sample <- data[indices,]
  summary <- list(c(get_summmary_performance_measures(data_sample, outcome, outcome_class_TRUE)))
  return(unlist(summary, use.names = FALSE))
}


get_variable_names_from_summmary_performance_measures_as_vector <- function(data, outcome, outcome_class_TRUE){
  summary <- list(names(get_summmary_performance_measures(data, outcome, outcome_class_TRUE)))
  return(unlist(summary))
}


get_summmary_performance_measures <- function(data, outcome, outcome_class_TRUE){
  auroc <- calculate_auroc(data, outcome)
  confusion_matrix <- calculate_confusion_matrix(data, outcome, outcome_class_TRUE)
  confusion_matrix_derived_variables <- tidy_confusion_matrix(confusion_matrix)
  combined <- bind_rows(auroc, confusion_matrix_derived_variables)
  return(bind_cols(auroc, confusion_matrix_derived_variables))
}


calculate_auroc <- function(data, outcome){
  return(data.frame(auroc = as.numeric(auc(roc(data[[outcome]], as.numeric(data[["continuous_prediction"]]))))))
}


calculate_confusion_matrix <- function(data, outcome, outcome_class_TRUE){
  confusion_matrix <- confusionMatrix(
    as.factor(as.numeric(data[["categorical_prediction"]])),
    as.factor(as.numeric(data[[outcome]])),
    positive = as.character(as.numeric(outcome_class_TRUE)))
}


print_confusion_matrix <- function(confusion_matrix){
  print(confusion_matrix[[1]])
}


tidy_confusion_matrix <- function(confusion_matrix){
  tidy(confusion_matrix) %>% 
    mutate(
      estimate = case_when(
        term == "mcnemar" ~ p.value,
        TRUE ~ estimate)) %>%
    select(term, estimate) %>%
    pivot_wider(
      names_from = term,
      values_from = estimate
    )
}


# Functions to assess 'crossing-over' in confusion matrices based on aggregate results of discordance status.

calculate_crossover_within_confusion_matrix <- function(data, outcome){
  data_with_recoded_categorical_reference <- recode_categorical_reference(data)
  data_with_confusion_matrix <- calculate_confusion_matrix_reference_results(data_with_recoded_categorical_reference, outcome)
  percentage_discordant_per_simulation <- calculate_percentage_discordant_per_simulation(data_with_confusion_matrix, "confusion_matrix")
  percentage_discordant_per_simulation_completed <- add_empty_categories_confusion_matrix(percentage_discordant_per_simulation)
  summary <- summarise_crossover(percentage_discordant_per_simulation_completed)
  return(summary)
}


recode_categorical_reference <- function(data){
  data %>%
    mutate(
      categorical_reference = as.numeric(categorical_reference)
    )
}


calculate_confusion_matrix_reference_results <- function(data, outcome){
  data %>%
    mutate(
      confusion_matrix = case_when(
        categorical_reference == 1 & categorical_reference == !!sym(outcome) ~ "true_positive",
        categorical_reference == 1 & categorical_reference != !!sym(outcome) ~ "false_positive",
        categorical_reference == 0 & categorical_reference == !!sym(outcome) ~ "true_negative",
        categorical_reference == 0 & categorical_reference != !!sym(outcome) ~ "false_negative"
      )
    )
}


calculate_percentage_discordant_per_simulation <- function(data, category){
  data %>% 
    group_by(!!sym(category)) %>%
    summarise(percentage_discordant = sum(discordant) / nrow(data) * 100) %>%
    ungroup()
}


add_empty_categories_confusion_matrix <- function(data){
  matrix <- data.frame(confusion_matrix = c("true_positive", "false_positive", "true_negative", "false_negative"))
  added_empty_categories_confusion_matrix <- left_join(
    matrix, 
    data, by = "confusion_matrix") %>%
    mutate(
      percentage_discordant = if_else(is.na(percentage_discordant), 0, percentage_discordant),
      confusion_matrix = factor(confusion_matrix, levels = c("false_negative", "true_positive", "false_positive", "true_negative"))) %>%
    arrange(confusion_matrix)
}


summarise_crossover <- function(data){
  data <- data %>% 
    select(confusion_matrix, percentage_discordant) %>%
    pivot_wider(
      names_from = confusion_matrix,
      values_from = percentage_discordant,
      names_prefix = "percentage_discordant_"
    ) %>%
    mutate(fn_minus_tp_percentage_point_diff = percentage_discordant_false_negative - percentage_discordant_true_positive,
           fp_minus_tn_percentage_point_diff = percentage_discordant_false_positive - percentage_discordant_true_negative,
           percentage_discordant_sum = percentage_discordant_false_negative + percentage_discordant_true_positive + percentage_discordant_false_positive + percentage_discordant_true_negative)
}


get_crossover_summary_as_vector <- function(data, indices, outcome) {
  data_sample <- data[indices,]
  data_sample_summary_crossover <- calculate_crossover_within_confusion_matrix(data_sample, outcome)
  return(unlist(data_sample_summary_crossover, use.names = FALSE))
}


get_variables_names_from_crossover_summary_as_vector <- function(data, outcome){
  summary <- list(names(calculate_crossover_within_confusion_matrix(data, outcome)))
  return(unlist(summary))
}
