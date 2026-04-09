library(boot)
# const function copied from source code boot
library(dplyr)
library(ggplot2)
library(ggpubr)
# https://github.com/tidyverse/purrr/issues/804 for rename_list_items function
# https://stackoverflow.com/questions/65524427/using-bootboot-function-with-grouped-variables-in-r for grouped bootstrapping

get_bootstrapped_ci_all_laboratories <- function(boot_out_list, variables, confidence_level, methods){
  laboratories <- names(boot_out_list)
  list_of_bootstrapped_cis <- list()
  for (lab in laboratories){
    bootstrapped_ci_per_laboratory <- get_bootstrapped_ci_per_laboratory(boot_out_list[[lab]], variables, confidence_level, methods)
    list_of_bootstrapped_cis[[lab]] <- cbind(laboratory = lab, bootstrapped_ci_per_laboratory)
  }
  return(bind_rows(list_of_bootstrapped_cis))
}


get_bootstrapped_ci_per_laboratory <- function(boot_out, variables, confidence_level, methods){
  variables <- as.vector(variables)
  statistics <- get_statistics_from_boot_out(boot_out, variables)
  cis_per_variable <- get_bootstrapped_cis_per_variable(boot_out, variables, confidence_level, methods, statistics)
  # plot_boot_out(boot_out, variables) # comment this in for bootstrap histograms
  return(left_join(statistics, cis_per_variable, by = "variable"))
}


get_statistics_from_boot_out <- function(boot_out, variables){
  list_of_statistics <- list()
  for (variable_index in 1:length(variables)){
    statistics <- c(
      original = boot_out[["t0"]][variable_index],
      bias = mean(boot_out[["t"]][, variable_index]) - boot_out[["t0"]][variable_index],
      std_error = sd(boot_out[["t"]][,variable_index])
    )
    list_of_statistics[[variable_index]] <- statistics
  }
  return(bind_cols(variable = variables, bind_rows(list_of_statistics)))
}


get_bootstrapped_cis_per_variable <- function(boot_out, variables, confidence_level, methods, statistics){
  list_of_ci_per_variable <- list()
  for (variable_index in 1:length(variables)){
    if ("bca" %in% methods & statistics[["std_error"]][variable_index] < 0.001){
      print(paste0("Bca method not applied for ", variables[variable_index], " because of zero variance"))
      methods_checked <- methods[!methods %in% c("bca")]
    } else {methods_checked <- methods}
    extracted_ci_per_variable <- extract_ci(boot_out, variable_index, confidence_level, methods_checked)
    ci_dataframe <- get_dataframe_of_ci(extracted_ci_per_variable, variables[variable_index])
    list_of_ci_per_variable[[variable_index]] <- ci_dataframe
  }
  cis_per_variable <- bind_rows(list_of_ci_per_variable)
}


extract_ci <- function(boot_out, variable_index, confidence_level, methods){
  if (const(boot_out[["t"]][, variable_index], min(1e-8, mean(boot_out[["t"]][, variable_index], na.rm=TRUE)/1e6))){
    boot_ci_as_list <- list()
    for (method in 1:length(methods)){
      boot_ci_as_list[[methods[method]]] <- c(mean(boot_out[["t"]][, variable_index]), mean(boot_out[["t"]][, variable_index]))
      if (methods[[method]] == "perc"){
        boot_ci_as_list <- rename_list_items(boot_ci_as_list, percent = perc)  
      } else if (methods[[method]] == "norm"){
        boot_ci_as_list <- rename_list_items(boot_ci_as_list, normal = norm)
      }
    }
    return(boot_ci_as_list)
  } else {
    boot_ci_as_list <- boot.ci(boot_out, index = variable_index, conf = confidence_level, type = methods)
    return(boot_ci_as_list[-(1:3)])
  }
}


const <- function(w, eps=1e-8) {
  all(abs(w-mean(w, na.rm=TRUE)) < eps) | any(is.na(abs(w-mean(w, na.rm=TRUE)) < eps))
}


rename_list_items <- function(.x, ..., .strict = TRUE) {
  pos <- tidyselect::eval_rename(quote(c(...)), .x, strict = .strict)
  names(.x)[pos] <- names(pos)
  .x
}


get_dataframe_of_ci <- function(extracted_ci, variable_name){
  list_of_ci <- list()
  for (method in names(extracted_ci)){
    ci <- extracted_ci[[method]][c(length(extracted_ci[[method]])-1, length(extracted_ci[[method]]))]
    list_of_ci[[method]] <- ci
  }
  dataframe_of_ci <- as.data.frame(list_of_ci) %>%
    mutate(ci_level = c("conf_low", "conf_high")) %>%
    pivot_wider(
      names_from = ci_level,
      values_from = -ci_level
    ) %>%
    mutate(variable = variable_name)
}


get_dataframe_without_ci <- function(dataframe_of_ci){
  dataframe_of_ci %>%
    select(laboratory, variable, original) %>%
    pivot_wider(
      names_from = variable,
      values_from = original
    )
}


plot_boot_out <- function(boot_out, variables){
  for (variable_index in 1:length(variables)){
    t_zero <- boot_out[["t0"]][variable_index]
    t_star <- data.frame(t = boot_out[["t"]][, variable_index])
    histogram <- plot_boot_out_histogram(variables, variable_index, t_star, "t", t_zero)
    qqplot <- plot_boot_out_qqplot(variables, variable_index, t_star, "t")
    plot(ggarrange(histogram, qqplot))
  }
}


plot_boot_out_histogram <- function(variables, variable_index, data, x, reference){
  ggplot(data = data, aes(x = .data[[x]], y = after_stat(density))) +
    geom_histogram(bins = ceiling(length(data[[x]])/25), boundary = 0, closed = "left", color = "black", fill = "white") +
    geom_vline(xintercept = reference, color = "black", linetype = "dashed") +
    labs(title = paste0("Histogram of t* for \n ", variables[[variable_index]]),
         x = "t*") +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
}


plot_boot_out_qqplot <- function(variables, variable_index, data, x){
  ggplot(data = data, aes(sample = .data[[x]])) +
    geom_qq() +
    geom_qq_line() +
    labs(title = paste0("QQ plot of t* for \n ", variables[[variable_index]]),
         x = "Theoretical",
         y = "t*") +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
}