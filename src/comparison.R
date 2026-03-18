library(dplyr)
library(ggplot2)
library(ggsci)

get_histogram <- function(data, variable, variable_label, method, method_label, binwidth, x_range, y_range){
  
  if(is.null(x_range) & is.null(y_range)) {
    ggplot(data = data, aes(x = !!sym(variable))) +
      geom_histogram(binwidth = binwidth, color = "black", fill = "lightgrey") +
      # coord_cartesian( ylim = y_range) +
      scale_y_continuous(breaks = scales::pretty_breaks()) +
      labs(
        x = variable_label,
        y = "Frequency",
        fill = method_label
      ) +
      scale_color_nejm() + 
      theme_classic() +
      theme(legend.position="none") 
    
  } else if(!is.null(x_range) & is.null(y_range)) {
   
     ggplot(data = data, aes(x = !!sym(variable) )) +
      geom_histogram(binwidth = binwidth, color = "black", fill = "lightgrey") +
      coord_cartesian(xlim = x_range) +
      scale_y_continuous(breaks = scales::pretty_breaks()) +
      labs(
        x = variable_label,
        y = "Frequency",
        fill = method_label
      ) +
      scale_color_nejm() + 
      theme_classic() +
      theme(legend.position="none") 
    
  } else {
    
    ggplot(data = data, aes(x = !!sym(variable) )) +
      geom_histogram(binwidth = binwidth, color = "black", fill = "lightgrey") +
      coord_cartesian( ylim = y_range, xlim = x_range) +
      scale_y_continuous(breaks = scales::pretty_breaks()) +
      labs(
        x = variable_label,
        y = "Frequency",
        fill = method_label
      ) +
      scale_color_nejm() + 
      theme_classic() +
      theme(legend.position="none") 
    
  }
}



get_distribution_table <- function(data, variable, method){
  results_reference <- data %>% filter(laboratory == "reference") %>% group_by(laboratory) %>% get_distribution(., variable)
  distribution_altm <- data %>% get_distribution(., variable) %>% mutate(laboratory = "all labs") %>% relocate(laboratory)
  distribution_per_method <- data %>% group_by(!!sym(method)) %>% get_distribution(., variable)
  distribution_table <- bind_rows(results_reference, distribution_altm, distribution_per_method)
}

get_distribution <- function(data, variable){
  data %>%
    summarize(
      n = n(),
      mean = median(!!sym(variable)),
      `robust sd` = get_robust_sd(!!sym(variable)),
      min = get_quantile(!!sym(variable), probs = c(0)),
      `qauntile 2.5%` = get_quantile(!!sym(variable), probs = c(0.025)),
      `qauntile 25%` = get_quantile(!!sym(variable), probs = c(0.25)),
      `qauntile 75%` = get_quantile(!!sym(variable), probs = c(0.75)),
      `qauntile 97.5%` = get_quantile(!!sym(variable), probs = c(0.975)),
      max = get_quantile(!!sym(variable), probs = c(1))
    )
}

get_quantile <- function(x, probs){
  n = n()
  qnt = quantile(x,probs)
  
  qauntile_number = case_when(
    n < 19 ~ NA,  #based on at least 1/q observations for 95%
    n >= 20 ~ qnt
  )
  
  qauntile_number
}

get_robust_sd <- function(x){
  mad = mad(x)
  niqr = normalized_iqr(x)
  sd = sd(x)
  sd2 = sd_for_2(x)
  n = n()

  robust_sd = case_when(
    n < 2 ~ NA,
    n == 2 & sd2 > 0 ~ sd2,
    n == 2 & sd == 0 ~ NA,
    n > 2 & mad > 0 ~ mad,
    n > 2 & mad == 0 & niqr > 0 ~ niqr,
    n > 2 & mad == 0 & niqr == 0 & sd > 0 ~ sd,
    n > 2 & mad == 0 & niqr == 0 & sd == 0 ~ NA
  )
  
  robust_sd
}


normalized_iqr <- function(x){
  IQR(x) * 0.7413
}


sd_for_2 <- function(x){
  (max(x) - min(x)) / sqrt(2)
}
