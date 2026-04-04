library(dplyr)
library(ggplot2)

get_histogram <- function(data, variable, variable_label, binwidth, x_range, y_range){
  histogram <- ggplot(data = data, aes(x = .data[[variable]])) +
    geom_histogram(binwidth = binwidth, color = "black", fill = "lightgrey") +
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    labs(
      x = variable_label,
      y = "Frequency"
    ) +
    theme_classic() +
    theme(legend.position="none")

  if(is.null(x_range) & is.null(y_range)) {
    histogram
  } else if(!is.null(x_range) & is.null(y_range)) {
    histogram + coord_cartesian(xlim = x_range)
  } else if(is.null(x_range) & !is.null(y_range)){
    histogram + coord_cartesian(ylim = y_range)
  } else {
    histogram + coord_cartesian(ylim = y_range, xlim = x_range)
  }
}


get_distribution_table <- function(data, variable, laboratory){
  results_reference <- data %>% filter(.data[[laboratory]] == "reference") %>% group_by(.data[[laboratory]]) %>% get_distribution(., variable)
  distribution_altm <- data %>% get_distribution(., variable) %>% mutate(laboratory = "all labs") %>% relocate(laboratory)
  distribution_per_laboratory <- data %>% group_by(.data[[laboratory]]) %>% get_distribution(., variable)
  distribution_table <- bind_rows(results_reference, distribution_altm, distribution_per_laboratory)
}


get_distribution <- function(data, variable){
  data %>%
    summarize(
      n = n(),
      mean = median(.data[[variable]]),
      `robust sd` = get_robust_sd(.data[[variable]]),
      min = get_quantile(.data[[variable]], probs = c(0)),
      `quantile 2.5%` = get_quantile(.data[[variable]], probs = c(0.025)),
      `quantile 25%` = get_quantile(.data[[variable]], probs = c(0.25)),
      `quantile 75%` = get_quantile(.data[[variable]], probs = c(0.75)),
      `quantile 97.5%` = get_quantile(.data[[variable]], probs = c(0.975)),
      max = get_quantile(.data[[variable]], probs = c(1))
    )
}


get_quantile <- function(x, probs){
  x <- x[is.finite(x)]
  n <- length(x)
  qnt = quantile(x, probs)
  
  qauntile_number = case_when(
    n < 19 ~ NA,  # based on at least 1/q observations for 95%
    n >= 20 ~ qnt
  )
  
  qauntile_number
}


get_robust_sd <- function(x){
  x <- x[is.finite(x)]
  n <- length(x)
  
  if (n < 2){
    return(NA_real_)
  }
  
  if (n == 2){
    sd <- sd(x)
    sd2 <- sd_for_2(x)
    
    if (sd == 0){
      return(NA_real_)
    } else if (sd > 0){
      return(sd2)
    } else {
      return(sd)
    }
  }
  
  if (n > 2){
    mad = mad(x)
    niqr = normalized_iqr(x)
    sd = sd(x)
    
    if (mad > 0){
      return(mad)
    } else if (niqr > 0){
      return(niqr)
    } else if (sd > 0){
      return(sd)
    } else{
      return(NA_real_)
    }
  }
}


normalized_iqr <- function(x){
  IQR(x) * 0.7413
}


sd_for_2 <- function(x){
  (max(x) - min(x)) / sqrt(2)
}
