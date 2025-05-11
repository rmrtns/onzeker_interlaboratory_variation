library(dplyr)
library(ggplot2)
library(ggsci)

get_histogram <- function(data, variable, variable_label, method, method_label, binwidth, x_range, y_range){
  ggplot(data = data, aes(x = !!sym(variable), fill = !!sym(method))) +
    geom_histogram(binwidth = binwidth, color = "black") +
    coord_cartesian(xlim = x_range, ylim = y_range) +
    labs(
      x = variable_label,
      y = "Frequency",
      fill = method_label
    ) +
    scale_color_nejm() + 
    theme_classic()
}


get_distribution_table <- function(data, variable, method){
  results_reference <- data %>% filter(laboratory == "reference") %>% group_by(laboratory) %>% get_distribution(., variable)
  distribution_altm <- data %>% get_distribution(., variable) %>% mutate(laboratory = "ALTM") %>% relocate(laboratory)
  distribution_per_method <- data %>% group_by(!!sym(method)) %>% get_distribution(., variable)
  distribution_table <- bind_rows(results_reference, distribution_altm, distribution_per_method)
}


get_distribution <- function(data, variable){
  data %>%
    summarize(
      n = n(),
      median = median(!!sym(variable)),
      robust_sd = get_robust_sd(!!sym(variable))
    )
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
