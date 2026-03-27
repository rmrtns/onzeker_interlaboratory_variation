library(dplyr)
library(tidyr)

get_bias_table <- function(bias_data, lab_col, cluster_col, measurement_col, bias_data_names, non_bias_vector, reference_lab = NA){
  if (!is.na(reference_lab)){
    bias_data_recalculated <- regression_with_ref_centre(bias_data, lab_col, cluster_col, measurement_col, reference_lab)
    df_intercepts <- bias_data_recalculated %>% clean_bias_data(., lab_col, measurement_col, bias_data_names, non_bias_vector, "New_Intercept")
    df_slopes <- bias_data_recalculated %>% clean_bias_data(., lab_col, measurement_col, bias_data_names, non_bias_vector, "New_Slope")
    return(list(intercepts = df_intercepts, slopes = df_slopes))
  } else if (!is.na(cluster_col)) {
    bias_data_single_cluster <- bias_data %>% remove_clusters(., lab_col, cluster_col, measurement_col)
    df_intercepts <- bias_data_single_cluster %>% clean_bias_data(., lab_col, measurement_col, bias_data_names, non_bias_vector, "Intercept")
    df_slopes <- bias_data_single_cluster %>% clean_bias_data(., lab_col, measurement_col, bias_data_names, non_bias_vector, "Slope")
    return(list(intercepts = df_intercepts, slopes = df_slopes))
  } else if (is.na(cluster_col)){
    df_intercepts <- bias_data %>% clean_bias_data(., lab_col, measurement_col, bias_data_names, non_bias_vector, "Intercept")
    df_slopes <- bias_data %>% clean_bias_data(., lab_col, measurement_col, bias_data_names, non_bias_vector, "Slope")
    return(list(intercepts = df_intercepts, slopes = df_slopes))
  }
}


regression_with_ref_centre <- function(bias_data, lab_col, cluster_col, measurement_col, reference_lab){
  bias_data_single_cluster <- bias_data %>% remove_clusters(., lab_col, cluster_col, measurement_col)

  bias_data_reference_lab <- bias_data_single_cluster %>%
    filter(!!sym(lab_col) == reference_lab) %>%
    rename("Ref_Intercept" = "Intercept") %>%
    rename("Ref_Slope" = "Slope") %>%
    select(-!!sym(lab_col), -!!sym(cluster_col))

  bias_data_single_cluster_with_reference <- left_join(
    subset(bias_data_single_cluster, ptp != reference_lab),
    bias_data_reference_lab,
    by = measurement_col
  )

  bias_data_recalculated <- bias_data_single_cluster_with_reference %>%
    mutate(New_Intercept = -((Slope*Ref_Intercept)/Ref_Slope) + Intercept) %>%
    mutate(New_Slope = Slope/Ref_Slope)

  return(bias_data_recalculated)
}


remove_clusters <- function(bias_data, lab_col, cluster_col, measurement_col){
  bias_data_single_cluster <- bias_data %>%
    group_by(!!sym(measurement_col), !!sym(lab_col)) %>%
    filter(!!sym(cluster_col) == min(!!sym(cluster_col))) %>%
    ungroup()
}


clean_bias_data <- function(data, lab_col, measurement_col, bias_data_names, non_bias_vector, bias_parameter){
  data %>%
    group_by(!!sym(lab_col)) %>%
    select(!!sym(lab_col), !!sym(measurement_col), !!sym(bias_parameter)) %>%
    pivot_wider(names_from = !!sym(measurement_col), values_from = !!sym(bias_parameter)) %>%
    select(!!sym(lab_col), all_of(bias_data_names)) %>%
    mutate(!!!setNames(as.list(rep(NA, length(non_bias_vector))), non_bias_vector)) %>%
    filter(!if_any(-all_of(non_bias_vector), is.na)) %>%
    rename(lab = lab_col) %>%
    ungroup()
}
