library(dplyr)
library(tidyr)

get_bias_table <- function(bias_data, lab_col, cluster_col, measurement_col, bias_data_names, non_bias_vector, reference_lab = NA){
  if (!is.na(reference_lab)){
    bias_data_recalculated <- regression_with_ref_centre(bias_data, lab_col, cluster_col, measurement_col, reference_lab)
    df_intercepts <- clean_bias_data(bias_data_recalculated, lab_col, measurement_col, bias_data_names, non_bias_vector, "New_Intercept")
    df_slopes <- clean_bias_data(bias_data_recalculated, lab_col, measurement_col, bias_data_names, non_bias_vector, "New_Slope")
    return(list(intercepts = df_intercepts, slopes = df_slopes))
  } else if (!is.na(cluster_col)) {
    bias_data_single_cluster <- remove_clusters(bias_data, lab_col, cluster_col, measurement_col)
    df_intercepts <- clean_bias_data(bias_data_single_cluster, lab_col, measurement_col, bias_data_names, non_bias_vector, "Intercept")
    df_slopes <- clean_bias_data(bias_data_single_cluster, lab_col, measurement_col, bias_data_names, non_bias_vector, "Slope")
    return(list(intercepts = df_intercepts, slopes = df_slopes))
  } else if (is.na(cluster_col)){
    df_intercepts <- clean_bias_data(bias_data, lab_col, measurement_col, bias_data_names, non_bias_vector, "Intercept")
    df_slopes <- clean_bias_data(bias_data, lab_col, measurement_col, bias_data_names, non_bias_vector, "Slope")
    return(list(intercepts = df_intercepts, slopes = df_slopes))
  }
}


regression_with_ref_centre <- function(bias_data, lab_col, cluster_col, measurement_col, reference_lab){
  bias_data_single_cluster <- bias_data %>% remove_clusters(., lab_col, cluster_col, measurement_col)

  bias_data_reference_lab <- bias_data_single_cluster %>%
    filter(.data[[lab_col]] == reference_lab) %>%
    rename("Ref_Intercept" = "Intercept") %>%
    rename("Ref_Slope" = "Slope") %>%
    select(-all_of(c(lab_col, cluster_col)))

  bias_data_single_cluster_with_reference <- left_join(
    filter(bias_data_single_cluster, .data[[lab_col]] != reference_lab),
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
    group_by(across(all_of(c(measurement_col, lab_col)))) %>%
    filter(.data[[cluster_col]] == min(.data[[cluster_col]])) %>%
    ungroup()
}


clean_bias_data <- function(data, lab_col, measurement_col, bias_data_names, non_bias_vector, bias_parameter){
  data %>%
    group_by(.data[[lab_col]]) %>%
    select(all_of(c(lab_col, measurement_col, bias_parameter))) %>%
    pivot_wider(names_from = all_of(measurement_col), values_from = all_of(bias_parameter)) %>%
    select(all_of(c(lab_col, bias_data_names))) %>%
    mutate(!!!setNames(as.list(rep(NA, length(non_bias_vector))), non_bias_vector)) %>%
    filter(!if_any(-all_of(non_bias_vector), is.na)) %>%
    rename(lab = all_of(lab_col)) %>%
    ungroup()
}
