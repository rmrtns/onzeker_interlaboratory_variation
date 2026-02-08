
save_database_data <- function(data, base_dir, laboratory){
  base_dir <- paste0(base_dir, "individual_laboratories/")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(data, paste0(base_dir, "simulated_bias_induced_predictions", "_", laboratory, ".rds"))
}


save_summary <- function(summary_object, base_dir, model_name, analysis_type, population, description){
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  
  write.csv(
    summary_object,
    paste0(base_dir, model_name, "_", analysis_type, "_", population, "_", description, ".csv"),
    row.names = FALSE
  )
}


save_histogram <- function(histogram_object, base_dir, model_name, analysis_type, population){
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

  saveRDS(
    histogram_object,
    paste0(base_dir, model_name, "_", analysis_type, "_", population, "_", deparse(substitute(histogram_object)), ".rds")
  )
}