library(readxl)
library(tidyr)


# Helper function to get time-dependent columns from SKLM files,
# e.g. 2024.1A, 2024.2B, etc.
getTdpColnames <- function(df) {
  grep("\\b\\d{4}\\.\\d+[A-Z]\\b", names(df), value = TRUE, perl = TRUE)
}

convertNumColsToNumeric <- function(df) {
  
  # If these columns are present, convert them to numeric
  num_cols <- c( "Slope", "Intercept", "Gem.", "S.D.", "Sigma SA",
                 "Sigma TE",  "Score", "Gemiddelde", "Binnenlab",
                 "Tussenlab", "n", "nU", "Verw.", "SE", "%RSE")
  num_cols <- intersect(num_cols, names(df))
  df[num_cols] <- lapply(df[num_cols], as.numeric)
  
  # Get time-dependent columns and convert them to numeric
  num_cols_tdp <- getTdpColnames(df)
  
  # If num_cols_tdp is not zero then proceed
  if (length(num_cols_tdp) == 0) {
    # If results contain < or > signs, make them NA, this is to prevent 
    # as.numeric from throwing unnecessary warnings
    df[num_cols_tdp] <- lapply(df_result[num_cols_tdp], function(x) {
      x <- gsub("[<>]", "", x)
      as.numeric(x)
    })
  }
  
  return(df)
}

# Function to convert all non-numeric columns to factors
convertNonNumColsToFactor <- function(df) {
  
  # Check which columns are not numeric
  non_num_cols <- names(df)[!sapply(df, is.numeric)]
  df[non_num_cols] <- lapply(df[non_num_cols], as.factor)
  
  return(df)
}



readSKMLFiles <- function(path, sheet_name) {
  files <- list.files(path, full.names = FALSE, pattern = "xlsx")
  if (length(files) == 0) 
    stop("Error: No Excel files found in the specified folder.")
  
  dfs <- lapply(file.path(path, files), read_excel, sheet = sheet_name, 
                col_types = "text")
  names(dfs) <- gsub(".xlsx", "", files)
  
  return(dfs)
}

importSKMLresultaten <- function(path) {
  SKMLresults <- readSKMLFiles(path, "Resultaten")
  # If these columns are present, convert them to numeric
  num_cols <- c( "Slope", "Intercept", "Gem.", "S.D.", "Sigma SA",
                 "Sigma TE",  "Score")
  SKMLresults <- lapply(SKMLresults, function(df) {
    df[num_cols] <- as.numeric(df[num_cols])
    return(df)
  })
  # Reshape to long format using pivot_longer
  SKMLresults_long <- lapply(SKMLresults, function(df) {
    df[num_cols] <- as.numeric(df[num_cols])
    tdp_cols <- getTdpColnames(df)
    pivot_longer(df,
                 cols = all_of(tdp_cols),
                 names_to = "Periode",
                 values_to = "Resultaat")
  })
  return(SKMLresultst_long)
}


  
# Path should be the folder containing the SKML Excel files
importSKMLfilesInFolder <- function(path) {

  files <- list.files(path, full.names = F, pattern = "xlsx")
  if (length(files) == 0) 
    stop("Error: No Excel files found in the specified folder.")
  
  # We defer converting to specific columns types until after importing
  df_result <- lapply(paste(path, files, sep = "/"), read_excel, 
                            sheet = "Resultaten", col_types = "text")
  df_reference <- lapply(paste(path, files, sep = "/"), read_excel, 
                         sheet = "Consensuswaarden", col_types = "text")
  names(df_result) <- gsub(".xlsx", "", files)
  names(df_reference) <- gsub(".xlsx", "", files)
  
  # Function to convert specific columns to numeric
  
  
  # Apply convert functions to all elements in df_list and store in 
  # new converted list, df_list_conv
  df_list <- c(df_result, df_reference)
  df_result_conv <- lapply(df_result, convertNumColsToNumeric)
  df_result_conv <- lapply(df_result_conv, convertNonNumColsToFactor)
  df_reference_conv <- lapply(df_reference, convertNumColsToNumeric)
  df_reference_conv <- lapply(df_reference_conv, convertNonNumColsToFactor)
  
  
  return(list(resultaten = df_result_conv, consensus = df_reference_conv))
}

castSKMLdfToLong <- function(SKMLdf) {
  # Get time-dependent columns
  tdp_cols <- getTdpColnames(SKMLdf)
  
  # Reshape to long format using pivot_longer
  SKMLdf_long <- SKMLdf %>% pivot_longer(cols = all_of(tdp_cols),
                                         names_to = "Periode", 
                                         values_to = "Resultaat")
  # Extract Meting from Periode, i.e. A, B, C, D etc.
  SKMLdf_long$Meting <- sub("\\d{4}\\.\\d+([A-Z])", "\\1", SKMLdf_long$Periode)
  
  return(SKMLdf_long)
}

pabaRegression <- function()


# Development -------------------------------------------------------------

SKMLdfs <- importSKMLresultaten("data/SKML")

#SKMLdfs_long <- lapply(SKMLdfs, castSKMLdfToLong)
#SKMLdfs_long <- do.call(rbind, SKMLdfs_long)


