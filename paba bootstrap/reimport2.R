library(readxl)
library(tidyr)


# Helper functions --------------------------------------------------------


# Function to get time-dependent columns from SKLM files,
# e.g. 2024.1A, 2024.2B, etc.
getTdpColnames <- function(df) {
  grep("\\b\\d{4}\\.\\d+[A-Z]\\b", names(df), value = TRUE, perl = TRUE)
}

# Function to convert specified columns to numeric
convertColsToNumeric <- function(df, cols) {
  df[cols] <- lapply(df[cols], as.numeric)
  return(df)
}

# Function to pivot time-dependent columns to long format and remove <, > 
pivotTdpToLong <- function(df) {
  tdp_cols <- getTdpColnames(df)
  df <- pivot_longer(df, cols = all_of(tdp_cols), 
                     names_to = "ctm", values_to = "Resultaat")
  df$ctm <- as.factor(df$ctm)
  df$Resultaat <- with(df, as.numeric(ifelse(grepl("[<>]", Resultaat), 
                                             NA, Resultaat)))
  return(df)
}

# Read SKML files
readSKMLFiles <- function(path, sheet_name) {
  files <- list.files(path, full.names = FALSE, pattern = "xlsx")
  if (length(files) == 0) 
    stop("Error: No Excel files found in the specified folder.")
  
  dfs <- lapply(file.path(path, files), read_excel, sheet = sheet_name, 
                col_types = "text")
  names(dfs) <- gsub(".xlsx", "", files)
  
  return(dfs)
}

# Function to convert all non-numeric columns to factors
convertNonNumColsToFactor <- function(df) {
  
  # Check which columns are not numeric
  non_num_cols <- names(df)[!sapply(df, is.numeric)]
  df[non_num_cols] <- lapply(df[non_num_cols], as.factor)
  
  return(df)
}


# Main functions ----------------------------------------------------------


importSKMLresultaten <- function(path) {
  SKMLresults <- readSKMLFiles(path, "Resultaten")
  # If these columns are present, convert them to numeric
  num_cols <- c( "Slope", "Intercept", "Gem.", "S.D.", "Sigma SA",
                 "Sigma TE",  "Score")
  SKMLresults <- lapply(SKMLresults, convertColsToNumeric, num_cols)
  # Pivot the time-dependent columns to long
  SKMLresults_long <- lapply(SKMLresults, pivotTdpToLong)
  # Rest to factor
  SKMLresults_long <- lapply(SKMLresults_long, convertNonNumColsToFactor)
  return(SKMLresults_long)
}

importSKMLconsensus <- function(path) {
  SKMLconsensus <- readSKMLFiles(path, "Consensuswaarden")
  # If these columns are present, convert them to numeric
  num_cols <- c("Gemiddelde", "Binnenlab", "Tussenlab", "n", "nU", 
                "Verw.", "SE", "%RSE")
  SKMLconsensus <- lapply(SKMLconsensus, convertColsToNumeric, num_cols)
  # Rest to factor
  SKMLconsensus <- lapply(SKMLconsensus, convertNonNumColsToFactor)
  return(SKMLconsensus)
}

# Function to bind and filter SKML data frames based on type 
# "results" Resultaten or "consensus" Consensuswaarden.
bindAndFilterSKMLdfs <- function(SKML_dfs, Bepalingen_vector, 
                                 type) {
  
  # Determine columns to select based on type
  if (type == "results") {
    select_cols <- c('Bepaling', 'ptp', 'ctr', 
                     'Resultaat', 'ctm')
  } else if (type == "consensus") {
    select_cols <- c('Bepaling', 'ctm', 'Gemiddelde', 'Methode')
  } else {
    stop("Error: type must be either 'results' or 'consensus'.")
  }
  
  # Initialize an empty list to store filtered data frames
  filtered_dfs <- list()
  
  # Loop through each data frame in the input list
  for (i in seq_along(SKML_dfs)) {
    df <- SKML_dfs[[i]]
    
    # Filter the data frame for the specified Bepalingen
    filtered_df <- df %>% filter(Bepaling %in% Bepalingen_vector) %>% 
      select(all_of(select_cols))
    
    # Append the filtered data frame to the list
    filtered_dfs[[i]] <- filtered_df
  }
  
  # Combine all filtered data frames into one
  combined_df <- do.call(rbind, filtered_dfs)
  
  return(combined_df)
}


selectPreferredMethod <- function(SKML_consensus_bind) {
  preferred_methods <- c("Referentiewaarde", "Expertwaarde", "Consensuswaarde")
  
  SKML_consensus_bind %>%
    filter(Methode %in% preferred_methods) %>%
    mutate(prio = match(Methode, preferred_methods)) %>%
    group_by(ctm) %>%
    slice_min(order_by = prio, n = 1) %>%
    ungroup() 
}


# Run ---------------------------------------------------------------------


SKML_results <- importSKMLresultaten("data/SKML")
SKML_consensus <- importSKMLconsensus("data/SKML")

# Voorbeeld voor cholesterol

choles_res <- bindAndFilterSKMLdfs(SKML_results, 
                                   c("Cholesterol", "HDL-Cholesterol"),
                                   type = "results")

choles_ref <- bindAndFilterSKMLdfs(SKML_consensus, 
                                   c("Cholesterol", "HDL-Cholesterol"),
                                   type = "consensus")


library(dplyr)

choles_referentiewaarde <- selectPreferredMethod(choles_ref)



