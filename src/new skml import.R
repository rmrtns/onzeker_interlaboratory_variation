library(readxl)
library(tidyr)
library(dplyr)

# Choose year to generate data, year must be subfolder
year <- "2024"

# Read all Excel files in data/SKML subfolder
files <- list.files(paste0("data/SKML/",year), pattern = "\\.xlsx|.xls$", full.names = TRUE)
data_list_results <- lapply(files, read_excel, sheet = "Resultaten", 
                            col_types = "text")
data_list_reference <- lapply(files, read_excel, sheet = "Consensuswaarden", 
                              col_types = "text")


# Cast the columns "2024.1A", "2024.1B", "2024.1C", etc. to long by using
# pivot and cast to numeric in data_list_results.
data_list_results_long <- lapply(data_list_results, function(df) {
  df_long <- pivot_longer(df, cols = starts_with(paste0(year)), 
                          names_to = "ctm", values_to = "Resultaat")
  df_long$Resultaat_chr <- df_long$Resultaat
  no_nums <- str_detect(df_long$Resultaat, "^\\d+(\\.\\d+)?$", negate = T)
  df_long$Resultaat[no_nums] <- NA
  df_long$Resultaat <- as.numeric(df_long$Resultaat)
  return(df_long)
})

# Select columns srv, ptp, ctr, anl, Bepaling, mth, Meting and Resultaat,
# and bind to single df
results_filt <- do.call(rbind, lapply(data_list_results_long, function(df) {
  df[, c("srv", "ptp", "ctr", "anl", "Bepaling", 
         "Methode", "ctm", "Resultaat")]
}))

# Select columns, anl, ctm, mth, Methode, Gemiddelede from data_list_reference
# and bind to single df and cast Gemiddelde to numeric
reference_filt <- do.call(rbind, lapply(data_list_reference, function(df) {
  df <- df[, c("ctm", "anl", "Bepaling", "Methode", "Gemiddelde")]
  df$Gemiddelde <- as.numeric(df$Gemiddelde)
  df <- subset(df, grepl("[A-Za-z]", ctm)) # drop .1 and .2 etc.
  df %>% rename(ConsensusWaarde = Gemiddelde)
}))

# For each Bepaling and ctm in reference_filt, select one Methode 
# in hierarchical order, Referentiewaarde, Expertwaarde, ALTM
reference_methods_by_bepaling <- reference_filt %>% 
  group_by(anl) %>%
  slice(which.min(match(Methode, c("Referentiewaarde", "Expertwaarde",  
                                   "ALTM")))) %>%
  ungroup() %>% 
  select(Bepaling, Methode) %>% 
  rename(ConsensusMethode = Methode)


reference_filt <- left_join(reference_methods_by_bepaling, 
                            reference_filt,
                            by = c("Bepaling", "ConsensusMethode" = "Methode"))

skml_merged <- left_join(reference_filt, results_filt, 
                         by = c("anl", "ctm", "Bepaling"))

# Count number of results per ptp, ctr, anl, and keep only >= 16
skml_merged <- skml_merged %>% 
  drop_na(Resultaat) %>% 
  group_by(ptp, ctr, anl) %>% 
  mutate(n = n())  %>% 
  ungroup()


write.csv(skml_merged, paste0("data/skml_merged_",year,".csv"), 
          row.names = FALSE)


