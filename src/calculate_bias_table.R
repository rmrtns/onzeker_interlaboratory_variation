library(dplyr)
library(tidyr)

get_bias_table <- function(bias_data, reference_ptp, bias_data_names, non_bias_vector){

  df1 <- regression_with_ref_centre(bias_data, reference_ptp)
  # get the bias estimates in the wide format
  # Remove rows with missing in intercept
  cols_to_check <- names(bias_data_names)
  df_ints <- df1 %>%
    group_by(ptp) %>%

    select(c(ptp, Bepaling, New_Intercept )) %>%
    pivot_wider(names_from = Bepaling, values_from = c(New_Intercept)) %>% 
    select(ptp, all_of(bias_data_names)) %>%
    mutate(!!!setNames(as.list(rep(NA, length(non_bias_vector))), non_bias_vector)) %>% 
    # filter(!if_any(all_of(cols_to_check), is.na)) %>%  # hier gaat het mis
    filter(!if_any(-all_of(non_bias_vector), is.na)) %>%
    
    rename(lab = ptp)
  df_slopes <- df1 %>%
    group_by(ptp) %>%
    select(c(ptp, Bepaling, New_Slope )) %>%
    pivot_wider(names_from = Bepaling, values_from = c(New_Slope)) %>% 
    select(ptp, all_of(bias_data_names)) %>%
    mutate(!!!setNames(as.list(rep(NA, length(non_bias_vector))), non_bias_vector)) %>% 
    filter(!if_any(-all_of(non_bias_vector), is.na)) %>%
    # filter(!if_any(all_of(cols_to_check), is.na)) %>% # hier gaat het mis
    rename(lab = ptp)

  return(list(intercepts = df_ints, slopes = df_slopes))

  
}

# set one centre as the reference centre 

regression_with_ref_centre <- function(bias_data, reference_ptp){

  df0 <- bias_data %>%
    group_by(Bepaling, ptp) %>%
    filter(ctr == min(ctr))
  
  # seperate the reference ptp and designate the 
  # slope and intercept as the reference slope and intercept
  if (is.na(reference_ptp)) {
    df2 <- df0 %>% 
      rename("New_Intercept" = "Intercept") %>% 
      rename("New_Slope" = "Slope") 
  } else {
    
      # browser()
     df1 <- df0 %>% 
      ungroup() %>%
      filter(ptp == reference_ptp) %>% 
      rename("Ref_Intercept" = "Intercept") %>% 
      rename("Ref_Slope" = "Slope") %>% 
      select(-ptp, -ctr)
    
    # calculate the new slope and intercept with the reference bias
    df2 <- left_join(subset(df0, ptp != reference_ptp), 
                     df1, by = "Bepaling") %>% 
      mutate(New_Intercept = -((Slope*Ref_Intercept)/Ref_Slope) + Intercept) %>% 
      mutate(New_Slope = Slope/Ref_Slope) 
  }
  
  return(df2)
}


extract_only_bias_variables <- function(data, bias_data_names){

  df1 <- data %>%
    filter(if_all(all_of(bias_data_names), ~ !is.na(.)))
  return(df1)
  
}
