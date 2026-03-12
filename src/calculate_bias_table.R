library(dplyr)
library(tidyr)


get_bias_table <- function(data_SKML, reference_ptp, skml_names, non_bias_vec){

  df1 <- regression_with_ref_centre(data_SKML, reference_ptp)
  # get the bias estimates in the wide format
  # Remove rows with missing in intercept
  cols_to_check <- names(skml_names)
  df_ints <- df1 %>%
    group_by(ptp) %>%

    select(c(ptp, Bepaling, New_Intercept )) %>%
    pivot_wider(names_from = Bepaling, values_from = c(New_Intercept)) %>% 
    select(ptp, skml_names) %>%
    mutate(!!!setNames(as.list(rep(NA, length(non_bias_vec))), non_bias_vec)) %>% 
    # filter(!if_any(all_of(cols_to_check), is.na)) %>%  # hier gaat het mis
    filter(!if_any(-all_of(non_bias_vec), is.na)) %>%
    
    rename(lab = ptp)
  df_slopes <- df1 %>%
    group_by(ptp) %>%
    select(c(ptp, Bepaling, New_Slope )) %>%
    pivot_wider(names_from = Bepaling, values_from = c(New_Slope)) %>% 
    select(ptp, skml_names) %>%
    mutate(!!!setNames(as.list(rep(NA, length(non_bias_vec))), non_bias_vec)) %>% 
    filter(!if_any(-all_of(non_bias_vec), is.na)) %>%
    # filter(!if_any(all_of(cols_to_check), is.na)) %>% # hier gaat het mis
    rename(lab = ptp)

  return(list(intercepts = df_ints, slopes = df_slopes))

  
}

# set one centre as the reference centre 

regression_with_ref_centre <- function(data_SKML, reference_ptp){

  df0 <- data_SKML %>%
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


extract_only_bias_variables <- function(data, skml_names){

  df1 <- data %>%
    filter(if_all(all_of(skml_names), ~ !is.na(.)))
  
  # df1 <- data %>% select((all_of(skml_names))
  # df2 <- df1 %>%
  #   rowwise() %>%
  #   filter(sum(is.na(c_across(everything()))) == 0)
  # df3 <- df1 %>%
  #   semi_join(df2, by = "ID")
  
  return(df1)
  
}
