library("dplyr")
library("tidyr")

get_intercept_table <- function(data_SKML, reference_ptp, skml_names, non_bias_vec){

  df1 <- regression_with_ref_centre(data_SKML, reference_ptp)

  
  # get the bias estimates in the wide format
  df2 <- df1 %>%
    group_by(ptp) %>%
    select(c(Bepaling, New_Intercept )) %>%
    pivot_wider(names_from = Bepaling, values_from = c(New_Intercept)) 
  
  df3 <- df2 %>%
    # select("name", matches(skml_names)) %>%
    select(skml_names) %>%
    mutate(!!!setNames(as.list(rep(NA, length(non_bias_vec))), non_bias_vec))
  
  

  df4 <- extract_only_bias_variables(df3, skml_names)

  return(df4)
  
}

get_slope_table <- function(data_SKML,  reference_ptp, skml_names, non_bias_vec){


  df1 <- regression_with_ref_centre(data_SKML, reference_ptp)

  
  # get the bias estimates in the wide format
  df2 <- df1 %>%
    group_by(ptp) %>%
    select(c(Bepaling,New_Slope )) %>%
    pivot_wider(names_from = Bepaling, values_from = c(New_Slope)) 
  
  df3 <- df2 %>%
    # select("name", matches(skml_names)) %>%
    select(skml_names) %>%
    mutate(!!!setNames(as.list(rep(NA, length(non_bias_vec))), non_bias_vec))
  

  df4 <- extract_only_bias_variables(df3, skml_names)

  return(df4)
  
}

# set one centre as the refrence centre 

regression_with_ref_centre <- function(data_SKML, reference_ptp){

  df0 <- data_SKML %>%
    group_by(Bepaling, ptp) %>%
    filter(ctr == min(ctr))
  
  # seperate the reference ptp and designate the slope and intercept as the reference slope and intercept
  df1 <- subset(df0, ptp == reference_ptp)
  df1 <- df1 %>% 
    rename("Ref_Intercept" = "Intercept") %>% 
    rename("Ref_Slope" = "Slope") %>% 
    ungroup() %>% 
    select(-ptp, -ctr)
  
  # calculate the new slope and intercept with the reference bias
  df2 <- left_join(subset(df0, ptp != reference_ptp), 
                   df1, by = "Bepaling") %>% 
    mutate(New_Intercept = -((Slope*Ref_Intercept)/Ref_Slope) + Intercept) %>% 
    mutate(New_Slope = Slope/Ref_Slope) 
  
  return(df2)
}


extract_only_bias_variables <- function(data, skml_names){

  
  df1 <- data %>%
    filter(if_all(all_of(skml_names), ~ !is.na(.)))
  
  # df1 <- data %>% select(skml_names)
  # df2 <- df1 %>%
  #   rowwise() %>%
  #   filter(sum(is.na(c_across(everything()))) == 0)
  # df3 <- df1 %>%
  #   semi_join(df2, by = "ID")
  
  return(df1)
  
}

