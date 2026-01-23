get_intercept_table <- function(data_SKML, reference_ptp, skml_names, non_bias_vec){
  df1 <- Regression_with_ref_centre_function(data_SKML, reference_ptp)
  
  # get the bias estimates in the wide format
  df2 <- df1 %>%
    group_by(ptp,ctr) %>%
    select(c(Bepaling, New_Intercept )) %>%
    pivot_wider(names_from = Bepaling, values_from = c(New_Intercept)) 
  
  df3 <- df2 %>%
    # select("name", matches(skml_names)) %>%
    select(skml_names) %>%
    mutate(!!!setNames(as.list(rep(NA, length(non_bias_vec))), non_bias_vec))
  
  return(df3)
  
}

get_slope_table <- function(data_SKML,  reference_ptp, skml_names, non_bias_vec){
  
  df1 <- Regression_with_ref_centre_function(data_SKML, reference_ptp)
  
  # get the bias estimates in the wide format
  df2 <- df1 %>%
    group_by(ptp,ctr) %>%
    select(c(Bepaling,New_Slope )) %>%
    pivot_wider(names_from = Bepaling, values_from = c(New_Slope)) 
  
  df3 <- df2 %>%
    # select("name", matches(skml_names)) %>%
    select(skml_names) %>%
    mutate(!!!setNames(as.list(rep(NA, length(non_bias_vec))), non_bias_vec))
  
  return(df3)
  
}

# set one centre as the refrence centre 
Regression_with_ref_centre_function <- function(data_SKML, reference_ptp){

  # seperate the reference ptp and designate the slope and intercept as the reference slope and intercept
  df1 <- subset(data_SKML, ptp == reference_ptp)
  df1 <- df1 %>% 
    rename("Ref_Intercept" = "Intercept") %>% 
    rename("Ref_Slope" = "Slope") %>% 
    ungroup() %>% 
    select(-ptp, -ctr)
  
  # calculate the new slope and intercept with the reference bias
  df2 <- left_join(subset(data_SKML, ptp != reference_ptp), 
                   df1, by = "Bepaling") %>% 
    mutate(New_Intercept = -((Ref_Slope*Ref_Intercept)/Ref_Slope) + Intercept) %>% 
    mutate(New_Slope = Slope/Ref_Slope) 
  
  return(df2)
}