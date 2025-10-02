#packages 
library("readxl") 
library("dplyr")
library("tidyr")
library("mcr")

## main function to execute the pipeline
skml_data_wrapper_function <- function(vector_variable_of_interest){

  # data pipeline
  list_df_1 <- skml_data_load_function()
  list_df_2 <- skml_data_class_correction_function(list_df_1)
  df_measurements <- skml_data_merge_function(list_df_2)
  df_reference <- skml_data_refrence()
  df_merged <- skml_data_to_reference_merge_function(df_measurements, df_reference)
  df_bias <- SKML_data_bias_function(df_merged,vector_variable_of_interest)
  
  return(df_bias)
  
}

## data loading function
skml_data_load_function <- function(sheetnumber = 1){
  
  data_dir <- "data"  
  filenames <- list.files(data_dir,full.names=TRUE)
  
  for(i in 1:length(filenames)) {
    if (grepl(".csv$",filenames[i])) {
      
      df_name <- paste0("df_",i)
      df <- read.csv(filenames[i])
      assign(df_name,df)
      rm(df)
      
      
    } else if (grepl(".xlsx$",filenames[i])) {
      
      df_name <- paste0("df_",i)
      df <- read_xlsx(path = filenames[i], sheet = sheetnumber)
      assign(df_name,df)
      rm(df)
      
    } else {
      
      print(paste0("file: (", filenames[i], ") is not formated as a csv or an xlsx"))
    }
    
    
  }
  
  df_only <- Filter(is.data.frame, mget(ls(), 
                                        envir = environment()
  )
  )
  
  return(df_only)
  
}

## class correction function
skml_data_class_correction_function <- function(dataframe_list){
  
  dataframe_corrected <-list()
  
  for(i in 1:length(dataframe_list)){
    
    indvidual_dataframe <- dataframe_list[[i]]
    measurment_columns <- names(indvidual_dataframe)[grepl("\\d{4}[.]\\d{1}\\D{1}",
                                                           names(indvidual_dataframe), 
                                                           perl = TRUE)]
    
    for(j in measurment_columns){
      
      indvidual_dataframe[[j]] <- as.numeric(indvidual_dataframe[[j]])
      
    }
    
    dataframe_corrected[[i]] <- indvidual_dataframe
    
  }
  
  return(dataframe_corrected)
}

## data list merge function
skml_data_merge_function <- function(dataframe_list){
  
  dataframe_pivot <-list()
  
  
  for(i in 1:length(dataframe_list)){
    
    indvidual_dataframe <- dataframe_list[[i]]
    
    dataframe_pivoted <- pivot_longer(indvidual_dataframe,
                                      names(indvidual_dataframe)[grepl("\\d{4}[.]\\d{1}\\D{1}",
                                                                       names(indvidual_dataframe), 
                                                                       perl = TRUE)])
    dataframe_pivot[[i]] <- dataframe_pivoted
  }
  
  
  merged_dataframe <- dplyr::bind_rows(dataframe_pivot)
  return(merged_dataframe)
  
}

## data loading reference function
skml_data_refrence <- function(){
  
  df_list <- skml_data_load_function(2)
  df_corrected <-list()
  
  for(i in 1:length(df_list)){
    
    indvidual_df <- df_list[[i]]
    indvidual_df[["Gemiddelde"]] <- as.numeric(indvidual_df[["Gemiddelde"]])
    df_corrected[[i]] <- indvidual_df
    
  }
  
  merged_df <- dplyr::bind_rows(df_corrected)
  Ref_expert_df <- merged_df %>% 
    filter(Methode == "Referentiewaarde" | Methode == "Expertwaarde") %>% #is expert waarden hetzelfde als consensus waarde 
    select(ctm, Bepaling,Methode, Gemiddelde)
  
  return(Ref_expert_df)
}

## data merge measurements and reference function
skml_data_to_reference_merge_function <- function(skml_dataframe, sklm_reference_dataframe){
  
  df_merged <- inner_join(skml_dataframe, sklm_reference_dataframe,
                          by = c("Bepaling" = "Bepaling", "name" = "ctm"))
  return(df_merged)
  
}

## Bias calculation function
SKML_data_bias_function <- function(df, variable_list){
  
  Loop_list <- df %>% 
    filter(Bepaling %in% variable_list) %>%
    distinct(ptp, ctr,Bepaling)
  
  Loop_list$B <- NA
  Loop_list$A <- NA
  Loop_list$remark <- NA
  
  N_observations <- 16 # als functie variable aanroepen
  
  for( i in 1:nrow(Loop_list)){
    
    df_filterd <- filter(df, ptp == Loop_list[[i,1]] 
                         & ctr == Loop_list[[i,2]]
                         & Bepaling == Loop_list[[i,3]])
    
    if(length(na.omit(df_filterd$Gemiddelde)) >= N_observations &
       length(na.omit(df_filterd$value)) >= N_observations){
      
      PB.reg <- mcr::mcreg(df_filterd$Gemiddelde,
                           df_filterd$value ,
                           method.reg = "PBequi",
                           method.ci="analytical",
                           na.rm = TRUE)
      
      b <- PB.reg@para[1]
      a <- PB.reg@para[2]
      
      Loop_list$B[i] <- b
      Loop_list$A[i] <- a
      
    } else {
      
      Loop_list$remark[i] <- paste("Non missing observations below ", N_observations)
      
    }
    
  }
  
  return(Loop_list)
  
}


