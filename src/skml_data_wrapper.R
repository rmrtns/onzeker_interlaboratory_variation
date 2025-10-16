#packages 
library("readxl") 
library("dplyr")
library("tidyr")
library("mcr")

## main function to execute the pipeline
skml_data_wrapper_function <- function(vector_variable_of_interest){
  # A wrapper function to execute the SKML data pipeline in the correct order. 
  # This is the function that must be run in the application 

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
  # function to load the data from the "data" map it can recognize both excel files (.XLSX)
  # and comma-separated values files (.csv). It will output an list of dataframes with each 
  # dataframe corresponding to one loaded file 
  
  data_dir <- "data"  
  filenames <- list.files(data_dir,full.names=TRUE)
  dataframe_extracted <-list()
  j = 1 # used to position the dataframe in the list
  
  
  for(i in 1:length(filenames)) {
    if (grepl(".csv$",filenames[i])) {
      
      df_name <- paste0("df_",i)
      df <- read.csv(filenames[i])
      df_2 <- assign(df_name,df)
      
      dataframe_extracted[[j]] <- df_2
      j= j+1
      
      
    } else if (grepl(".xlsx$",filenames[i])) {
      
      df_name <- paste0("df_",i)
      df <- read_xlsx(path = filenames[i], sheet = sheetnumber)
      df_2 <- assign(df_name,df)
      
      dataframe_extracted[[j]] <- df_2
      j= j+1
      
    } else {
      
      print(paste0("file: (", filenames[i], ") is not formated as a csv or an xlsx"))
    }
    
    
  }
  

  return(dataframe_extracted)
  
}

## class correction function
skml_data_class_correction_function <- function(dataframe_list){
  # function to correct the classes in the measurements fields to numeric necessary for merging the data. 
  # The function recognizes the measurements column based on the perl grepl fucntion: "\\d{4}[.]\\d{1}\\D{1}".
  # meaning 4 numbers, a dot, one non-number, and one number corresponding to the encoding in the measurement column names.
  
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
  # function to pivot and merge the different dataframes in the dataframe list. It first pivots all measurements columns
  # using the same grepl perl function as seen in the class correction function.after these measurement colomns are pivoted.
  # the dataframe list is then merged in one main dataframe.
  
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
  # extracts the reference value out of the SKML data files and returns this as a dataframe.
  # it uses the SKML_data_load_function to load the excel files into this function.
  
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
  # a function that merges the dataframe with all the measurements with the corresponding references values into one dataframe
  
  df_merged <- inner_join(skml_dataframe, sklm_reference_dataframe,
                          by = c("Bepaling" = "Bepaling", "name" = "ctm"))
  return(df_merged)
  
}

## Bias calculation function
SKML_data_bias_function <- function(df, variable_list){
  # A function that calculates the A and B (which reflect the bias) for the passing-bablock method for each combination of 
  # ptp, ctr, and bepaling found the the dataframe (containing the measurements and the reference).
  
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


