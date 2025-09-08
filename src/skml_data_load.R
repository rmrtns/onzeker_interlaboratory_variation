library("readxl")

skml_data_load_function <- function(){
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
      df <- read_xlsx(path = filenames[i])
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

