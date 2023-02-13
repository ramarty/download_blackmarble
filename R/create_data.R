# Create datasets of Black Marble tile data names

if(F){
  
  # Daily: VNP46A1 -------------------------------------------------------------
  for(year in 2012:2023){
    for(day in 1:366){
      
      if(year == 2012 & day < 19) next
      
      print(paste(year, day))
      
      dir.create(paste0("~/Documents/Github/download_blackmarble/data/VNP46A1/",year))
      OUT_FILE <- paste0("~/Documents/Github/download_blackmarble/data/VNP46A1/",year,"/VNP46A1_",year,"_",pad3(day),".csv")
      
      if(!file.exists(OUT_FILE)){
        
        out <- tryCatch(
          {
            
            VNP46A1_df <- create_dataset_name_df(product_id = "VNP46A1",
                                                 all = T, 
                                                 years = year,
                                                 days = day)
            
            VNP46A1_df <- VNP46A1_df %>%
              dplyr::select(-c(resourceType, md5sum, cksum, mtime, size, last_modified))
            
            write.csv(VNP46A1_df, OUT_FILE, row.names = F)
            
            Sys.sleep(2)
          },
          error=function(cond) {
            print("Error! Skipping.")
          }
        )
        
      }
    }
  }
  
  # Daily: VNP46A2 -------------------------------------------------------------
  for(year in 2012:2023){
    for(day in 1:366){
      
      if(year == 2012 & day < 19) next
      
      print(paste(year, day))
      
      dir.create(paste0("~/Documents/Github/download_blackmarble/data/VNP46A2/",year))

      OUT_FILE <- paste0("~/Documents/Github/download_blackmarble/data/VNP46A2/",year,"/VNP46A2_",year,"_",pad3(day),".csv")
      
      if(!file.exists(OUT_FILE)){

        out <- tryCatch(
          {
            
            VNP46A2_df <- create_dataset_name_df(product_id = "VNP46A2",
                                                 all = T, 
                                                 years = year,
                                                 days = day)
            
            VNP46A2_df <- VNP46A2_df %>%
              dplyr::select(-c(resourceType, md5sum, cksum, mtime, size, last_modified))
            
            write.csv(VNP46A2_df, OUT_FILE, row.names = F)
            
            Sys.sleep(2)
          },
          error=function(cond) {
            print("Error! Skipping.")
          }
        )
        
      }
    }
  }
  
  # Monthly: VNP46A3 -----------------------------------------------------------
  for(year in 2022:2023){
    VNP46A3_df <- create_dataset_name_df(product_id = "VNP46A3",
                                         all = T, 
                                         years = year)
    
    VNP46A3_df <- VNP46A3_df %>%
      dplyr::mutate(month = day %>% month_start_day_to_month())
    
    closeAllConnections()
    
    write.csv(VNP46A3_df, paste0("~/Documents/Github/download_blackmarble/data/VNP46A3/VNP46A3_",year,".csv"), row.names = F)
    Sys.sleep(10)
  }
  
  VNP46A3_all_df <- list.files("~/Documents/Github/download_blackmarble/data/VNP46A3", 
                               pattern = ".csv",
                               full.names = T) %>%
    str_subset("VNP46A3_") %>%
    map_df(read.csv)
  
  write.csv(VNP46A3_all_df, paste0("~/Documents/Github/download_blackmarble/data/VNP46A3.csv"), row.names = F)
  
  # Annual: VNP46A4 ---------------------------------------------------------------------
  VNP46A4_df <- create_dataset_name_df(product_id = "VNP46A4",
                                       all = T)
  
  closeAllConnections()
  
  write.csv(VNP46A4_df, "~/Documents/Github/download_blackmarble/data/VNP46A4.csv", row.names = F)
  
}