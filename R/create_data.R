# Create datasets of Black Marble tile data names

if(F){
  
  # Monthly: VNP46A3 -----------------------------------------------------------
  for(year in 2012:2022){
    VNP46A3_df <- create_dataset_name_df(product_id = "VNP46A3",
                                         all = T, 
                                         years = year)
    
    VNP46A3_df <- VNP46A3_df %>%
      dplyr::mutate(month = day %>% month_start_day_to_month())
    
    closeAllConnections()
    
    write.csv(VNP46A3_df, paste0("~/Documents/Github/download_blackmarble/data/VNP46A3_",year,".csv"), row.names = F)
    Sys.sleep(10)
  }
  
  VNP46A3_all_df <- list.files("~/Documents/Github/download_blackmarble/data", 
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