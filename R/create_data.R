# Create datasets of Black Marble tile data names

if(F){
  
  # Monthly: VNP46A3 -----------------------------------------------------------
  VNP46A3_df <- create_dataset_name_df(product_id = "VNP46A3",
                                       all = T)
  
  VNP46A3_df <- VNP46A3_df %>%
    dplyr::mutate(month = day %>% month_start_day_to_month())
  
  write.csv(VNP46A3_df, "~/Documents/Github/download_blackmarble/data/VNP46A3.csv", row.names = F)
  
  # Annual: VNP46A4 ---------------------------------------------------------------------
  VNP46A4_df <- create_dataset_name_df(product_id = "VNP46A4",
                                       all = T)
  
  write.csv(VNP46A4_df, "~/Documents/Github/download_blackmarble/data/VNP46A4.csv", row.names = F)
  
}