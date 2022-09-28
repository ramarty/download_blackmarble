if(F){
  
  monthly_files_df <- create_monthly_dataset_name_df(all = T)
  
  monthly_files_df <- monthly_files_df %>%
    dplyr::mutate(month = month_day_start %>% month_start_day_to_month())
  
  write.csv(monthly_files_df, "~/Documents/Github/download_blackmarble/data/monthly_datasets.csv", row.names = F)
  
}