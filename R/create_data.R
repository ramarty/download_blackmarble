if(F){
  
  monthly_files_df <- create_monthly_dataset_name_df(all = T)
  write.csv(monthly_files_df, "~/Documents/Github/download_blackmarble/monthly_datasets.csv", row.names = F)
  
}