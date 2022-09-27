# TODO:
# 1. Sys.Date(), for day/month to check for downloading data.

library(purrr)
library(furrr)
library(stringr)
library(rhdf5)
library(raster)

file_to_raster <- function(f){
  # Converts h5 file to raster.
  # ARGS
  # --f: Filepath to h5 file
  
  ## Boundaries
  # Only works on later years
  #spInfo <- h5readAttributes(f,"/") 
  
  #xMin<-spInfo$WestBoundingCoord
  #yMin<-spInfo$SouthBoundingCoord
  #yMax<-spInfo$NorthBoundingCoord
  #xMax<-spInfo$EastBoundingCoord
  
  ## Data
  h5_data <- H5Fopen(f) 
  lat <- h5_data$HDFEOS$GRIDS$VIIRS_Grid_DNB_2d$`Data Fields`$lat
  lon <- h5_data$HDFEOS$GRIDS$VIIRS_Grid_DNB_2d$`Data Fields`$lon
  out <- h5_data$HDFEOS$GRIDS$VIIRS_Grid_DNB_2d$`Data Fields`$NearNadir_Composite_Snow_Free
  #land_water_mask <- h5_data$HDFEOS$GRIDS$VIIRS_Grid_DNB_2d$`Data Fields`$Land_Water_Mask
  
  xMin <- min(lon) %>% round()
  yMin <- min(lat) %>% round()
  xMax <- max(lon) %>% round()
  yMax <- max(lat) %>% round()
  
  ## Metadata
  nRows      <- nrow(out)
  nCols      <- ncol(out)
  res        <- nRows
  nodata_val <- NA
  myCrs      <- 4326
  
  ## Make Raster
  
  #transpose data to fix flipped row and column order 
  #depending upon how your data are formatted you might not have to perform this
  out <- t(out)
  #land_water_mask <- t(land_water_mask)
  
  #assign data ignore values to NA
  out[out == nodata_val] <- NA
  
  #turn the out object into a raster
  outr <- raster(out,crs=myCrs)
  #land_water_mask_r <- raster(land_water_mask,crs=myCrs)
  
  #create extents class
  rasExt <- raster::extent(c(xMin,xMax,yMin,yMax))
  
  #assign the extents to the raster
  extent(outr) <- rasExt
  #extent(land_water_mask_r) <- rasExt
  
  #water to 0
  outr[][outr[] %in% 65535] <- 0 #TODO: Better way to do
  
  h5closeAll()
  
  return(outr)
}

read_bm_monthly_csv <- function(year, month_day_start){
  print(paste("Reading:", year, month_day_start))
  
  df_out <- tryCatch(
    {
      df <- read.csv(paste0("https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/5000/VNP46A3/",year,"/",month,".csv"))
      
      df$year <- year
      df$month_day_start <- month_day_start
      
      df
    },
    error = function(e){
      warning(paste0("Error with year: ", year, "; month_day_start: ", month_day_start))
      data.frame(NULL)
    }
  )
  
  return(df_out)
}

create_monthly_dataset_name_df <- function(all = TRUE,
                                           year = NULL, 
                                           month_day_start = NULL){
  
  month_param_df <- cross_df(list(year            = 2012:2022,
                                  month_day_start = c("001", "032", "061", "092", "122", "153", "183", "214", "245", "275", "306", "336")))
  
  monthly_files_df <- future_map2_dfr(month_param_df$year,
                                      month_param_df$month_day_start,
                                      read_bm_monthly_csv)
  
  return(monthly_files_df)
}

download_raster <- function(file_name, bearer){
  
  temp_dir <- tempdir()
  
  year <- file_name %>% substring(10,13)
  day  <- file_name %>% substring(14,16)
  
  wget_command <- paste0("wget -e robots=off -m -np .html,.tmp -nH --cut-dirs=3 ",
                         "'https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/5000/VNP46A3/", year, "/", day, "/", file_name,"'",
                         " --header 'Authorization: Bearer ",
                         bearer,
                         "' -P ",
                         temp_dir, "/")
  
  system(wget_command)
  
  r <- file_to_raster(file.path(temp_dir, "VNP46A3", year, day, file_name))
  
  unlink(file.path(temp_dir, "VNP46A3"), recursive = T)
  
  return(r)
}

month_start_day_to_month <- function(x){
  
  month <- NA
  
  if(x == "001") month <- "01"
  if(x == "032") month <- "02"
  if(x == "061") month <- "03"
  if(x == "092") month <- "04"
  if(x == "122") month <- "05"
  if(x == "153") month <- "06"
  if(x == "183") month <- "07"
  if(x == "214") month <- "08"
  if(x == "245") month <- "09"
  if(x == "275") month <- "10"
  if(x == "306") month <- "11"
  if(x == "336") month <- "12"
  
  return(month)
}

month_start_day_to_month <- Vectorize(month_start_day_to_month)
