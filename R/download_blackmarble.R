# TODO:
# 1. Sys.Date(), for day/month to check for downloading data.
# 2. Yearly datasets (so don't have to recreate all of them?)

library(purrr)
library(furrr)
library(stringr)
library(rhdf5)
library(raster)
library(dplyr)

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

read_bm_csv <- function(year, 
                        day,
                        product_id){
  print(paste0("Reading: ", product_id, "/", year, "/", day))
  df_out <- tryCatch(
    {
      df <- read.csv(paste0("https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/5000/",product_id,"/",year,"/",day,".csv"))
      
      df$year <- year
      df$day <- day
      
      df
    },
    error = function(e){
      warning(paste0("Error with year: ", year, "; day: ", day))
      data.frame(NULL)
    }
  )
  
  return(df_out)
}

create_dataset_name_df <- function(product_id,
                                   all = TRUE,
                                   years = NULL, 
                                   months = NULL,
                                   days = NULL){
  
  #### Determine end year
  year_end <- Sys.Date() %>% 
    substring(1,4) %>% 
    as.numeric()
  
  #### Make parameter dataframe
  if(product_id %in% c("VNP46A1", "VNP46A2")){
    param_df <- cross_df(list(year = 2012:year_end,
                              day  = pad3(1:366)))
  }
  
  if(product_id == "VNP46A3"){
    param_df <- cross_df(list(year            = 2012:year_end,
                              day = c("001", "032", "061", "092", "122", "153", "183", "214", "245", "275", "306", "336",
                                      "060", "091", "121", "152", "182", "213", "244", "274", "305", "335")))
  }
  
  if(product_id == "VNP46A4"){
    param_df <- cross_df(list(year = 2012:year_end,
                              day  = "001"))
  }
  
  #### Add month if daily or monthly data
  if(product_id %in% c("VNP46A1", "VNP46A2", "VNP46A3")){
    
    param_df <- param_df %>%
      dplyr::mutate(month = day %>% 
                      month_start_day_to_month() %>%
                      as.numeric())
    
  }
  
  #### Subset time period
  ## Year
  if(!is.null(years)){
    param_df <- param_df[param_df$year %in% years,]
  }
  
  ## Month
  if(product_id %in% c("VNP46A1", "VNP46A2", "VNP46A3")){
    
    if(!is.null(months)){
      param_df <- param_df[as.numeric(param_df$month) %in% as.numeric(months),]
    }
    
  }
  
  #### Create data
  files_df <- map2_dfr(param_df$year,
                       param_df$day,
                       read_bm_csv,
                       product_id)
  
  return(files_df)
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
  
  if(x == "060") month <- "03"
  if(x == "061") month <- "03"
  
  if(x == "091") month <- "04"
  if(x == "092") month <- "04"
  
  if(x == "121") month <- "05"
  if(x == "122") month <- "05"
  
  if(x == "152") month <- "06"
  if(x == "153") month <- "06"
  
  if(x == "182") month <- "07"
  if(x == "183") month <- "07"
  
  if(x == "213") month <- "08"
  if(x == "214") month <- "08"
  
  if(x == "244") month <- "09"
  if(x == "245") month <- "09"
  
  if(x == "274") month <- "10"
  if(x == "275") month <- "10"
  
  if(x == "305") month <- "11"
  if(x == "306") month <- "11"
  
  if(x == "335") month <- "12"
  if(x == "336") month <- "12"
  
  return(month)
}

month_start_day_to_month <- Vectorize(month_start_day_to_month)

pad3 <- function(x){
  if(nchar(x) == 1) out <- paste0("00", x)
  if(nchar(x) == 2) out <- paste0("0", x)
  if(nchar(x) == 3) out <- paste0(x)
  return(out)
}
pad3 <- Vectorize(pad3)

#' Make Black Marble Raster
#'
#' Make raster from Black Marble data
#'
#' @param loc_sf `sf` object indicating location to query. If `"all"`, downloads data for whole world
#' @param product_id Black Marble product ID. `VNP46A3` for monthly.
#' @param time Time to query (depends on `product_id`). For annual data, year (e.g., `2012`). For monthly data, year and month (e.g., `2012-01`). For daily data, date (e.g., `2012-01-01`).
#' @param mosiac If multiple tiles needed to be downloaded to cover `loc_sf`, mosaic them together.
#' @param mask Mask final raster to `loc_sf`.
#'
#' @export
bm_mk_raster <- function(loc_sf,
                         product_id,
                         time,
                         mosaic = T,
                         mask = T){
  
  # Checks ---------------------------------------------------------------------
  if(nrow(loc_sf) > 1){
    stop(paste0("loc_sf is ", nrow(loc_sf), " rows; must be 1 row. Dissolve polygon into 1 row."))
  }
  
  # Determine grids to download ------------------------------------------------
  grid_sf <- read_sf("https://raw.githubusercontent.com/ramarty/download_blackmarble/main/data/blackmarbletiles.geojson")
  
  inter <- st_intersects(grid_sf, loc_sf, sparse = F) %>% as.vector()
  grid_use_sf <- grid_sf[inter,]
  
  # Determine tiles to download ------------------------------------------------
  tile_ids_rx <- grid_use_sf$TileID %>% paste(collapse = "|")
  
  if(product_id == "VNP46A3"){
    tiles_df <- read.csv("https://raw.githubusercontent.com/ramarty/download_blackmarble/main/data/monthly_datasets.csv")
    
    tiles_df <- tiles_df[tiles_df$name %>% str_detect(tile_ids_rx),]
    
    ## Create year_month variable
    tiles_df <- tiles_df %>%
      mutate(month_day_start = month_day_start %>% pad3(),
             month = month_day_start %>% month_start_day_to_month(),
             year_month = paste0(year, "-", month))
  }
  
  # Download data --------------------------------------------------------------
  tiles_download_df <- tiles_df[tiles_df$year_month %in% time,]
  
  r_list <- lapply(tiles_download_df$name, function(name_i){
    download_raster(name_i, BEARER)
  })
  
  # Mosaic/mask ----------------------------------------------------------------
  if(mosaic){
    
    ## If just one file, use the one file; otherwise, mosaic
    if(length(r_list) == 1){
      r_out <- r_list[[1]]
    } else{
      
      names(r_list)    <- NULL
      r_list$fun       <- max
      
      r_out <- do.call(raster::mosaic, r_list) 
    }
    
    ## Mask (only mask if mosaiced)
    if(mask){
      r_out <- r_out %>% crop(loc_sf) %>% mask(loc_sf)
    }
  } else{
    r_out <- r_list
  }
  
  return(r_out)
}
