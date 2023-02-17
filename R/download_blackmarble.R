# TODO:
# 1. Sys.Date(), for day/month to check for downloading data.
# 2. Yearly datasets (so don't have to recreate all of them?)
# 3. Cloud mask: https://ladsweb.modaps.eosdis.nasa.gov/api/v2/content/archives/Document%20Archive/Science%20Data%20Product%20Documentation/VIIRS_Black_Marble_UG_v1.1_July_2020.pdf
#   file:///Users/robmarty/Downloads/Thesis_Zihao_Zheng.pdf

library(purrr)
library(furrr)
library(stringr)
library(rhdf5)
library(raster)
library(dplyr)
library(sf)
library(lubridate)

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

pad2 <- function(x){
  if(nchar(x) == 1) out <- paste0("0", x)
  if(nchar(x) == 2) out <- paste0(x)
  return(out)
}
pad2 <- Vectorize(pad2)

pad3 <- function(x){
  if(nchar(x) == 1) out <- paste0("00", x)
  if(nchar(x) == 2) out <- paste0("0", x)
  if(nchar(x) == 3) out <- paste0(x)
  return(out)
}
pad3 <- Vectorize(pad3)


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
  
  if(f %>% str_detect("VNP46A1|VNP46A2")){
    
    tile_i <- f %>% str_extract("h\\d{2}v\\d{2}")
    
    grid_sf <- read_sf("https://raw.githubusercontent.com/ramarty/download_blackmarble/main/data/blackmarbletiles.geojson")
    grid_i_sf <- grid_sf[grid_sf$TileID %in% tile_i,]
    
    grid_i_sf_box <- grid_i_sf %>%
      st_bbox()
    
    xMin <- min(grid_i_sf_box$xmin) %>% round()
    yMin <- min(grid_i_sf_box$ymin) %>% round()
    xMax <- max(grid_i_sf_box$xmax) %>% round()
    yMax <- max(grid_i_sf_box$ymax) %>% round()
    
    names(h5_data$HDFEOS$GRIDS$VNP_Grid_DNB$`Data Fields`)
    h5_data$HDFEOS$GRIDS$VNP_Grid_DNB$`Data Fields`$QF_Cloud_Mask %>% as.vector() %>% table()
    out <- h5_data$HDFEOS$GRIDS$VNP_Grid_DNB$`Data Fields`$`Gap_Filled_DNB_BRDF-Corrected_NTL`
    
  } else{
    lat <- h5_data$HDFEOS$GRIDS$VIIRS_Grid_DNB_2d$`Data Fields`$lat
    lon <- h5_data$HDFEOS$GRIDS$VIIRS_Grid_DNB_2d$`Data Fields`$lon
    out <- h5_data$HDFEOS$GRIDS$VIIRS_Grid_DNB_2d$`Data Fields`$NearNadir_Composite_Snow_Free
    #land_water_mask <- h5_data$HDFEOS$GRIDS$VIIRS_Grid_DNB_2d$`Data Fields`$Land_Water_Mask
    
    xMin <- min(lon) %>% round()
    yMin <- min(lat) %>% round()
    xMax <- max(lon) %>% round()
    yMax <- max(lat) %>% round()
  }
  
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
  outr[][outr[] %in% 65535] <- NA #TODO: Better way to do
  
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
  
  Sys.sleep(0.1)
  
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
    
    if(!is.null(days)){
      param_df <- param_df[as.numeric(param_df$day) %in% as.numeric(days),]
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
  
  year       <- file_name %>% substring(10,13)
  day        <- file_name %>% substring(14,16)
  product_id <- file_name %>% substring(1,7)
  
  wget_command <- paste0("wget -e robots=off -m -np .html,.tmp -nH --cut-dirs=3 ",
                         "'https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/5000/",product_id,"/", year, "/", day, "/", file_name,"'",
                         " --header 'Authorization: Bearer ",
                         bearer,
                         "' -P ",
                         temp_dir, "/")
  
  system(wget_command)
  
  r <- file_to_raster(file.path(temp_dir, product_id, year, day, file_name))
  
  unlink(file.path(temp_dir, product_id), recursive = T)
  
  return(r)
}


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
bm_mk_raster <- function(loc_sf = NULL,
                         tile_ids = NULL,
                         product_id,
                         time,
                         bearer,
                         mosaic = T,
                         mask = T){
  
  # Checks ---------------------------------------------------------------------
  if(is.null(loc_sf) & is.null(tile_ids)){
    stop("Either loc_sf or tile_ids must be specified")
  }
  
  if(!is.null(loc_sf) & !is.null(tile_ids)){
    stop("Both loc_sf and tile_ids cannot be specified; only one can be specified")
  }
  
  if(!is.null(loc_sf)){
    if(nrow(loc_sf) > 1){
      stop(paste0("loc_sf is ", nrow(loc_sf), " rows; must be 1 row. Dissolve polygon into 1 row."))
    }
  }
  
  # Determine grids to download ------------------------------------------------
  if(is.null(loc_sf)){
    
    tile_ids_rx <- tile_ids %>% paste(collapse = "|")
    
  } else{
    grid_sf <- read_sf("https://raw.githubusercontent.com/ramarty/download_blackmarble/main/data/blackmarbletiles.geojson")
    
    inter <- st_intersects(grid_sf, loc_sf, sparse = F) %>% as.vector()
    grid_use_sf <- grid_sf[inter,]
    
    tile_ids_rx <- grid_use_sf$TileID %>% paste(collapse = "|")
  }
  
  # Determine tiles to download ------------------------------------------------
  tiles_df <- read.csv(paste0("https://raw.githubusercontent.com/ramarty/download_blackmarble/main/data/",product_id,".csv"))
  tiles_df <- tiles_df[tiles_df$name %>% str_detect(tile_ids_rx),]
  
  ## Create year_month variable
  if(product_id == "VNP46A3"){
    
    tiles_df <- tiles_df %>%
      mutate(day = day %>% pad3(),
             month = day %>% month_start_day_to_month(),
             time = paste0(year, "-", month))
  }
  
  if(product_id == "VNP46A4"){
    tiles_df <- tiles_df %>%
      mutate(time = year)
  }
  
  # Download data --------------------------------------------------------------
  tiles_download_df <- tiles_df[tiles_df$time %in% time,]
  
  r_list <- lapply(tiles_download_df$name, function(name_i){
    download_raster(name_i, bearer)
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

r_big_mosaic <- function(r_list){
  
  ## Make template raster
  r_list_temp <- r_list
  
  names(r_list_temp)    <- NULL
  r_list_temp$tolerance <- 9999999
  
  r_temp <- do.call(raster::merge, r_list_temp)
  r_temp[] <- NA
  
  ## Resample to template
  for(i in 1:length(r_list)) r_list[[i]] <- raster::resample(r_list[[i]], 
                                                             r_temp, 
                                                             method = "ngb")
  
  ## Mosaic rasters together
  names(r_list)    <- NULL
  r_list$fun       <- max
  r_list$tolerance <- 999
  
  r <- do.call(raster::mosaic, r_list) 
  
  return(r)
}


#' Make Black Marble Raster
#' 
#' Make a raster of nighttime lights from [NASA Black Marble data](https://blackmarble.gsfc.nasa.gov/)
#' 
#' @param roi_sf Region of interest; sf polygon.
#' @product_id Either `VNP46A1`, `VNP46A2`, `VNP46A3`, or `VNP46A4`. `VNP46A1` is daily data, `VNP46A2` is daily data with additional corrections `VNP46A3` is monthly data, and `VNP46A4` is annual data. For more information, see [here](https://blackmarble.gsfc.nasa.gov/). 
#' @year Year of raster data. Required for product ID `VNP46A3` (monthly data) and `VNP46A4` (annual data); otherwise, ignored.
#' @month Month of raster data (values between `1-12`). Required for product ID `VNP46A3` (monthly data); otherwise, ignored.
#' @date Date of raster data (e.g. `"2021-03-01"`). Required for product IDs `VNP46A1` and `VNP46A2` (daily data); otherwise, ignored.
#' @bearer NASA bearer token. For instructions on how to create a bearer token, see [here](https://github.com/ramarty/download_blackmarble)
#' 
#' @return Raster
#'
#' @examples
#' \dontrun{
#' # Define bearer token
#' bearer <- "BEARER-TOKEN-HERE"
#' 
#' # sf polygon of Kenya
#' roi_sf <- getData('GADM', country='KEN', level=0) %>% st_as_sf()
#' 
#' # Daily data: raster for February 5, 2021
#' ken_20210205_r <- bm_raster(roi_sf = roi_sf,
#'                    product_id = "VNP46A4",
#'                    year = 2021,
#'                    day = 36,
#'                    bearer = bearer)
#' 
#' # Monthly data: raster for March 2021
#' ken_202103_r <- bm_raster(roi_sf = roi_sf,
#'                    product_id = "VNP46A4",
#'                   year = 2021,
#'                    month = 3,
#'                   bearer = bearer)
#' 
#' # Annual data: raster for 2021
#' ken_2021_r <- bm_raster(roi_sf = roi_sf,
#'                    product_id = "VNP46A4",
#'                    year = 2021,
#'                    bearer = bearer)
#'}
#'
#' @export
bm_raster <- function(roi_sf,
                      product_id,
                      year = NULL,
                      month = NULL,
                      date = NULL,
                      bearer){
  
  # Checks ---------------------------------------------------------------------
  if(nrow(roi_sf) > 1){
    stop("roi must be 1 row")
  }
  
  if(!("sf" %in% class(roi_sf))){
    stop("roi must be an sf object")
  }
  
  # Black marble grid ----------------------------------------------------------
  grid_sf <- read_sf("https://raw.githubusercontent.com/ramarty/download_blackmarble/main/data/blackmarbletiles.geojson")
  
  # Load dataframe of tiles ----------------------------------------------------
  if(product_id %in% c("VNP46A1", "VNP46A2")){
    
    day <- yday(date)
    year <- date %>% year()
    
    bm_files_df <- read.csv(paste0("https://raw.githubusercontent.com/ramarty/download_blackmarble/main/data/",
                                   product_id,
                                   "/", year, "/",
                                   product_id,"_",year,"_",pad3(day),".csv"))
    
    bm_files_df <- bm_files_df[bm_files_df$day %in% day,]
  }
  
  if(product_id == "VNP46A3"){
    bm_files_df <- read.csv(paste0("https://raw.githubusercontent.com/ramarty/download_blackmarble/main/data/VNP46A3/VNP46A3_",year,".csv"))
    
    bm_files_df <- bm_files_df[bm_files_df$month %in% month,]
  }
  
  if(product_id == "VNP46A4"){
    bm_files_df <- read.csv(paste0("https://raw.githubusercontent.com/ramarty/download_blackmarble/main/data/VNP46A4.csv"))
    
    bm_files_df <- bm_files_df[bm_files_df$year %in% year,]
  }
  
  # Intersecting tiles ---------------------------------------------------------
  # Remove grid along edges, which causes st_intersects to fail
  grid_sf <- grid_sf[!(grid_sf$TileID %>% str_detect("h00")),]
  grid_sf <- grid_sf[!(grid_sf$TileID %>% str_detect("v00")),]
  
  inter <- st_intersects(grid_sf, roi_sf, sparse = F) %>% as.vector()
  grid_use_sf <- grid_sf[inter,]
  
  # Make Raster ----------------------------------------------------------------
  tile_ids_rx <- grid_use_sf$TileID %>% paste(collapse = "|")
  bm_files_df <- bm_files_df[bm_files_df$name %>% str_detect(tile_ids_rx),]
  
  r_list <- lapply(bm_files_df$name, function(name_i){
    download_raster(name_i, bearer)
  })
  
  if(length(r_list) == 1){
    r <- r_list[[1]]
  } else{
    
    #r <- r_big_mosaic(r_list)
    
    #r_listr <<- r_list
    
    ## Mosaic rasters together
    names(r_list)    <- NULL
    r_list$fun       <- max
    
    r <- do.call(raster::mosaic, r_list) 
    
  }
  
  ## Crop
  r <- r %>% crop(roi_sf)
  
  return(r)
}


