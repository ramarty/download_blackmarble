# Download Nighttime Lights Black Marble Data

This package provides functions to download nighttime lights [Black Marble](https://blackmarble.gsfc.nasa.gov/) data. Black Marble data is downloaded from the [NASA LAADS Archive](https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/5000/VNP46A3/). However, downloading data for specific regions for a long time period from the NASA LAADS Archive can be time consuming. This package automates the process of (1) downloading data from the NASA LAADS Archive, (2) converting files from H5 files to geotiff files, and (3) mosiacing files together (when needed).

## Installation

Eventually the package will be available via `devtools`; for now, load the packages using the below code.

```r
# install.packages(purrr)
# install.packages(furrr)
# install.packages(stringr)
# install.packages(rhdf5)
# install.packages(raster)
# install.packages(dplyr)
# install.packages(sf)

source("https://raw.githubusercontent.com/ramarty/download_blackmarble/main/R/download_blackmarble.R")
```

## Bearer Token

The function requires using a **Bearer Token**; to obtain a token, follow the below steps:

1. Go to the [NASA LAADS Archive](https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/5000/VNP46A3/)
2. Click "Login" (bottom on top right); create an account if needed.
3. Click "See wget Download Command" (bottom near top, in the middle)
4. After clicking, you will see text that can be used to download data. The "Bearer" token will be a long string in red.

## Quickstart

### bm_raster() function

The primary function in the package is `bm_raster`, which produces a raster of black marble nighttime lights. It can produce rasters from the following Black Marble products:

* `VNP46A1`: Daily nighttime lights
* `VNP46A2`: Daily nighttime lights with additional light corrections.
* `VNP46A3`: Monthly nighttime lights.
* `VNP46A4`: Annual nighttime lights.

For more infromation on the different nighttime lights products, see [here](https://blackmarble.gsfc.nasa.gov/).

The function takes the following arguments:
 
* `roi_sf`: Region of interest; sf polygon. Must be in the [WGS 84 (epsg:4326)](https://epsg.io/4326) coordinate reference system.
* `product_id`: Either: `VNP46A1`, `VNP46A2`, `VNP46A3`, or `VNP46A4`.
* `year`: Year of raster data. Always required.
* `month`: Month of raster data (values between `1-12`). Required for product ID `VNP46A3` (monthly data); otherwise, ignored.
* `day`: Day of raster data (values between `1-366`). Required for product IDs `VNP46A1` and `VNP46A2` (daily data); otherwise, ignored.
* `bearer`: NASA bearer token. For instructions on how to create a bearer token, see [here](https://github.com/ramarty/download_blackmarble)

### Examples

The below example shows making daily, monthly, and annual rasters of nighttime
lights for Ghana.

```r
#### Setup
# Define NASA bearer token
bearer <- "BEARER-TOKEN-HERE"

# Define region of interest (roi). The roi must be (1) an sf polygon and (2)
# in the WGS84 (epsg:4326) coordinate reference system. Here, we use the 
# getData function to load a polygon of Kenya
roi_sf <- getData('GADM', country='GHA', level=0) %>% st_as_sf()

#### Make Rasters
### Daily data: raster for February 5, 2021

# Feb 5 is the 36th day of the year
yday("2021-02-05")

r_20210205 <- bm_raster(roi_sf = roi_sf,
                            product_id = "VNP46A2",
                            year = 2021,
                            day = 36,
                            bearer = bearer)
  
### Monthly data: raster for October 2021
r_202110 <- bm_raster(roi_sf = roi_sf,
                          product_id = "VNP46A3",
                          year = 2021,
                          month = 10,
                          bearer = bearer)

### Annual data: raster for 2021
r_2021 <- bm_raster(roi_sf = roi_sf,
                        product_id = "VNP46A4",
                        year = 2021,
                        bearer = bearer)
```

We can then plot the raster

```r
#### Prep data
r_2021 <- r_2021 %>% mask(roi_sf) 

r_df <- rasterToPoints(r_2021, spatial = TRUE) %>% as.data.frame()
names(r_df) <- c("value", "x", "y")

## Remove very low values of NTL; can be considered noise 
r_df$value[r_df$value <= 2] <- 0

## Distribution is skewed, so log
r_df$value_adj <- log(r_df$value+1)

##### Map 
p <- ggplot() +
  geom_raster(data = r_df, 
              aes(x = x, y = y, 
                  fill = value_adj)) +
  scale_fill_gradient2(low = "black",
                       mid = "yellow",
                       high = "red",
                       midpoint = 4.5) +
  labs(title = "NTL, October 2021") +
  coord_quickmap() + 
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "none")

```

<p align="center">
<img src="man/figures/ntl_gha.png" alt="Mapbox Example Polygon" width="800"/>
</p>

Relying on the `writeRaster` from the `raster` package, the rasters can be saved as `.tif` files. The [`exactextractr`](https://github.com/isciences/exactextractr) package can be used to aggregate raster values to different administrative zones. 


