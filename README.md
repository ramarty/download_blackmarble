# Download Nighttime Lights Black Marble Data

This package provides functions to download nighttime lights [Black Marble](https://blackmarble.gsfc.nasa.gov/) data. Black Marble data is downloaded from the [NASA LAADS Archive](https://ladsweb.modaps.eosdis.nasa.gov/missions-and-measurements/products/VNP46A3/). However, downloading data for specific regions for a long time period from the NASA LAADS Archive can be time consuming. This package automates the process of (1) downloading data from the NASA LAADS Archive, (2) converting files from H5 files to geotiff files, and (3) mosiacing files together (when needed).

## Installation

Eventually the package will be available via `devtools`; for now, load the packages using the below code.

```r
# install.packages(purrr)
# install.packages(furrr)
# install.packages(stringr)
# install.packages(rhdf5)
# install.packages(raster)

source("https://raw.githubusercontent.com/ramarty/download_blackmarble/main/R/download_blackmarble.R")
```

## Bearer Token

INSTRUCTIONS HERE ON GETTING BEARER TOKEN. ESSENTIALLY LOGIN TO LAADS ARCHIVE AND GET FROM THERE.

## Examples

EXAMPLES USING MAIN FUNCTIONS HERE
