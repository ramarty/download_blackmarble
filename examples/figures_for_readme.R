# Figures for readme

library(ggplot2)
source("https://raw.githubusercontent.com/ramarty/download_blackmarble/main/R/download_blackmarble.R")

# Setup ------------------------------------------------------------------------
bearer <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJBUFMgT0F1dGgyIEF1dGhlbnRpY2F0b3IiLCJpYXQiOjE2NzY1Nzc5NzksIm5iZiI6MTY3NjU3Nzk3OSwiZXhwIjoxNjkyMTI5OTc5LCJ1aWQiOiJyYW1hcnR5IiwiZW1haWxfYWRkcmVzcyI6InJhbWFydHlAZW1haWwud20uZWR1IiwidG9rZW5DcmVhdG9yIjoicmFtYXJ0eSJ9.I2mL5S3JXg0p9S9nOPHF9_xyLT7CilOdqi2DgsF1GGc"
roi_sf <- getData('GADM', country='GHA', level=0) %>% st_as_sf()
product_id <- "VNP46A3"
year <- 2018
month <- 5
day <- 1

# Make map ---------------------------------------------------------------------
#### Make raster
r <- bm_raster(roi_sf = roi_sf,
               product_id = "VNP46A3",
               year = 2021,
               month = 10,
               bearer = bearer)

#### Prep data
r <- r %>% mask(roi_sf) 

r_df <- rasterToPoints(r, spatial = TRUE) %>% as.data.frame()
names(r_df) <- c("value", "x", "y")

## Transform NTL
r_df$value[r_df$value <= 1] <- 0

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
  labs(title = "Nighttime Lights: October 2021") +
  coord_quickmap() + 
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "none")

ggsave(p,
       filename = file.path("~/Documents/Github/download_blackmarble/man/figures/ntl_gha.png"),
       height = 5, width = 6)

