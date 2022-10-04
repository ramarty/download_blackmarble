# Examples

BEARER <- "BEARER-HERE"

ken_sf <- getData('GADM', country='KEN', level=0) %>% st_as_sf()

r <- bm_mk_raster(loc_sf = ken_sf,
                  product_id = "VNP46A4",
                  time = 2013,
                  bearer_token = BEARER,
                  mosaic = T,
                  mask = T)
