rm(list = ls())

library(tidyverse)
library(Hmisc)
library(raster)
library(ncdf4)

df <- tibble(
  date = seq(as.Date("2000-03-01"), as.Date("2019-08-01"), by = "1 month")
) %>%
  mutate(id = 1:n())

fun.clim <- function(month, data) {
  print(month)
  
  grd.mt <- df %>%
    filter(
      str_sub(date, 6, 7) == month
    )

  data[grd.mt$id] %>%
    raster::stack() %>%
    "*"(1) %>%
    mean(na.rm = T) %>%
    return()
}

grd.clim <- sapply(
  sprintf("%02d", 1:12),
  FUN = fun.clim,
  data =
    list.files(
      "data/raster/pp/trmm/monthly/tif_world",
      pattern = ".tif", full.names = T
    )
) %>%
  stack() %>%
  "*"(1) %>%
  "names<-"(sprintf(month.abb))

writeRaster(
  sum(grd.clim, na.rm = T),
  "data/raster/pp/trmm/clim/clim_world/trmm_clim_yearly_world.tif",
  overwrite = T
)

#' LOAD VECTOR DATA TO PLOT WHIT RASTER OF TEMPERATURE
#'   load world countries limits
#'     load sf data
sf.world <- st_read(
  dsn = "data/vector/limits.gpkg",
  layer = "world_countries", quiet = T, as_tibble = T
)

#' SMOOTING CLIMATOLOGY RASTER
index <-
  sum(grd.clim, na.rm = T) %>%
  raster::crop(sf.world) %>%
  raster::mask(sf.world)

#' Apply focal function to classified raster
#'   build function
conv <- function(x, i = 5) {
  if (!is.na(x)[i]) {
    return(mean(x, na.rm = T))
  } else {
    return(x[i])
  }
}
#'   apply function
for (i in 1:nlayers(index)) {
  band <- raster::focal(
    index[[i]],
    w = matrix(1, 3, 3),
    fun = conv,
    pad = TRUE, na.rm = FALSE
  )
  if (i == 1) img <- band else img <- raster::stack(img, band)
}

writeRaster(
  img,
  "data/raster/pp/trmm/clim/clim_world/trmm_clim_yearly_world_smooth2.tif",
  overwrite = T
)
