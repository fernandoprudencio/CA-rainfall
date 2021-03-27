rm(list = ls())

library(rgee)
library(raster)
library(tidyverse)
library(sf)
library(mapedit)
library(HelpersMG)

ee_Initialize(drive = TRUE)

roi <-
  st_read(
    dsn = "data/vector/tiles_download.gpkg",
    layer = "tiles"
  )

# roi <- mapedit::editMap()
# roi2 <- mapedit::editMap()

for (i in 1:nrow(roi)) {
  print(i)
  ee_roi <- sf_as_ee(roi[i, ]$geometry)

  srtm <- ee$Image("srtm90_v4")$select("elevation")

  geom_params <- list(
    scale = 500,
    crs = "EPSG:4326",
    region = ee_roi
  )

  dem <- srtm$getDownloadUrl(geom_params)

  wget(dem)
}

lista <-
  list.files(
    "data/raster/dem/gee/",
    include.dirs = F,
    full.names = T
  )

for (k in 1:length(lista)) {
  print(k)
  unzip(lista[k], exdir = "data/raster/dem/gee/")
  file <- list.files("data/raster/dem/gee", pattern = "srtm90_v4.elevation.tif", full.names = T)
  file.rename(file, sprintf("%1$s%2$s.tif", str_sub(file, 1, -17), k))
}

# lst_img <- list.files("data/raster/dem/gee/", pattern = "*.tif", full.names = T)
# 
# for (m in 1:length(lst_img)) {
#   print(m)
# 
#   img1 <- raster(lst_img[m])
# 
#   if (m == 1) {
#     img <- img1
#   } else {
#     img <- raster::mosaic(img, img1, fun = mean)
#   }
# }