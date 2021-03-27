rm(list = ls())

library(gdalUtilities)
library(tidyverse)
library(magick)

#' RESAMPLE RASTER DATA (DEM)
grd.ref <- raster(
  xmn = -85, xmx = -30,
  ymn = -35, ymx = 15
) %>%
  "res<-"(0.05) %>%
  "values<-"(0)


dem <- raster("data/raster/pp/trmm/clim/clim_world/trmm_clim_yearly_world_smooth3kernel.tif") %>%
  resample(grd.ref)

writeRaster(dem, "data/raster/pp/trmm/clim/clim_world/trmm_clim_yearly_world_smooth3kernel_rspl.tif")