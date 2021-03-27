#' @title
#' Plot the spatial distribution of GVMI an GVMI anomalies
#'
#' @description
#' this script plots, as monitoring, the spatial distribution of GMVI and GVMI
#'   anomalies for each month of the current year
#'
#' @author Fernando Prudencio

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c(
  "tidyverse", "raster", "ncdf4", "sf", "lattice", "extrafont",
  "cptcity", "latticeExtra", "rasterVis", "maptools", "grid"
)

sapply(
  pkg,
  function(x) {
    is.there <- x %in% rownames(installed.packages())
    if (is.there == FALSE) {
      install.packages(x, dependencies = T)
    }
  }
)

#' LOAD PACKAGE
library(tidyverse)
library(raster)
library(sf)
library(lattice)
library(extrafont)
library(cptcity)
library(latticeExtra)
library(rasterVis)
library(stringr)
library(magick)
library(gridExtra)
library(grid)

#' LOAD VECTOR DATA TO PLOT WHIT RASTER OF TEMPERATURE
#'   load world countries limits
#'     load sf data
sf.world <- st_read(
  dsn = "data/vector/limits.gpkg",
  layer = "world_countries", quiet = T, as_tibble = T
)
#'     load sp data
sp.world <- as(st_geometry(sf.world), Class = "Spatial")

#'   load climat regions
sf.andes <- st_read(
  dsn = "data/vector/climatic_regions.gpkg",
  layer = "bastian_regions_dissolve", quiet = T,
  as_tibble = T
) %>%
  dplyr::filter(dissolve == 1)
#'     load sp data
sp.andes <- as(st_geometry(sf.andes), Class = "Spatial")

#' LOAD RASTER DATA
#'   load raster
index <-
  raster("data/raster/dem/mosaico_dem_south.tif") %>%
  raster::mask(sf.world)
#'   define values
index[index < 0] <- NA
index[index >= 6500] <- 6500
#'   reduce matrix size
index_crop <- raster::crop(index, extent(-83, -33, -23, 5))

#' CALCULATE SLOPE, ASPECT AND HILLSHADE
sl <- terrain(index_crop)
as <- terrain(index_crop, opt = "aspect")
hi <- hillShade(sl, as)
hi[hi < 0] <- 0
writeRaster(hi, "data/raster/hillshade.tif")

#' RECLASSIFY RASTER
#'   define labels
reclass_df <-
  c(
    1, 100, 1,
    100, 150, 2,
    150, 200, 3,
    200, 250, 4,
    250, 300, 5,
    300, 400, 6,
    400, 500, 7,
    500, 750, 8,
    750, 1000, 9,
    1000, 2000, 10,
    2000, 3000, 11,
    3000, 4000, 12,
    4000, Inf, 13
  )
#'   build matriz to reclass
reclass_m <-
  matrix(
    reclass_df,
    ncol = 3,
    byrow = TRUE
  )
#'   reclass raster
index_reclass <- raster::reclassify(index_crop, reclass_m)
test <- index_reclass

#' DEFINE INTERVAL OF VALUES
levels(test) <-
  data.frame(
    ID = 1:13,
    elev =
      c(
        "<100", "100-150", "150-200", "200-250",
        "250-300", "300-400", "400-500", "500-750",
        "750-1000", "1000-2000", "2000-3000", "3000-4000",
        "4000-6500"
      )
  )

#' BUILD PLOT
#'   Define color palette
cb.palette <- cptcity::cpt("ncl_topo_15lev")
# cptcity::find_cpt("topo_15lev")
#'   plot dem
map_elev <-
  levelplot(test,
    # main = list(
    #   "Elevation", cex = .8, fontfamily = "Source Sans Pro",
    #   fontface = "plain"
    # ),
    scales = list(
      x = list(
        limits = c(-83, -33), tick.number = 4
      ),
      y = list(
        at = c(-15, -10, -5, 0, 5, 10), limits = c(-23, 5), rot = 90,
        tick.number = 4
      ),
      draw = T
    ),
    alpha.regions = .6,
    col.regions = cb.palette,
    margin = F,
    pretty = T,
    maxpixels = 15e6,
    colorkey = list(
      height = .8, width = 1.1,
      # title = "[m]",
      # title.gpar = list(
      #   cex = .8,
      #   font = list(family = "Source Sans Pro"),
      #   col = "blue"
      # ),
      space = "right", tck = .3, # location of legend
      labels = list(cex = .7),
      font = list(family = "Source Sans Pro"),
      axis.line = list(lwd = 1, col = "black")
    ),
    xlab = NULL,
    ylab = NULL,
    par.settings = list(
      axis.text = list(fontfamily = "Source Sans Pro", cex = .6),
      axis.components = list(
        bottom = list(tck = .4, pad1 = .3),
        left = list(tck = .4, pad1 = .3)
      ),
      strip.background = list(col = "transparent"), # header fill for each map
      strip.border = list(col = "transparent", lwd = 1), # header line for each map
      panel.background = list(col = "gray"),
      axis.line = list(lwd = 1, col = "black")
    ),
    par.xlab.text = list(fontfamily = "Source Sans Pro"),
    par.ylab.text = list(fontfamily = "Source Sans Pro"),
    par.main.text = list(fontfamily = "Source Sans Pro", pad1 = -2),
    par.sub.text = list(fontfamily = "Source Sans Pro")
  ) +
  latticeExtra::layer(
    sp.lines(sp.world, col = "black", lwd = .7),
    sp.lines(sp.andes, col = "black", lwd = .7)
  )
#'   plot slope
# map_slope <-
#   levelplot(
#     sl,
#     par.settings = GrTheme,
#     alpha.regions = 1,
#     col.regions = gray(100:0 / 100),
#     maxpixels = 15e6
#   )
map_slope <-
  levelplot(
    sl, par.settings = GrTheme,
    at = seq(-1, 0, 0.01),
    interpolate = TRUE,
    maxpixels = 15e6,
    alpha.regions = 1,
    panel = panel.levelplot.raster,
    col.regions = gray(100:0 / 100)
  )

#'   plot hillshade
map_hill <-
  levelplot(
    hi,
    par.settings = GrTheme,
    alpha.regions = 1,
    interpolate = TRUE,
    col.regions = gray(0:100 / 100),
    panel = panel.levelplot.raster,
    maxpixels = 15e6
  )
#'   plot union of maps
map_union <- map_slope + map_hill
#'   Define plot name
name <- "export/elev_map_v3.png"
#'   Save plot
png(name, width = 20, height = 10, units = "cm", res = 300) # width = 20

map_elev + as.layer(map_union, under = T)

grid::grid.text(
  "SRTM (Elevation)",
  y = unit(.965, "npc"),
  rot = 0, x = unit(.5, "npc"),
  gp = gpar(
    fontsize = 10,
    # fontface = "bold",
    fontfamily = "Source Sans Pro",
    col = "black"
  )
)

#' CLOSE THE SAVED OF PLOT
dev.off()

#' TRIM FIGURE
img <- magick::image_read(name, strip = TRUE) %>%
  image_trim() %>%
  image_border("white", "50x50")

#' SAVE FIGURE
image_write(img, path = name, format = "png", quality = 100)
