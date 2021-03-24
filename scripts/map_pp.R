#' @title
#' Plot the spatial distribution of rainfall (PISCO, GPM, TRMM)
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

#' IF YOU WANT TO APPLAY MEAN FILTER
#'   run it
# index <- raster::focal(
#   index,
#   w = matrix(1, 3, 3),
#   fun = function(x) {
#     mean(x, na.rm = T)
#   },
#   pad = TRUE, na.rm = FALSE
# )

#' LOAD RASTER DATA
pisco <-
  brick("data/raster/pp/pisco/clim/pisco_clim_yearly.tif") %>%
  raster::mask(sf.world)

gpm <-
  brick("data/raster/pp/gpm/clim/gpm_clim_yearly.tif") %>%
  raster::mask(sf.world)

trmm <-
  brick("data/raster/pp/trmm/clim/trmm_clim_yearly.tif") %>%
  raster::mask(sf.world)

#' STACK RASTER
index <- raster::stack(pisco, gpm, trmm)

#' DEFINE INTERVAL OF VALUES
intrv <- c(seq(0, 6000, 500))
intrv.lbl <- c(seq(0, 6000, 1000))

#' BUILD PLOT
#'   Define color palette
cb.palette <- cptcity::cpt("ncl_precip_11lev")
# cptcity::find_cpt("precip_11lev")

#'   Define plot name
name <- "export/pp_map.png"
#'   Save plot
png(name, width = 60, height = 10, units = "cm", res = 500)

levelplot(index,
  names.attr = c("PISCO v2p1", "TRMM 3B42RT", "GPM IMERG 3b"),
  par.strip.text = list(cex = .8, lines = 1.5), # header size for each map
  # layout = c(4, 1), # define number of row and colums
  scales = list(
    x = list(
      limits = c(-81.8, -68.2), tick.number = 4 # , tck = .2
    ),
    y = list(
      at = c(-15, -10, -5, 0), limits = c(-18.5, .1), rot = 90,
      tick.number = 4
    ),
    draw = T
  ),
  col.regions = cb.palette,
  margin = F,
  pretty = T,
  maxpixels = 15e6,
  at = intrv,
  colorkey = list(
    at = intrv, height = .8, width = 1.1,
    space = "bottom", tck = .3, # location of legend
    labels = list(at = intrv.lbl, cex = .7),
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
  par.main.text = list(fontfamily = "Source Sans Pro"),
  par.sub.text = list(fontfamily = "Source Sans Pro")
) +
  latticeExtra::layer(
    sp.lines(sp.world, col = "black", lwd = .7),
    sp.lines(sp.andes, col = "black", lwd = .7)
  )

grid::grid.text(
  "[mm/year]",
  y = unit(.103, "npc"),
  rot = 0, x = unit(.387, "npc"),
  gp = gpar(
    fontsize = 8,
    # fontface = "bold",
    fontfamily = "Source Sans Pro",
    col = "black"
  )
)

grid::grid.text(
  "a)",
  y = unit(.241, "npc"),
  rot = 0, x = unit(.382, "npc"),
  gp = gpar(
    fontsize = 12,
    # fontface = "bold",
    fontfamily = "Source Sans Pro",
    col = "black"
  )
)

grid::grid.text(
  "b)",
  y = unit(.241, "npc"),
  rot = 0, x = unit(.4695, "npc"),
  gp = gpar(
    fontsize = 12,
    # fontface = "bold",
    fontfamily = "Source Sans Pro",
    col = "black"
  )
)

grid::grid.text(
  "c)",
  y = unit(.241, "npc"),
  rot = 0, x = unit(.557, "npc"),
  gp = gpar(
    fontsize = 12,
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
