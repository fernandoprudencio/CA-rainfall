#' @title
#' Plot the spatial distribution of error statistics
#'
#' @author Fernando Prudencio

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c(
  "tidyverse", "raster", "sf", "lattice", "extrafont", "cptcity",
  "latticeExtra", "rasterVis", "stringr", "magick", "gridExtra", "grid"
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

#' LOAD LIST OF RASTER
lst <- list.files(
  "data/raster/pp/test/ioa",
  pattern = ".tif",
  full.names = T
)

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
#'     load sf data
sf.region <- st_read(
  dsn = "data/vector/Andes_region.gpkg",
  layer = "Andes_region_clipped", quiet = T, as_tibble = T
)
#'     load sp data
sp.region <- as(st_geometry(sf.region), Class = "Spatial")

#' LOAD RASTER DATA
index <-
  aggregate(
    raster::stack(lst),
    fact = 2, fun = mean
  ) %>%
  raster::crop(sf.world) %>%
  raster::mask(sf.world)

#' Apply focal function to classified raster
#'   build function
conv <- function(x, i = 13) {
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
    w = matrix(1, 5, 5),
    fun = conv,
    pad = TRUE, na.rm = FALSE
  )
  if (i == 1) img <- band else img <- raster::stack(img, band)
}

names(img) <- names(index)

#' DEFINE INTERVAL OF VALUES
intrv <- seq(-1, 1, .2)
intrv.lbl <- seq(-1, 1, .2)

#' BUILD PLOT
#'   Define color palette
cb.palette <-
  c(
    #' blue scale
    "#053062", "#2167ad", "#4494c5", "#93c7e0",
    #' gray scale
    "#ffffff", "#ffffff",
    #' red scale
    "#f6a683", "#d8614e", "#b3182b", "#68001f"
  )

#'   Define plot name
name <- "export/stats/ioa_stats.png"
#'   Save plot
png(name, width = 20, height = 20, units = "cm", res = 500)

levelplot(img,
  names.attr =
    c(
      #' PISCO-GPM
      "a) PISCO-GPM (DJF)", "b) PISCO-GPM (MAM)",
      "c) PISCO-GPM (JJA)", "d) PISCO-GPM (SON)",
      #' PISCO-TRMM
      "e) PISCO-TRMM (DJF)", "f) PISCO-TRMM (MAM)",
      "g) PISCO-TRMM (JJA)", "h) PISCO-TRMM (SON)"
    ),
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
  col.regions = rev(cb.palette), # rev(cpt("es_landscape_es_landscape_59")), # rev(cb.palette),
  margin = F,
  pretty = T,
  maxpixels = 15e6,
  at = intrv,
  colorkey = list(
    at = intrv, height = .8, width = 1.1,
    space = "top", tck = .3, # location of legend
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
    sp.lines(sp.region, col = "black", lwd = .7)
  )

#' CLOSE THE SAVED OF PLOT
dev.off()

#' TRIM FIGURE
img <- magick::image_read(name, strip = TRUE) %>%
  image_trim() %>%
  image_border("white", "50x50")

#' SAVE FIGURE
image_write(img, path = name, format = "png", quality = 100)
