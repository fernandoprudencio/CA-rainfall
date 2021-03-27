rm(list = ls())

library(gdalUtilities)
library(tidyverse)
library(magick)

#' TRIM FIGURE
name <- "export/study_area_map_v4.png"
img <- magick::image_read(name, strip = TRUE) %>%
  image_trim() %>%
  image_border("white", "50x50")

#' SAVE FIGURE
image_write(img, path = name, format = "png", quality = 100)

