#' @title
#' daily data to monthly data
#'
#' @author Fernando Prudencio
#'

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c("tidyverse", "ncdf4", "raster", "sf")

sapply(
  pkg,
  function(x) {
    is.there <- x %in% rownames(installed.packages())
    if (is.there == FALSE) {
      install.packages(x)
    }
  }
)

#' LOAD PACKAGES
library(raster)
library(ncdf4)
library(sf)
library(tidyverse)

#' BUILD FUNCTION TO GET MONTHLY DATA FROM DAILY DATA
daily_to_monthly <- function(lenght, data, start, end, ref, out) {
  #' build data frame of daily date
  period <- seq(as.Date(start), as.Date(end), by = "day")
  df_date <- tibble(date = period) %>% mutate(id = 1:n())

  #' build vector of monthly date
  monthly_date <-
    seq(as.Date(start), as.Date(end), by = "month") %>%
    str_sub(1, 7)

  #' n layers by monthly date
  n <- dplyr::filter(df_date, str_sub(date, 1, 7) == monthly_date[lenght])

  #' stacking data
  stck <- raster::stack(data[n$id]) %>% sum(na.rm = T)

  #' re-sampling data
  stck_rspl <- stck
  # stck_rspl <- raster::resample(stck, ref)

  #' name of monthly data
  name <- monthly_date[lenght]

  #' save monthly data
  writeRaster(
    stck_rspl,
    sprintf(
      "%1$strmm_3B42RT_monthly_%2$s.tif", out, name
    ),
    overwrite = T
  )
}

#' APPLY FUNCTION
#'   load reference raster
pisco <- raster("data/raster/pp/pisco/monthly/pisco_v2p1_stable_monthly.nc")
#'   list of daily data
lista <-
  list.files(
    "/home/fernando/Documentos/netCDF/data/tif",
    pattern = ".tif", full.names = T
  )
#'   output link
out <- "data/raster/pp/trmm/monthly/tif_world/"
#'   get monthly data
sapply(
  1:234,
  FUN = daily_to_monthly, lista,
  "2000-03-01", "2019-08-31", pisco, out
)

#' STACK AND SAVE MONTHLY DATA AS netCDF
#'   stack monthly data
# img <-
#   raster::stack(
#     list.files(
#       "data/raster/pp/trmm/monthly",
#       pattern = ".tif", full.names = T
#     )
#   )
#'   save monthly data
# writeRaster(img, "data/raster/pp/trmm/monthly/trmm_3B42RT_monthly.nc")