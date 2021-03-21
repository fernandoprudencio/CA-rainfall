#' @title
#' calculate index of agreement
#'
#' @author Fernando Prudencio
#'

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c("raster", "openair", "ncdf4", "sf", "tidyverse")

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
library(openair)
library(ncdf4)
library(sf)
library(tidyverse)

#' BUILD DATAFRAME OF DATE BY EACH DATASET
date_pisco <-
  tibble(
    date = seq(
      as.Date("1981-01-01"),
      as.Date("2016-12-31"),
      by = "month"
    )
  ) %>%
  dplyr::mutate(id = 1:n()) %>%
  dplyr::filter(
    date >= "2000-06-01",
    date <= "2016-12-31"
  )

date_gmp <-
  tibble(
    date = seq(
      as.Date("2000-06-01"),
      as.Date("2019-09-30"),
      by = "month"
    )
  ) %>%
  dplyr::mutate(id = 1:n()) %>%
  dplyr::filter(
    date >= "2000-06-01",
    date <= "2016-12-31"
  )

date_trmm <-
  tibble(
    date = seq(
      as.Date("2000-03-01"),
      as.Date("2019-08-31"),
      by = "month"
    )
  ) %>%
  dplyr::mutate(id = 1:n()) %>%
  dplyr::filter(
    date >= "2000-06-01",
    date <= "2016-12-31"
  )

#' LOAD VECTOR DATA
sf_world <-
  st_read(dsn = "data/vector/limits.gpkg", layer = "world_countries")

#' LINK OF RASTER DATASET
link_pisco <- "data/raster/pp/pisco/monthly/pisco_v2p1_stable_monthly.nc"
link_gpm <- "data/raster/pp/gpm/monthly/gpm_imerg_3b_monthly.nc"
link_trmm <- "data/raster/pp/trmm/monthly/trmm_3B42RT_monthly.nc"

#' LOAD RASTER DATA
pisco <-
  brick(link_pisco)[[date_pisco$id]] %>%
  raster::mask(sf_world)
gpm <-
  brick(link_gpm)[[date_gmp$id]] %>%
  raster::mask(sf_world)
trmm <-
  brick(link_trmm)[[date_trmm$id]] %>%
  raster::mask(sf_world)

#' GET TABLE OF RASTER
df_pisco <-
  getValues(pisco) %>%
  as_tibble()

df_gpm <-
  getValues(gpm) %>%
  as_tibble()

df_trmm <-
  getValues(trmm) %>%
  as_tibble()

#' CALCULATE INDEX OF AGREEMENT
# ini <- date()
for (i in 1:nrow(df_pisco)) {
  print(i)
  #' build dataframe for pixel
  df <-
    tibble(
      date =
        seq(
          as.Date("2000-06-01"),
          as.Date("2016-12-31"),
          by = "month"
        ),
      pisco = as.numeric(df_pisco[i, ]),
      gpm = as.numeric(df_gpm[i, ]),
      trmm = as.numeric(df_trmm[i, ])
    )
  #' calculate stats
  stat_gpm <- modStats(df, mod = "gpm", obs = "pisco", type = "season")
  stat_trmm <- modStats(df, mod = "trmm", obs = "pisco", type = "season")

  #' buidl matrix of results
  if (i == 1) {
    tbl_gpm <-
      tibble(
        ioa_autumn = stat_gpm$IOA[1],
        ioa_winter = stat_gpm$IOA[2],
        ioa_spring = stat_gpm$IOA[3],
        ioa_summer = stat_gpm$IOA[4],
        r_autumn = stat_gpm$r[1],
        r_winter = stat_gpm$r[2],
        r_spring = stat_gpm$r[3],
        r_summer = stat_gpm$r[4],
        coe_autumn = stat_gpm$COE[1],
        coe_winter = stat_gpm$COE[2],
        coe_spring = stat_gpm$COE[3],
        coe_summer = stat_gpm$COE[4],
        rmse_autumn = stat_gpm$RMSE[1],
        rmse_winter = stat_gpm$RMSE[2],
        rmse_spring = stat_gpm$RMSE[3],
        rmse_summer = stat_gpm$RMSE[4],
        mb_autumn = stat_gpm$MB[1],
        mb_winter = stat_gpm$MB[2],
        mb_spring = stat_gpm$MB[3],
        mb_summer = stat_gpm$MB[4],
        nmb_autumn = stat_gpm$NMB[1],
        nmb_winter = stat_gpm$NMB[2],
        nmb_spring = stat_gpm$NMB[3],
        nmb_summer = stat_gpm$NMB[4]
      )

    tbl_trmm <-
      tibble(
        ioa_autumn = stat_trmm$IOA[1],
        ioa_winter = stat_trmm$IOA[2],
        ioa_spring = stat_trmm$IOA[3],
        ioa_summer = stat_trmm$IOA[4],
        r_autumn = stat_trmm$r[1],
        r_winter = stat_trmm$r[2],
        r_spring = stat_trmm$r[3],
        r_summer = stat_trmm$r[4],
        coe_autumn = stat_trmm$COE[1],
        coe_winter = stat_trmm$COE[2],
        coe_spring = stat_trmm$COE[3],
        coe_summer = stat_trmm$COE[4],
        rmse_autumn = stat_trmm$RMSE[1],
        rmse_winter = stat_trmm$RMSE[2],
        rmse_spring = stat_trmm$RMSE[3],
        rmse_summer = stat_trmm$RMSE[4],
        mb_autumn = stat_trmm$MB[1],
        mb_winter = stat_trmm$MB[2],
        mb_spring = stat_trmm$MB[3],
        mb_summer = stat_trmm$MB[4],
        nmb_autumn = stat_trmm$NMB[1],
        nmb_winter = stat_trmm$NMB[2],
        nmb_spring = stat_trmm$NMB[3],
        nmb_summer = stat_trmm$NMB[4]
      )
  } else {
    tbl_gpm <-
      rbind(
        tbl_gpm,
        c(
          stat_gpm$IOA,
          stat_gpm$r,
          stat_gpm$COE,
          stat_gpm$RMSE,
          stat_gpm$MB,
          stat_gpm$NMB
        )
      )

    tbl_trmm <-
      rbind(
        tbl_trmm,
        c(
          stat_trmm$IOA,
          stat_trmm$r,
          stat_trmm$COE,
          stat_trmm$RMSE,
          stat_trmm$MB,
          stat_trmm$NMB
        )
      )
  }
  #' save table
  if (i == nrow(df_pisco)) {
    save(tbl_gpm, file = "data/rdata/tbl_gpm.RData")
    save(tbl_trmm, file = "data/rdata/tbl_trmm.RData")
  }
}
# fin <- date()

#' BUILD FUNCTION TO GET RASTER FROM TABLE
tbl_to_raster <- function(n, table, ref, versus) {
  #' names of season
  nm <- names(table)
  #' select error type and season
  error <-
    dplyr::select(table, nm[n]) %>%
    rename(value = nm[n])

  xy.coord <- coordinates(ref) %>%
    as.data.frame() %>%
    mutate(z = error$value) %>%
    as_tibble()

  data.crs <- crs(ref)
  data.res <- res(ref)
  data.grid <-
    rasterFromXYZ(
      xy.coord,
      data.res,
      data.crs,
      digits = 0
    )

  stat <- str_split(nm[n], pattern = "_")[[1]][1]
  season <- str_split(nm[n], pattern = "_")[[1]][2]

  writeRaster(
    data.grid,
    sprintf(
      "data/raster/pp/test/%3$s/%1$s_%3$s_%2$s.tif",
      versus, season, stat
    )
  )
}

#' APPLY FUNCTION TO GET RASTER FROM TABLE
#'   load RData file
load("data/rdata/tbl_gpm.RData")
load("data/rdata/tbl_trmm.RData")
#   apply function
sapply(1:24, FUN = tbl_to_raster, tbl_gpm, pisco, "gpm_vs_pisco")
