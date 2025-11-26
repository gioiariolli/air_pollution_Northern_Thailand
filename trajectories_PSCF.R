#---------------Settiamo la working Directory
rm(list = ls())
setwd("/Users/Riolli/Downloads/NSH_2020_shapefile")

library(sf)
library(dplyr)
library(openair)

# Preparazione dei data set e shapefile
prepare_traj <- function(shapefile_path) {
  traj_sf <- st_read(shapefile_path)
  
  # ID univoco
  traj_sf$traj_id <- seq_len(nrow(traj_sf))
  
  # LINESTRING in POINT
  traj_points <- traj_sf %>% st_cast("POINT")
  
  # Estrazione coordinate
  coords <- st_coordinates(traj_points)
  traj_points$lon <- as.numeric(coords[, "X"])
  traj_points$lat <- as.numeric(coords[, "Y"])
  
  # Rimuovere geometria per openair
  traj_points <- traj_points %>% st_drop_geometry()
  
  # Calcolare hour.inc per ogni traiettoria
  traj_points <- traj_points %>%
    group_by(traj_id) %>%
    mutate(hour.inc = row_number() - 1) %>%
    ungroup()
  
  # Convertire date
  traj_points$date <- as.POSIXct(strptime(traj_points$date, format="%Y%m%d"))
  
  return(traj_points)
}

# ------------------ Caricamento di tutti i datasets
traj_pointsNSH20 <- prepare_traj("NSH_2020.shp")
traj_pointsSH20  <- prepare_traj("SH_2020.shp")
traj_pointsNSH21 <- prepare_traj("NSH_2021.shp")
traj_pointsSH21  <- prepare_traj("SH_2021.shp")

# ------------------ Setting di limiti globali per evitare frame diversi
all_lon <- c(traj_pointsNSH20$lon, traj_pointsSH20$lon,
             traj_pointsNSH21$lon, traj_pointsSH21$lon)
all_lat <- c(traj_pointsNSH20$lat, traj_pointsSH20$lat,
             traj_pointsNSH21$lat, traj_pointsSH21$lat)

xlim_global <- range(all_lon, na.rm = TRUE)
ylim_global <- range(all_lat, na.rm = TRUE)

# ------------------ plotting delle traiettorie con open air library
plot_traj <- function(traj_data, title_name) {
  trajPlot(
    traj_data,
    xlim = xlim_global,
    ylim = ylim_global,
    asp = 1,
    main = title_name
  )
}

# ------------------ PSCF
plot_pscf <- function(traj_data, pollutant_col, title_name) {
  trajLevel(
    traj_data,
    pollutant = pollutant_col,   # colonna numerica con concentrazione
    statistic = "pscf",
    col = "increment",
    border = NA,
    xlim = xlim_global,
    ylim = ylim_global,
    asp = 1,
    main = title_name
  )
}

# Traiettorie plottate
plot_traj(traj_pointsNSH20, "Trajectories NSH20")
plot_traj(traj_pointsSH20,  "Trajectories SH20")
plot_traj(traj_pointsNSH21, "Trajectories NSH21")
plot_traj(traj_pointsSH21,  "Trajectories SH21")

# Limiti ristretti solo per PSCF
xlim_pscf <- c(85, 105)
ylim_pscf <- c(10, 22)

traj_pointsNSH20 |> 
  trajLevel(
    pollutant = "traj", 
    statistic = "pscf",
    col = "increment",
    border = NA,
    xlim = xlim_pscf,
    ylim = ylim_pscf,
    asp = 1,
    main = "Non-Smoke Haze 2020- PSCF"
  )

traj_pointsNSH21 |> 
  trajLevel(
    pollutant = "traj",  
    statistic = "pscf",
    col = "increment",
    border = NA,
    xlim = xlim_pscf,
    ylim = ylim_pscf,
    asp = 1,
    main = "Non-Smoke Haze 2021 - PSCF"
  )

traj_pointsSH20 |> 
  trajLevel(
    pollutant = "traj", 
    statistic = "pscf",
    col = "increment",
    border = NA,
    xlim = xlim_pscf,
    ylim = ylim_pscf,
    asp = 1,
    main = "Smoke Haze 2020 - PSCF"
  )

traj_pointsSH21 |> 
  trajLevel(
    pollutant = "traj", 
    statistic = "pscf",
    col = "increment",
    border = NA,
    xlim = xlim_pscf,
    ylim = ylim_pscf,
    asp = 1,
    main = "Smoke Haze 2021 - PSCF"
  )
