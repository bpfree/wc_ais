###############################
### 06. AIS points in ocean ###
###############################

# Clear environment
rm(list = ls())

# Calculate start time of code (determine how long it takes to complete all code)
start <- Sys.time()

#####################################
#####################################

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(DescTools,
               dplyr,
               lubridate,
               plyr,
               purrr,
               reshape2,
               stringr,
               tidyr)

#####################################
#####################################

# parameters
## year directory
year <- "2023"
month <- "01"
region <- "wc"

#####################################
#####################################

# set directories
## AIS and big islands data directory
data_dir <- "data/b_intermediate_data"

## AIS geopackage
ais_gpkg <- file.path(data_dir, stringr::str_glue("{region}_ais.gpkg"))

## land directory
land_gpkg <- "data/b_intermediate_data/land.gpkg"

## export geopackage
ais_ocean_gpkg <- file.path(data_dir, stringr::str_glue("{region}_ais_ocean.gpkg"))

#####################################

# load data
load_start <- Sys.time()

## load AIS time-distance data
month_point_time_distance <- sf::st_read(dsn = ais_gpkg, layer = stringr::str_glue("{region}_{year}{month}_time_distance"))
ais_time <- Sys.time()

## Load continental land data
continents <- sf::st_read(dsn = land_gpkg, layer = "continents")
continents_time <- Sys.time()

## Load big island land data
big_islands <- terra::readRDS(file = file.path(data_dir, "big_islands.RData"))
big_islands_time <- Sys.time()

## Load small island land data
small_islands <- sf::st_read(dsn = land_gpkg, layer = "small_islands")
small_islands_time <- Sys.time()

## Load very small island land data
very_small_islands <- sf::st_read(dsn = land_gpkg, layer = "very_small_islands")
very_small_islands_time <- Sys.time()

load_end <- Sys.time()
print(paste("Time to take load AIS data:", ais_time - load_start, units(ais_time - load_start),
            "Time to take load continents data:", continents_time - ais_time, units(continents_time - ais_time),
            "Time to take load big islands data:", big_islands_time - continents_time, units(big_islands_time - continents_time),
            "Time to take load small islands data:", small_islands_time - big_islands_time, units(small_islands_time - big_islands_time),
            "Time to take load very small islands data:", very_small_islands_time - small_islands_time, units(very_small_islands_time - small_islands_time),
            "Time to take load AIS and land data:", load_end - load_start, units(load_end - load_start)))

#####################################
#####################################

# only ocean based AIS points
ocean_time <- Sys.time()

month_point_time_distance_ocean <- month_point_time_distance %>%
  # check validity or fix any invalid geometry
  sf::st_make_valid() %>%
  
  # Remove continental land
  sf::st_difference(continents) %>%
  # Remove big island land
  sf::st_difference(big_islands) %>%
  # Remove small island land
  sf::st_difference(small_islands) %>%
  # Remove very small island land
  sf::st_difference(very_small_islands)

# calculate time to create annual shrimp fishing data in only ocean areas
print(Sys.time() - ocean_time)

## export month AIS data in ocean
sf::st_write(obj = month_point_time_distance_ocean, dsn = ais_ocean_gpkg, layer = paste0(region, "_", year, month, "_time_distance_ocean"), append = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
