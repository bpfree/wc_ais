################################
### 07. Create AIS transects ###
################################

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
month <- "04"
region <- "wc"

crs <- "EPSG:4326"

#####################################
#####################################

# set directories
## transect data directory
intermediate_dir <- "data/b_intermediate_data"
data_dir <- "data/c_transect_data"

## AIS ocean geopackage
wc_ais_ocean_gpkg <- file.path(intermediate_dir, "wc_ais_ocean.gpkg")

### inspect layers
sf::st_layers(dsn = wc_ais_ocean_gpkg)

## export geopackage
### AIS transect geopackage
wc_ais_transect_gpkg <- file.path(data_dir, "wc_ais_transect.gpkg")

#####################################
#####################################

# create transects
## load monthly ocean AIS data
month_point_time_distance_ocean <- sf::st_read(dsn = wc_ais_ocean_gpkg, layer = stringr::str_glue("wc_{year}{month}_time_distance_ocean"))

transect_time <- Sys.time()

## calculate transects
month_transects <- month_point_time_distance_ocean %>%
  # group by the vessel transect
  dplyr::group_by(MMSI,
                  vessel_trans) %>%
  
  # summarise all the associated points along the transect
  ## do_union = FALSE will make points get added in order for the transect
  dplyr::summarise(do_union = FALSE,
                   sog_min = min(SOG),
                   sog_mean = mean(SOG),
                   sog_max = max(SOG),
                   sog_sd = sd(SOG),
                   length = DescTools::Mode(Length),
                   width = DescTools::Mode(Width),
                   # draft = DescTools::Mode(Draft),
                   vessel_type = DescTools::Mode(VesselType)) %>%
  
  # set as simple feature (sf)
  sf::st_sf() %>%
  # ensure all points are multipoint
  ## ***note: this is for any transect that is a single point
  sf::st_cast(x = .,
              to = "MULTIPOINT") %>%
  # change all the points to linestring to make them a single line feature
  sf::st_cast(x = .,
              to = "LINESTRING") %>%
  
  # reproject the coordinate reference system
  sf::st_transform(crs) # EPSG 4326 (https://epsg.io/4326)

print(Sys.time() - transect_time)

#####################################
#####################################

# export data
sf::st_write(obj = month_transects, dsn = wc_ais_transect_gpkg, layer = paste0(region, "_", year, month, "_transects"), append = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
