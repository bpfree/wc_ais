###########################################
### 05. Calculate AIS time and distance ###
###########################################

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

## year directory
year <- "2023"
month <- "12"
region <- "wc"

crs <- "EPSG:4326"

#####################################
#####################################

# set directories
## AIS and big islands data directory
data_dir <- "data/b_intermediate_data"

# RDS files
rds_dir <- file.path(data_dir, stringr::str_glue("{region}{year}"))

## export geopackage
wc_ais_gpkg <- file.path(data_dir, "wc_ais.gpkg")

#####################################
#####################################

# vector of files
rds_files <- list.files(rds_dir, recursive = T, pattern = stringr::str_glue("wc{month}"))

## Load data
month_point <- readRDS(file = file.path(rds_dir, rds_files))

month_point_clean <- month_point %>%
  # remove bad coordinates
  ## any latitudes below or above -90 and 90 or longitudes below and above -180 and 180 are not real
  dplyr::filter(between(LAT, -90, 90),
                between(LON, -180, 180)) %>%
  # remove duplicates 
  distinct(MMSI, BaseDateTime, .keep_all = T) %>%
  dplyr::group_by(MMSI, BaseDateTime) %>%
  # sort by time stamp within vessels
  arrange(MMSI, BaseDateTime) %>%
  # ungroup to add in the information back to the vessel and date-time for later calculations
  dplyr::ungroup()

distance_time <- Sys.time()

## ***note: distances will need to be under 1 nautical mile (1 nautical mile = 1852 meters)
month_point_time_distance <- month_point_clean %>%
  
  arrange(MMSI, BaseDateTime) %>%
  dplyr::mutate(date_time = as.POSIXct(BaseDateTime, tz = "UTC", "%Y-%m-%dT%H:%M:%S"),
                nm = geosphere::distVincentyEllipsoid(cbind(LON, LAT),
                                                      cbind(lag(LON), lag(LAT))) / 1852,
                start_date = format(as.POSIXct(date_time), format="%Y/%m/%d"),
                start_time = format(as.POSIXct(date_time), format = "%H:%M:%S"),
                mins = as.numeric(date_time - lag(date_time), units = "mins")) %>%
  # move the "nm" to be before the "mins" field
  dplyr::relocate(nm,
                  .before = mins) %>%
  
  # convert to sf feature
  sf::st_as_sf(coords = c("LON", "LAT"),
               # set the coordinate reference system to WGS84
               crs = crs, # EPSG 4326 (https://epsg.io/4326)
               # keep longitude and latitude fields
               remove = FALSE) %>%
  
  # create a transect field for each new vessel
  ## transect is defined as when the distance between points is under 1 nautical mile and within 30 minutes of the previous points
  ## ***note: a new transect will begin if the nautical mile distance is great than 1 or minutes difference is greater than 30
  ## ***note: new vessels will have NA for both the "nm" and "mins" fields and the transect will start at 0 again
  dplyr::mutate(transect = ifelse(test = (nm <= 1.0 & mins <= 30) | is.na(nm),
                                  yes = 0,
                                  no = 1) %>% cumsum(),
                vessel_trans = paste(MMSI, transect, sep = "_"))

print(Sys.time() - distance_time)

#####################################

# export data
sf::st_write(obj = month_point_time_distance, dsn = wc_ais_gpkg, layer = paste0(region, "_", year, month, "_time_distance"), append = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
