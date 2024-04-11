################################
### 04. Create AIS transects ###
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

## year directory
year <- "2023"
month <- "01"
region <- "wc"

# fields
# point_fields <- c("MMSI", "BaseDateTime", "LAT", "LON", "SOG", "COG", "VesselType", "Length", "Width", "Draft")
# line_fileds <- c("MMSI", "BaseDateTime", "VesselType", "Length", "Width", "Draft")

crs <- "EPSG:4326"

#####################################
#####################################

# set directories
## AIS and big islands data directory
data_dir <- "data/b_intermediate_data"

# RDS files
rds_dir <- file.path(data_dir, paste0(region, year))

load_start <- Sys.time()

## EEZ
# eez_gpkg <- file.path(data_dir, "us_west_eez", "us_wc_eez.gpkg")
# eez_time <- Sys.time()

## land directory
land_gpkg <- "data/b_intermediate_data/land.gpkg"

## export geopackage
wc_gpkg <- file.path(data_dir, "wc_ais.gpkg")

### load AIS time-distance data
# month_point_time_distance <- sf::st_read(dsn = wc_gpkg, layer = "wc_202301_time_distance")
ais_time <- Sys.time()

### Load continental land data
continents <- sf::st_read(dsn = land_gpkg, layer = "continents")
continents_time <- Sys.time()

### Load big island land data
big_islands <- terra::readRDS(file = file.path(data_dir, "big_islands.RData"))
big_islands_time <- Sys.time()

### Load small island land data
small_islands <- sf::st_read(dsn = land_gpkg, layer = "small_islands")
small_islands_time <- Sys.time()

### Load very small island land data
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

#####################################
#####################################

#####################################

# vector of files
rds_files <- list.files(rds_dir, recursive = T, pattern = ".rds")

## Load data
month_point <- readRDS(file = file.path(rds_dir, rds_files[1]))

# duplicated_data1 <- month_point %>%
#   ## any latitudes below or above -90 and 90 or longitudes below and above -180 and 180 are not real
#   dplyr::filter(between(LAT, -90, 90), between(LON, -180, 180)) %>%
#   # remove duplicates 
#   janitor::get_dupes(MMSI, BaseDateTime, LON, LAT)
# 
# duplicated_data2 <- month_point %>%
#   ## any latitudes below or above -90 and 90 or longitudes below and above -180 and 180 are not real
#   dplyr::filter(between(LAT, -90, 90), between(LON, -180, 180)) %>%
#   # remove duplicates 
#   janitor::get_dupes(MMSI, BaseDateTime)

month_point_clean <- month_point %>%
  # remove bad coordinates
  ## any latitudes below or above -90 and 90 or longitudes below and above -180 and 180 are not real
  dplyr::filter(between(LAT, -90, 90), between(LON, -180, 180)) %>%
  # remove duplicates 
  distinct(MMSI, BaseDateTime, .keep_all = T) %>%
  dplyr::group_by(MMSI, BaseDateTime) %>%
  # sort by time stamp within vessels
  arrange(MMSI, BaseDateTime) %>%
  # ungroup to add in the information back to the vessel and date-time for later calculations
  dplyr::ungroup()

# calculate distances and times
## ***Note: geopackage was used originally by Kyle Dettloff (kyle.dettloff@noaa.gov) when creating the original dataset
##          The methods noted that the previous analysis used Vincenty ellipsoid method given it took the curvature
##          of the earth into consideration (https://sedarweb.org/documents/sedar-87-dw-01-estimation-of-commercial-shrimp-effort-in-the-gulf-of-mexico/)
##          Original methods: https://github.com/kyledettloff-NOAA/GOMshrimpEffort/blob/main/effort_scaled.R

## To learn more about the methods see here: http://www.movable-type.co.uk/scripts/latlong-vincenty.html
## and here: https://github.com/rspatial/geosphere/blob/master/R/distVincentyEllipsoid.R

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





sf::st_layers(dsn = wc_gpkg)

month_point_time_distance_ocean <- sf::st_read(dsn = wc_gpkg, layer = "wc_202301_time_distance_ocean")

transect_time <- Sys.time()

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




transect_time <- Sys.time()

# create transects for marine only shrimp data by year
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
                   draft = DescTools::Mode(Draft),
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






# 
# # run analysis
# i <- 1
# for(i in 1:length(rds_files)){
#   
#   # designate loop start time
#   start_time <- Sys.time()
#   
#   # designate fishing analysis start time
#   fishing_pings <- Sys.time()
#   
#   #####################################
#   
#   # define annual object names for shrimp data
#   ## shrimp ping data by year
#   shrimp_ping_year <- paste0("shrimp_pings", rds_files[i])
#   
#   ## shrimp ping data only in ocean by year
#   shrimp_ping_ocean_year <- paste0("shrimp_pings_ocean", rds_files[i])
#   
#   ## shrimp transect data by year
#   shrimp_transect_year <- paste0("shrimp_transects", rds_files[i])
#   
#   #####################################
#   #####################################
#   
#   # create shrimp ping data for a particular year
#   shrimp_pings <- pings_2014_2021 %>%
#     # limit to fishing activity
#     dplyr::filter(vessel_state == "fishing") %>%
#     # filter for tows that start in year of interest
#     ## ***Note: faster to do as separate filter than combining with activity filter
#     dplyr::filter(.,
#                   # search for the year within the STAMP field
#                   stringr::str_detect(string = .$STAMP,
#                                       pattern = rds_files[i])) %>%
#     # remove bad coordinates
#     ## any latitudes below or above -90 and 90 or longitudes below and above -180 and 180 are not real
#     filter(between(LATITUDE, -90, 90), between(LONGITUDE, -180, 180)) %>%
#     # remove duplicates 
#     distinct(VSBN, SERIAL, STAMP, LONGITUDE, LATITUDE) %>%
#     
#     # # sort by time stamp within vessels
#     # arrange(VSBN, STAMP) %>%
#     # group by vessel
#     group_by(VSBN, SERIAL) %>%
#     
#     # sort by time stamp within vessels and serial numbers
#     arrange(VSBN, SERIAL, STAMP) %>%
#     
#     # calculate distances and times
#     ## ***Note: geopackage was used originally by Kyle Dettloff (kyle.dettloff@noaa.gov) when creating the original dataset
#     ##          The methods noted that the previous analysis used Vincenty ellipsoid method given it took the curvature
#     ##          of the earth into consideration (https://sedarweb.org/documents/sedar-87-dw-01-estimation-of-commercial-shrimp-effort-in-the-gulf-of-mexico/)
#     ##          Original methods: https://github.com/kyledettloff-NOAA/GOMshrimpEffort/blob/main/effort_scaled.R
#     
#     ## To learn more about the methods see here: http://www.movable-type.co.uk/scripts/latlong-vincenty.html
#     ## and here: https://github.com/rspatial/geosphere/blob/master/R/distVincentyEllipsoid.R
#     
#     ## ***note: distances will need to be under 1 nautical mile (1 nautical mile = 1852 meters)
#   mutate(nm = geosphere::distVincentyEllipsoid(cbind(LONGITUDE, LATITUDE),
#                                                cbind(lag(LONGITUDE), lag(LATITUDE))) / 1852,
#          start_date = format(as.POSIXct(STAMP), format="%Y/%m/%d"),
#          start_time = format(as.POSIXct(STAMP), format = "%H:%M:%S"),
#          mins = as.numeric(STAMP - lag(STAMP), units = "mins")) %>%
#     # move the "nm" to be before the "mins" field
#     dplyr::relocate(nm,
#                     .before = mins) %>%
#     
#     # convert to sf feature
#     sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
#                  # set the coordinate reference system to WGS84
#                  crs = ping_crs, # EPSG 4326 (https://epsg.io/4326)
#                  # keep longitude and latitude fields
#                  remove = FALSE) %>%
#     
#     # create a transect field for each new vessel
#     ## transect is defined as when the distance between points is under 1 nautical mile and within 30 minutes of the previous points
#     ## ***note: a new transect will begin if the nautical mile distance is great than 1 or minutes difference is greater than 30
#     ## ***note: new vessels will have NA for both the "nm" and "mins" fields and the transect will start at 0 again
#     dplyr::mutate(transect = ifelse(test = (nm <= 1.0 & mins <= 30) | is.na(nm),
#                                     yes = 0,
#                                     no = 1) %>% cumsum(),
#                   vessel_trans = paste(VSBN, SERIAL, transect, sep = "_"))
#   
#   # assign the shrimp pings data looped to templated annual data object
#   assign(shrimp_ping_year, shrimp_pings)
#   
#   # Export data
#   sf::st_write(obj = shrimp_pings, dsn = shrimp_gpkg, layer = paste0("shrimp_pings", rds_files[i]), append = F)
#   
#   # calculate time to create annual shrimp fishing data
#   fishing_total <- Sys.time() - fishing_pings
#   print(fishing_total)
#   
#   #####################################
#   #####################################
#   
#   # start time for ocean ping data analysis
#   ocean_time <- Sys.time()
#   
#   # create shrimp ping data by year that do not fall on land
#   
#   
#   # assign the shrimp pings data looped to templated annual data object
#   assign(shrimp_ping_ocean_year, shrimp_pings_ocean)
#   
#   # Export data
#   sf::st_write(obj = shrimp_pings_ocean, dsn = shrimp_gpkg, layer = paste0("shrimp_pings_ocean", rds_files[i]), append = F)
#   
#   
#   print(ocean_total)
#   
#   # print the time it takes to complete the first two parts (fishing and now ocean pings by year)
#   second_phase <- Sys.time() - start_time
#   print(paste("Time to complete first two analyses took:", second_phase, units(Sys.time() - start_time)))
#   
#   #####################################
#   #####################################
#   
#   transect_time <- Sys.time()
#   
#   
#   
#   # assign the shrimp transects data looped to templated annual data object
#   assign(shrimp_transect_year, shrimp_transects)
#   
#   # Export data
#   ## Geopackage
#   sf::st_write(obj = shrimp_transects, dsn = shrimp_gpkg, layer = paste0("shrimp_transects", rds_files[i]), append = F)
#   
#   # calculate time to create annual shrimp fishing transect data in only ocean areas
#   transect_time <- Sys.time() - transect_time
#   print(transect_time)
#   
#   #####################################
#   #####################################
#   
#   # calculate total time to finish the three components (fishing, ocean, and transect)
#   total_time <- Sys.time() - start_time
#   print(paste0("Time to run the whole analysis for ", rds_files[i], ": ", total_time, units(Sys.time() - start_time)))
# }

#####################################
#####################################

sf::st_write(obj = month_point_time_distance, dsn = wc_gpkg, layer = paste0(region, "_", year, month, "_time_distance"), append = F)
sf::st_write(obj = month_point_time_distance_ocean, dsn = wc_gpkg, layer = paste0(region, "_", year, month, "_time_distance_ocean"), append = F)
sf::st_write(obj = month_transects, dsn = wc_gpkg, layer = paste0(region, "_", year, month, "_transects"), append = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
