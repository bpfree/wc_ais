### 

# Clear environment
rm(list = ls())

# Calculate start time of code (determine how long it takes to complete all code)
start <- Sys.time()

#####################################
#####################################

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,
               lubridate,
               plyr,
               purrr,
               reshape2,
               stringr,
               tidyr)

#####################################
#####################################

# set directories
## download directory
data_dir <- "data/a_raw_data"

## EEZ
wc_gpkg <- file.path(data_dir, "us_west_eez", "us_wc_eez.gpkg")

## year directory
year <- 2023
yr_dir <- file.path(data_dir, year)
yr_dir
list.files(yr_dir)

# RDS files
rds_dir <- "data/b_intermediate_data"

#####################################
#####################################

# fields
point_fields <- c("MMSI", "BaseDateTime", "LAT", "LON", "SOG", "COG", "VesselType", "Length", "Width", "Draft")
line_fileds <- c("MMSI", "BaseDateTime", "VesselType", "Length", "Width", "Draft")

#####################################

# vector of files
ais_files <- list.files(yr_dir, recursive = T, pattern = ".csv")
ais_files <- ais_files[1:2]

# load US west coast EEZ
us_wc_eez <- sf::st_read(dsn = wc_gpkg, layer = "us_wc_eez")

## parameters
xmin <- sf::st_bbox(obj = us_wc_eez)[1] # west
ymin <- sf::st_bbox(obj = us_wc_eez)[2] # south
xmax <- sf::st_bbox(obj = us_wc_eez)[3] # east
ymax <- sf::st_bbox(obj = us_wc_eez)[4] # north

for(i in 1:length(ais_files)){
  # read CSV data as separate files
  
  ## generate template name
  data_name <- tools::file_path_sans_ext(ais_files[i])
  
  ## read the CSV file
  csv <- read.csv(file = file.path(yr_dir, ais_files[i]))
  
  ## assign template name (unique identifier) to CSV file
  assign(data_name, csv)
  
  #####################################
  
  ## generate template name for west coast EEZ for day
  eez_name <- paste("wc", data_name, sep = "_")
  
  wc_ais <- csv %>%
    dplyr::filter(LON >= xmin & LON <= xmax,
                  LAT <= ymax & LAT >= ymin) %>%
    dplyr::select(all_of(point_fields))
  
  ## assign template name (unique identifier) to CSV file
  assign(eez_name, wc_ais)
  
  readr::write_rds(x = wc_ais, file = file.path(rds_dir, paste0(eez_name, ".rds")))
  
  #####################################
  
  ## remove CSV
  rm(csv)
  rm(wc_ais)
}

#### create transects from point data
day1 <- wc_AIS_2023_01_27

duplicated_data1 <- day1 %>%
  ## any latitudes below or above -90 and 90 or longitudes below and above -180 and 180 are not real
  dplyr::filter(between(LAT, -90, 90), between(LON, -180, 180)) %>%
  # remove duplicates 
  janitor::get_dupes(MMSI, BaseDateTime, LON, LAT)

duplicated_data2 <- day1 %>%
  ## any latitudes below or above -90 and 90 or longitudes below and above -180 and 180 are not real
  dplyr::filter(between(LAT, -90, 90), between(LON, -180, 180)) %>%
  # remove duplicates 
  janitor::get_dupes(MMSI, BaseDateTime)

test <- day1 %>%
  # remove bad coordinates
  ## any latitudes below or above -90 and 90 or longitudes below and above -180 and 180 are not real
  dplyr::filter(between(LAT, -90, 90), between(LON, -180, 180)) %>%
  # remove duplicates 
  distinct(MMSI, BaseDateTime, .keep_all = T) %>%
  dplyr::group_by(MMSI, BaseDateTime) %>%
  # sort by time stamp within vessels
  arrange(MMSI, BaseDateTime)


# calculate distances and times
## ***Note: geopackage was used originally by Kyle Dettloff (kyle.dettloff@noaa.gov) when creating the original dataset
##          The methods noted that the previous analysis used Vincenty ellipsoid method given it took the curvature
##          of the earth into consideration (https://sedarweb.org/documents/sedar-87-dw-01-estimation-of-commercial-shrimp-effort-in-the-gulf-of-mexico/)
##          Original methods: https://github.com/kyledettloff-NOAA/GOMshrimpEffort/blob/main/effort_scaled.R

## To learn more about the methods see here: http://www.movable-type.co.uk/scripts/latlong-vincenty.html
## and here: https://github.com/rspatial/geosphere/blob/master/R/distVincentyEllipsoid.R

## ***note: distances will need to be under 1 nautical mile (1 nautical mile = 1852 meters)
test1 <- test %>%
  arrange(MMSI, BaseDateTime) %>%
  head(50) %>%
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
               crs = "EPSG:4326", # EPSG 4326 (https://epsg.io/4326)
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

# Export data
sf::st_write(obj = shrimp_pings, dsn = shrimp_gpkg, layer = paste0("shrimp_pings", year_list[i]), append = F)



# run analysis
i <- 1
for(i in 1:length(year_list)){
  
  # designate loop start time
  start_time <- Sys.time()
  
  # designate fishing analysis start time
  fishing_pings <- Sys.time()
  
  #####################################
  
  # define annual object names for shrimp data
  ## shrimp ping data by year
  shrimp_ping_year <- paste0("shrimp_pings", year_list[i])
  
  ## shrimp ping data only in ocean by year
  shrimp_ping_ocean_year <- paste0("shrimp_pings_ocean", year_list[i])
  
  ## shrimp transect data by year
  shrimp_transect_year <- paste0("shrimp_transects", year_list[i])
  
  #####################################
  #####################################
  
  # create shrimp ping data for a particular year
  shrimp_pings <- pings_2014_2021 %>%
    # limit to fishing activity
    dplyr::filter(vessel_state == "fishing") %>%
    # filter for tows that start in year of interest
    ## ***Note: faster to do as separate filter than combining with activity filter
    dplyr::filter(.,
                  # search for the year within the STAMP field
                  stringr::str_detect(string = .$STAMP,
                                      pattern = year_list[i])) %>%
    # remove bad coordinates
    ## any latitudes below or above -90 and 90 or longitudes below and above -180 and 180 are not real
    filter(between(LATITUDE, -90, 90), between(LONGITUDE, -180, 180)) %>%
    # remove duplicates 
    distinct(VSBN, SERIAL, STAMP, LONGITUDE, LATITUDE) %>%
    
    # # sort by time stamp within vessels
    # arrange(VSBN, STAMP) %>%
    # group by vessel
    group_by(VSBN, SERIAL) %>%
    
    # sort by time stamp within vessels and serial numbers
    arrange(VSBN, SERIAL, STAMP) %>%
    
    # calculate distances and times
    ## ***Note: geopackage was used originally by Kyle Dettloff (kyle.dettloff@noaa.gov) when creating the original dataset
    ##          The methods noted that the previous analysis used Vincenty ellipsoid method given it took the curvature
    ##          of the earth into consideration (https://sedarweb.org/documents/sedar-87-dw-01-estimation-of-commercial-shrimp-effort-in-the-gulf-of-mexico/)
    ##          Original methods: https://github.com/kyledettloff-NOAA/GOMshrimpEffort/blob/main/effort_scaled.R
    
    ## To learn more about the methods see here: http://www.movable-type.co.uk/scripts/latlong-vincenty.html
    ## and here: https://github.com/rspatial/geosphere/blob/master/R/distVincentyEllipsoid.R
    
    ## ***note: distances will need to be under 1 nautical mile (1 nautical mile = 1852 meters)
  mutate(nm = geosphere::distVincentyEllipsoid(cbind(LONGITUDE, LATITUDE),
                                               cbind(lag(LONGITUDE), lag(LATITUDE))) / 1852,
         start_date = format(as.POSIXct(STAMP), format="%Y/%m/%d"),
         start_time = format(as.POSIXct(STAMP), format = "%H:%M:%S"),
         mins = as.numeric(STAMP - lag(STAMP), units = "mins")) %>%
    # move the "nm" to be before the "mins" field
    dplyr::relocate(nm,
                    .before = mins) %>%
    
    # convert to sf feature
    sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
                 # set the coordinate reference system to WGS84
                 crs = ping_crs, # EPSG 4326 (https://epsg.io/4326)
                 # keep longitude and latitude fields
                 remove = FALSE) %>%
    
    # create a transect field for each new vessel
    ## transect is defined as when the distance between points is under 1 nautical mile and within 30 minutes of the previous points
    ## ***note: a new transect will begin if the nautical mile distance is great than 1 or minutes difference is greater than 30
    ## ***note: new vessels will have NA for both the "nm" and "mins" fields and the transect will start at 0 again
    dplyr::mutate(transect = ifelse(test = (nm <= 1.0 & mins <= 30) | is.na(nm),
                                    yes = 0,
                                    no = 1) %>% cumsum(),
                  vessel_trans = paste(VSBN, SERIAL, transect, sep = "_"))
  
  # assign the shrimp pings data looped to templated annual data object
  assign(shrimp_ping_year, shrimp_pings)
  
  # Export data
  sf::st_write(obj = shrimp_pings, dsn = shrimp_gpkg, layer = paste0("shrimp_pings", year_list[i]), append = F)
  
  # calculate time to create annual shrimp fishing data
  fishing_total <- Sys.time() - fishing_pings
  print(fishing_total)
  
  #####################################
  #####################################
  
  # start time for ocean ping data analysis
  ocean_time <- Sys.time()
  
  # create shrimp ping data by year that do not fall on land
  shrimp_pings_ocean <- shrimp_pings %>%
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
  
  # assign the shrimp pings data looped to templated annual data object
  assign(shrimp_ping_ocean_year, shrimp_pings_ocean)
  
  # Export data
  sf::st_write(obj = shrimp_pings_ocean, dsn = shrimp_gpkg, layer = paste0("shrimp_pings_ocean", year_list[i]), append = F)
  
  # calculate time to create annual shrimp fishing data in only ocean areas
  ocean_total <- Sys.time() - ocean_time
  print(ocean_total)
  
  # print the time it takes to complete the first two parts (fishing and now ocean pings by year)
  second_phase <- Sys.time() - start_time
  print(paste("Time to complete first two analyses took:", second_phase, units(Sys.time() - start_time)))
  
  #####################################
  #####################################
  
  transect_time <- Sys.time()
  
  # create transects for marine only shrimp data by year
  shrimp_transects <- shrimp_pings_ocean %>%
    # group by the vessel transect
    dplyr::group_by(VSBN, vessel_trans) %>%
    # summarise all the associated points along the transect
    ## do_union = FALSE will make points get added in order for the transect
    dplyr::summarise(do_union = FALSE) %>%
    # add year back as a field
    dplyr::mutate(year = year_list[i]) %>%
    # relocate year before vessel and vessel transect
    dplyr::relocate(year,
                    .before = VSBN) %>%
    # set as simple feature (sf)
    sf::st_sf() %>%
    # ensure all points are multipoint
    ## ***note: this is for any transect that is a single point
    sf::st_cast(x = .,
                to = "MULTIPOINT") %>%
    # change all the points to linestring to make them a single line feature
    sf::st_cast(x = .,
                to = "LINESTRING") %>%
    # reproject the coordinate reference system to match BOEM call areas
    sf::st_transform("EPSG:5070") # EPSG 5070 (https://epsg.io/5070)
  
  # assign the shrimp transects data looped to templated annual data object
  assign(shrimp_transect_year, shrimp_transects)
  
  # Export data
  ## Geopackage
  sf::st_write(obj = shrimp_transects, dsn = shrimp_gpkg, layer = paste0("shrimp_transects", year_list[i]), append = F)
  
  # calculate time to create annual shrimp fishing transect data in only ocean areas
  transect_time <- Sys.time() - transect_time
  print(transect_time)
  
  #####################################
  #####################################
  
  # calculate total time to finish the three components (fishing, ocean, and transect)
  total_time <- Sys.time() - start_time
  print(paste0("Time to run the whole analysis for ", year_list[i], ": ", total_time, units(Sys.time() - start_time)))
}





#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
