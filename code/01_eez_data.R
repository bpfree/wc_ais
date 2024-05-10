####################
### 01. EEZ data ###
####################

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

## EEZ (download source: https://marineregions.org/downloads.php)
eez_dir <- "data/a_raw_data/World_EEZ_v12_20231025_gpkg/eez_v12.gpkg"

# US west coast EEZ
wc_dir <- dir.create(file.path(data_dir, "us_west_eez"))
wc_gpkg <- file.path(data_dir, "us_west_eez", "us_wc_eez.gpkg")

#####################################
#####################################

# parameters
region <- "wc"

#####################################
#####################################

# Global EEZ dataset (source: https://www.marineregions.org/)
## will need to fill out form for download to begin
eez <- sf::st_read(dsn = eez_dir, layer = sf::st_layers(dsn = eez_dir)[[1]][grep(pattern = "eez",
                                                                                 x = sf::st_layers(dsn = eez_dir)[1])]) %>%
  # filter for the correct exclusive economic zone
  dplyr::filter(GEONAME == "United States Exclusive Economic Zone") %>%
  # convert to polygon (so to get unique objects)
  sf::st_cast(to = "POLYGON") %>%
  # make all objects have valid geometry
  sf::st_make_valid()

## subset for the correct polygon object
us_wc_eez <- eez[2,]

#####################################
#####################################

# export exclusive economic zone
sf::st_write(obj = us_wc_eez, dsn = wc_gpkg, layer = stringr::str_glue("us_{region}_eez"), append = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
