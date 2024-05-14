##############################
### 03. AIS subset for EEZ ###
##############################

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

# parameters
year <- 2023
month <- "12"
region <- "wc"

## fields
point_fields <- c("MMSI", "BaseDateTime", "LAT", "LON", "SOG", "COG", "VesselType", "Length", "Width", "Draft")
# line_fileds <- c("MMSI", "BaseDateTime", "VesselType", "Length", "Width", "Draft")

#####################################
#####################################

# set directories
## download directory
data_dir <- "data/a_raw_data"

## EEZ
### use geopackage when running locally
eez_gpkg <- file.path(data_dir, stringr::str_glue("us_{region}_eez"), stringr::str_glue("us_{region}_eez.gpkg"))

### use .rds file when running on Microsoft Azure ML
# wc_rds <- filepath(data_dir, stringr::str_glue("us_{region}_eez"))

## year directory
yr_dir <- file.path(data_dir, year)
# yr_dir
# list.files(yr_dir)

## RDS files directory
rds_dir <- "data/b_intermediate_data"

## create directory for .RDS files
dest_path <- file.path(rds_dir, paste0(region, year, month))

### Check if the directory exists, if not, create it
if (!dir.exists(dest_path)) {
  dir.create(dest_path, recursive = TRUE)
}

### set directory for AIS .RDS files
ais_rds_dir <- file.path(rds_dir, paste0(region, year, month))

#####################################
#####################################

# vector of files
ais_files <- list.files(yr_dir, recursive = T, pattern = stringr::str_glue("AIS_{year}_{month}"))
# ais_files <- ais_files[1:2]
# ais_files

# load US west coast EEZ
## use geopackage when running locally
eez <- sf::st_read(dsn = eez_gpkg, layer = stringr::str_glue("us_{region}_eez"))

## use .rds file when running on Microsoft Azure ML
# eez <- readRDS(file = file.path(wc_rds, stringr::str_glue("us_{region}_eez.rds")))

## parameters
### boundaries of the US west coast EEZ
xmin <- sf::st_bbox(obj = eez)[1] # west
ymin <- sf::st_bbox(obj = eez)[2] # south
xmax <- sf::st_bbox(obj = eez)[3] # east
ymax <- sf::st_bbox(obj = eez)[4] # north

loop_time <- Sys.time()

day_function <- function(ais_file){
  data_name <- tools::file_path_sans_ext(ais_file)

  ## read the CSV file
  csv <- readr::read_csv(file = file.path(yr_dir, ais_file))

  #####################################

  ## generate template name for west coast EEZ for day
  eez_name <- paste(region, data_name, sep = "_")

  ## limit points to west coast EEZ
  region_ais <- csv %>%
    # subset points to ones within the longitude and latitude of the west coast EEZ
    dplyr::filter(LON >= xmin & LON <= xmax,
                  LAT <= ymax & LAT >= ymin) %>%
    # select all the fields that are relevant for the AIS transects
    dplyr::select(all_of(point_fields))

  ## export data as .RDS file
  readr::write_rds(x = region_ais, file = file.path(ais_rds_dir, paste0(eez_name, ".rds")))

  #####################################

  ## remove CSV and west coast AIS data
  rm(csv)
  rm(wc_ais)
}

#####################################
#####################################

# run parallel analysis for subsetting the AIS data within US west coast EEZ

## make the cluster
cl <- parallel::makeCluster(spec = parallel::detectCores(), # number of clusters wanting to create
                            type = 'PSOCK')

## make sure packages within function get loaded within the cluster (have to do for any non-base libraries)
parallel::clusterCall(cl = cl, fun = library, package = 'tools', character.only = TRUE)
parallel::clusterCall(cl = cl, fun = library, package = 'dplyr', character.only = TRUE)
parallel::clusterCall(cl = cl, fun = library, package = 'readr', character.only = TRUE)

## add variables that need to get defined out of the function to the cluster
parallel::clusterExport(cl = cl,
                        varlist = c('region',
                                    'yr_dir',
                                    "xmin",
                                    "xmax",
                                    "ymin",
                                    "ymax",
                                    "point_fields",
                                    "ais_rds_dir"))

## run parallel function
work <- parallel::parLapply(cl = cl, X = ais_files, fun = day_function)

## stop cluster
parallel::stopCluster(cl = cl)

# print how long it takes to loop through dates
print(paste("Takes", Sys.time() - loop_time, units(Sys.time() - loop_time), "to complete limiting all of", year, "for", region, sep = " "))

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
