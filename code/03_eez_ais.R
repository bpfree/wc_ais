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
year <- "2023"
month <- "03"
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
wc_gpkg <- file.path(data_dir, "us_west_eez", "us_wc_eez.gpkg")

## year directory
yr_dir <- file.path(data_dir, year)
# yr_dir
# list.files(yr_dir)

# RDS files
rds_dir <- "data/b_intermediate_data"

dir.create(file.path(rds_dir, paste0(region, year, month)))
ais_rds_dir <- file.path(rds_dir, paste0(region, year, month))

#####################################
#####################################

# vector of files
ais_files <- list.files(yr_dir, recursive = T, pattern = stringr::str_glue("AIS_{year}_{month}"))
# ais_files <- ais_files[1:2]
# ais_files

# load US west coast EEZ
us_wc_eez <- sf::st_read(dsn = wc_gpkg, layer = "us_wc_eez")

## parameters
xmin <- sf::st_bbox(obj = us_wc_eez)[1] # west
ymin <- sf::st_bbox(obj = us_wc_eez)[2] # south
xmax <- sf::st_bbox(obj = us_wc_eez)[3] # east
ymax <- sf::st_bbox(obj = us_wc_eez)[4] # north

loop_time <- Sys.time()

day_function <- function(ais_file){
  data_name <- tools::file_path_sans_ext(ais_file)

  ## read the CSV file
  csv <- read.csv(file = file.path(yr_dir, ais_file))

  # ## assign template name (unique identifier) to CSV file
  # assign(data_name, csv)

  #####################################

  ## generate template name for west coast EEZ for day
  eez_name <- paste(region, data_name, sep = "_")

  wc_ais <- csv %>%
    dplyr::filter(LON >= xmin & LON <= xmax,
                  LAT <= ymax & LAT >= ymin) %>%
    dplyr::select(all_of(point_fields))

  # ## assign template name (unique identifier) to CSV file
  # assign(eez_name, wc_ais)

  readr::write_rds(x = wc_ais, file = file.path(ais_rds_dir, paste0(eez_name, ".rds")))

  #####################################

  ## remove CSV
  rm(csv)
  rm(wc_ais)
}

cl <- parallel::makeCluster(spec = parallel::detectCores(), # number of clusters wanting to create
                            type = 'PSOCK')

# make sure packages within function get loaded within the cluster (have to do for any non-base libraries)
parallel::clusterCall(cl = cl, fun = library, package = 'tools', character.only = TRUE)
parallel::clusterCall(cl = cl, fun = library, package = 'dplyr', character.only = TRUE)
parallel::clusterCall(cl = cl, fun = library, package = 'readr', character.only = TRUE)

# add variables that need to get defined out of the function to the cluster
parallel::clusterExport(cl = cl,
                        varlist = c('region',
                                    'yr_dir',
                                    "xmin",
                                    "xmax",
                                    "ymin",
                                    "ymax",
                                    "point_fields",
                                    "ais_rds_dir"))

work <- parallel::parLapply(cl = cl, X = ais_files, fun = day_function)

parallel::stopCluster(cl = cl)

# # load AIS .csv files 
# for(i in 1:length(ais_files)){
#   # read CSV data as separate files
#   
#   start2 <- Sys.time()
#   
#   ## generate template name
#   data_name <- tools::file_path_sans_ext(ais_files[i])
#   
#   ## read the CSV file
#   csv <- read.csv(file = file.path(yr_dir, ais_files[i]))
#   
#   # ## assign template name (unique identifier) to CSV file
#   # assign(data_name, csv)
#   
#   #####################################
#   
#   ## generate template name for west coast EEZ for day
#   eez_name <- paste(region, data_name, sep = "_")
#   
#   wc_ais <- csv %>%
#     dplyr::filter(LON >= xmin & LON <= xmax,
#                   LAT <= ymax & LAT >= ymin) %>%
#     dplyr::select(all_of(point_fields))
#   
#   # ## assign template name (unique identifier) to CSV file
#   # assign(eez_name, wc_ais)
#   
#   readr::write_rds(x = wc_ais, file = file.path(ais_rds_dir, paste0(eez_name, ".rds")))
#   
#   #####################################
#   
#   ## remove CSV
#   rm(csv)
#   rm(wc_ais)
#   
#   # print how long it takes to calculate
#   print(paste("Iteration", i, "of", length(ais_files), "takes", Sys.time() - start2, units(Sys.time() - start2), "to complete limiting", data_name, "data to", region, sep = " "))
# }

# print how long it takes to loop through dates
print(paste("Takes", Sys.time() - loop_time, units(Sys.time() - loop_time), "to complete limiting all of", year, "for", region, sep = " "))

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
