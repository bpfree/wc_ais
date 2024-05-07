################################
### 04. combine AIS EEZ data ###
################################

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
## RDS files
data_dir <- "data/b_intermediate_data"

#####################################
#####################################

# parameters
year <- 2023
month <- "06"
region <- "wc"

#####################################

dir.create(file.path(data_dir, stringr::str_glue("{region}{year}")))
ais_comb_dir <- file.path(data_dir, stringr::str_glue("{region}{year}"))

rds_dir <- file.path(data_dir, stringr::str_glue("{region}{year}{month}"))

#####################################
#####################################

# vector of files
rds_files <- list.files(rds_dir, recursive = T, pattern = ".rds")

# i <- 1

rds_table <- data.frame(MMSI = numeric(),
                        BaseDateTime = character(),
                        LAT = numeric(),
                        LON = numeric(),
                        SOG = numeric(),
                        COG = numeric(),
                        VesselType = numeric(),
                        Length = numeric(),
                        Width = numeric(),
                        Draft = numeric())

loop_time <- Sys.time()

# load AIS RDS files 
for(i in 1:length(rds_files)){
  # read RDS data as separate files
  
  start2 <- Sys.time()
  
  ## generate template name
  data_name <- tools::file_path_sans_ext(rds_files[i])
  
  ## read the RDS file
  rds <- readRDS(file = file.path(rds_dir, rds_files[i]))
  
  ## assign template name (unique identifier) to CSV file
  assign(data_name, rds)
  
  #####################################
  
  rds_table <- rbind(rds_table, rds)
  
  # make latitude and VesselType numeric when not numeric
  rds_table <- rds_table %>%
    dplyr::mutate(across(c("LAT", "VesselType"), as.numeric))
  
  #####################################
  
  ## remove RDS
  rm(rds)
  
  # print how long it takes to calculate
  print(paste("Iteration", i, "of", length(rds_files), "takes", Sys.time() - start2, units(Sys.time() - start2), "to complete creating and adding", data_name, "data to", month, "of", year, "dataframe", sep = " "))
}

str(rds_table)

# print how long it takes to loop through dates
print(paste("Takes", Sys.time() - loop_time, units(Sys.time() - loop_time), "to complete creating and adding data to dataframe", sep = " "))

readr::write_rds(x = rds_table, file = file.path(ais_comb_dir, paste0(region, month, year, ".rds")))

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
