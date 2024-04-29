############################
### 0. Download AIS data ###
############################

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
               parallel,
               plyr,
               purrr,
               reshape2,
               stringr,
               tidyr)

#####################################
#####################################

# set directories
## download directory
download_dir <- "data/a_raw_data"

#####################################
#####################################

# parameters
year <- 2023
month <- 02
start_day <- 01
end_day <- lubridate::days_in_month(stringr::str_glue("{year}-{month}-{start_day}"))


#####################################
#####################################

### Note: adapted from Nick McMillan's code: https://github.com/NPR-investigations/rices-whale-speed-analysis/blob/main/analysis/1-download-marinecadastre-data.qmd

### AIS data hosting site: https://coast.noaa.gov/htdata/CMSP/AISDataHandler/2023/index.html

# Given a year, the function creates a vector for all the zip files to download
generate_ais_url = function(year){
  
  ## list dates for time frame of interest
  dates <- seq(from = ymd(stringr::str_glue("{year}-{month}-{start_day}")),
               to = ymd(stringr::str_glue("{year}-{month}-{end_day}")),
               by = "day")
  
  urls <- stringr::str_glue("https://coast.noaa.gov/htdata/CMSP/AISDataHandler/{year}/AIS_{year}_{format(dates, '%m')}_{format(dates, '%d')}.zip")
  return(urls)
}

urls <- generate_ais_url(year)
urls

#####################################
#####################################

# Define the path where the files will be saved
dest_path <- file.path(download_dir, year)

# Check if the directory exists, if not, create it
if (!dir.exists(dest_path)) {
  dir.create(dest_path, recursive = TRUE)
}

#####################################

# Function to download and save a file
download_save <- function(url, dest_path) {
  # Extract filename from the URL
  file_name <- basename(url)
  
  # Combine destination path and filename
  dest_file <- file.path(dest_path, file_name)
  
  # Attempt to download the file
  tryCatch({
    options(timeout=100000)
    
    download.file(url = url,
                  # place the downloaded file in correct data directory location
                  destfile = dest_file,
                  method = "auto",
                  mode = "wb")
    message("Downloaded: ", url)
  }, error = function(e) {
    message("Failed to download: ", url)
  })
}

#####################################

# Apply the function to each URL
# lapply(urls, fun = download_save, dest_path = dest_path)

# run parallel function to each URL
## set up the cluster
cl <- makeCluster(spec = parallel::detectCores(), # number of clusters wanting to create (use all possible cores available)
                  type = 'PSOCK')

## run the parallel function over the urls
work <- parallel::parLapply(cl = cl, X = urls, fun = download_save, dest_path = dest_path)

## stop the cluster
parallel::stopCluster(cl = cl)

#####################################
#####################################

# List all zip files in the directory
zip_files <- list.files(path = dest_path, pattern = "*.zip")
zip_files

zip_time <- Sys.time()

# Loop through each file
for(zip_file in zip_files){
  
  start2 <- Sys.time()

  # Generate full paths to zip and unzip locations
  zip_path <- file.path(dest_path, zip_file)

  # Unzip the file
  unzip(zipfile = zip_path,
        # keep in same location
        exdir = dest_path) #unzip_path

  # Delete the zipped file
  file.remove(zip_path)
}

#print how long it takes to loop through dates
print(paste("Takes", Sys.time() - zip_time, units(Sys.time() - zip_time), "to complete unzipping data", sep = " "))

#####################################
#####################################

# Downloaded AIS files
ais_files = list.files(dest_path,
                       full.names = T,
                       pattern = stringr::str_glue("AIS_{year}_0{month}"),
                       recursive = TRUE)

# Expected AIS files
dates <- seq(from = ymd(stringr::str_glue("{year}-{month}-{start_day}")),
             to = ymd(stringr::str_glue("{year}-{month}-{end_day}")),
             by = "day")
dates <- stringr::str_replace_all(dates, "-", "_")
expected_ais_files <- stringr::str_glue("{dest_path}/AIS_{dates}.csv")

if(sum(ais_files == expected_ais_files) == end_day){
  print("All files downloaded")
} else {
  print("Files are missing")
}

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
