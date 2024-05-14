#############################
### 0. Create Directories ###
#############################

# parameters
year <- 2023
region <- "wc"

#####################################
#####################################

# Define the path where the files will be saved
dest_path <- file.path("data")

# Check if the directory exists, if not, create it
if (!dir.exists(dest_path)) {
  dir.create(dest_path, recursive = TRUE)
}

# designate subdirectories
data_subdirectories <- c("a_raw_data",
                         "b_intermediate_data",
                         "c_transect_data")

#####################################

# create sub-directories within data directory
if (dir.exists(dest_path)) {
  for (i in 1:length(data_subdirectories)){
    subdirectories <- dir.create(file.path("data", data_subdirectories[i]))
  }
}

#####################################

# Define the path where the files will be saved
dest_path <- file.path("data/a_raw_data", year)

# Check if the directory exists, if not, create it
if (!dir.exists(dest_path)) {
  dir.create(dest_path, recursive = TRUE)
}

#####################################

# US west coast EEZ
## region directory
dest_path <- file.path("data/a_raw_data", stringr::str_glue("us_{region}_eez"))

## Check if the directory exists, if not, create it
if (!dir.exists(dest_path)) {
  dir.create(dest_path, recursive = TRUE)
}

#####################################

## create directory for region and year
dest_path <- file.path("data/b_intermediate_data", stringr::str_glue("{region}{year}"))

## Check if the directory exists, if not, create it
if (!dir.exists(dest_path)) {
  dir.create(dest_path, recursive = TRUE)
}

#####################################

# create code directory
code_dir <- dir.create("code")

#####################################

# create figure directory
figure_dir <- dir.create("figure")

#####################################
#####################################

# delete directory (if necessary)
### ***Note: change directory path for desired directory
#unlink("data/a_raw_data", recursive = T)
