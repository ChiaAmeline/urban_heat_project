# Import libraries
install.packages("tidyverse", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("sf", dependencies = TRUE)
install.packages("tmap", dependencies = TRUE)
install.packages("measurements", dependencies = TRUE)
install.packages("spData",dependencies = TRUE)
install.packages("googledrive", dependencies = TRUE)
install.packages("readxl")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(sf)
library(tmap)
library(measurements)
library(spData)
library(googledrive)
library(readxl)

# 1. DATA CONNECTION VIA GOOGLE DRIVE

#Using Google Drive to connect to the csv files as advised by Prof. Pls install the GoogleDrive package above before running this chunk.

##### File ID from Google Drive URL (For GlobalLandTemperaturesByCity)
file_id_temp_cities <- "17lPSwGwt5HTMbIiaTkotfmuKlkKZAPcM"

# Download the file to a temporary location
temp_cities <- tempfile(fileext = ".csv")
drive_download(as_id(file_id_temp_cities), path = temp_cities, overwrite = TRUE)


# Read the CSV file from the temporary location
temp_cities_data <- read.csv(temp_cities)

##### File ID from Google Drive URL (For GlobalLandTemperaturesByCountry)
file_id_temp_country <- "1eUG98W8Mz6OURXim17SEYiFFtsz3oGpG"

# Download the file to a temporary location
temp_country <- tempfile(fileext = ".csv")
drive_download(as_id(file_id_temp_country), path = temp_country, overwrite = TRUE)


# Read the CSV file from the temporary location
temp_country_data <- read.csv(temp_country)

##### File ID from Google Drive URL (For world_population)
file_id_temp_pop <- "1sAMaGYknHeDDwdTICCsqgfWBXcmw5Wtl"

# Download the file to a temporary location
temp_pop <- tempfile(fileext = ".csv")
drive_download(as_id(file_id_temp_pop), path = temp_pop, overwrite = TRUE)

# Read the CSV file from the temporary location
population_data <- read.csv(temp_pop)


##### File ID from Google Drive URL (For energy)
file_id_temp_energy <- "1Oha-hiaJZCH9d4jJW-SN5GqcUJew9s1Z"

# Download the file to a temporary location
temp_energy <- tempfile(fileext = ".csv")
drive_download(as_id(file_id_temp_energy), path = temp_energy, overwrite = TRUE)

# Read the CSV file from the temporary location
energy_data <- read.csv(temp_energy)


##### File ID from Google Drive URL (For Share_of_green_areas_and_green_area_per_capita_in_cities_and_urban_areas_1990_-_2020)
file_id_temp_green <- "1Yu75p6z9dXVbfIe9n2rb9QU7YBntvsyz"

# Download the file to a temporary location with .xlsx extension
temp_green <- tempfile(fileext = ".xlsx")
drive_download(as_id(file_id_temp_green), path = temp_green, overwrite = TRUE)

# Read the Excel file from the temporary location
green_area_data <- read_excel(temp_green)


## Datasets merger
#test new shana

## Data cleaning
### Checking for NA / NAN / empty values
### Removing data that doesnt value add the analysis (if required)
### Removing data with special characters (if required)

# 2. DATA TRANSFORMATION
## Checking and modifying the data types of each columns
## Mathematical transformation on data columns (if required)

## Spitting of data (Train and test)


# 3. EXPLORATORY DATA ANALYSIS (EDA)


# 4. MODELLING ANALYSIS


#Testing >>>>>>>>>>>>>>>>>>>>>

#test something rp
#test another line
# to use KDE contour plots for temperature density etc.

