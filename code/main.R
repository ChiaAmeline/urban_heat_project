# Import libraries
install.packages("tidyverse", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("sf", dependencies = TRUE)
install.packages("tmap", dependencies = TRUE)
install.packages("measurements", dependencies = TRUE)
install.packages("spData",dependencies = TRUE)
install.packages("googledrive", dependencies = TRUE)
install.packages("readxl")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(sf)
library(tmap)
library(measurements)
library(spData)
library(googledrive)
library(readxl)
library(rnaturalearth)
library(rnaturalearthdata)

# DATA CONNECTION VIA GOOGLE DRIVE

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


##### File ID from Google Drive URL (For HydroLAKES_polys_v10_shp)
#to be added https://drive.google.com/file/d/1la5j_6CXWYZ4bHbaRMacMppdaV3ojNiX/view?usp=sharing

file_id_hydrolakes <- "1la5j_6CXWYZ4bHbaRMacMppdaV3ojNiX"

# Download the zip file to a temporary location
temp_zip_hydrolakes <- tempfile(fileext = ".zip")
drive_download(as_id(file_id_hydrolakes), path = temp_zip_hydrolakes, overwrite = TRUE)

# Unzip the file to a temporary directory
temp_dir_hydrolakes <- tempdir()
unzip(temp_zip_hydrolakes, exdir = temp_dir_hydrolakes)

# Select the HydroLAKES shapefile
shp_file <- "/var/folders/xp/0vgps1xn0lx45nm1jfnyvlrw0000gn/T//RtmpJHdXCV/HydroLAKES_polys_v10.shp"

# Read the shapefile
lakes_data <- st_read(shp_file)

##### File ID from Google Drive URL (For HydroRIVERS_v10_shp)
#to be added


# LOAD R DATASETS

#World map dataset with country polygons

world_map <- ne_countries(scale = "medium", returnclass = "sf")
View(world_map)

world_cities <- ne_download(scale = "medium", type = "populated_places", category = "cultural", returnclass = "sf")

# Extract city coordinates
cities_coords <- st_coordinates(world_cities)

# Add coordinates back to the world_cities data frame
world_cities <- cbind(world_cities, cities_coords)


## DATASETS JOIN - MAY NOT NEED TO JOIN

# Rename columns for clarity
temp_country_data <- temp_country_data %>%
  rename(
    country_avg_temp = AverageTemperature,
    country_temp_uncertainty = AverageTemperatureUncertainty
  )

temp_cities_data <- temp_cities_data %>%
  rename(
    city_avg_temp = AverageTemperature,
    city_temp_uncertainty = AverageTemperatureUncertainty
  )

# Join the datasets on 'dt' and 'Country'
joined_country_city_temp <- left_join(temp_country_data, temp_cities_data, by = c("dt", "Country"))

#May need to clean the joined_country_city_temp further, too many NAs. maybe can aggregate by monthly/yearly temp instead.





## Data cleaning
### Checking for NA / NAN / empty values
### Removing data that doesnt value add the analysis (if required)
### Removing data with special characters (if required)

# 2. DATA TRANSFORMATION
## Checking and modifying the data types of each columns
## Mathematical transformation on data columns (if required)

## Spitting of data (Train and test)


# 3. EXPLORATORY DATA ANALYSIS (EDA)

#TEST PLOT

# Plot world map and world_cities
ggplot() +
  # Plot country boundaries
  geom_sf(data = world_map, fill = "lightblue", color = "black") +
  # Plot city locations
  geom_point(data = world_cities, aes(x = X, y = Y), color = "red", size = 2) +
  # Add labels for world_cities (assuming the city name column is 'name' or something similar)
  geom_text(data = world_cities, aes(x = X, y = Y, label = NAME), 
            hjust = 0, vjust = 1, size = 2, color = "darkred") +
  # Set a minimal theme
  theme_minimal() +
  labs(title = "World Map with Cities",
       x = "Longitude",
       y = "Latitude")


# 4. MODELLING ANALYSIS


#Testing >>>>>>>>>>>>>>>>>>>>>

#test something rp
#test another line
# to use KDE contour plots for temperature density etc.

