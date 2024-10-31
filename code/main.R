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
install.packages("lubridate")
install.packages("plotly")
install.packages("viridis")

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
library(lubridate)
library(plotly)
library(viridis)

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

#################################################################################################################
# Manually loading the dataset from my working directory
# GlobalLandTemperaturesByCity
temp_cities_data <- read.csv("GlobalLandTemperaturesByCity.csv")

# GlobalLandTemperaturesByCountry
temp_country_data <- read.csv("GlobalLandTemperaturesByCountry.csv")

# world_population
population_data <- read.csv("world_population.csv")

# energy
energy_data <- read.csv("energy.csv")

green_area_data <- read_excel("Share_of_green_areas_and_green_area_per_capita_in_cities_and_urban_areas_1990_-_2020.xlsx")

lakes_data <- st_read("HydroLAKES_polys_v10.shp")

#################################################################################################################

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
### Identifying overlapping dates across all cities to standardize the dataset and minimize data biasness
# Remove rows with NA values
cleaned_temp_data <- drop_na(joined_country_city_temp)
# Checking and removing duplicated values if there are any
print(paste("Total number of duplicated records: ", nrow(cleaned_temp_data[duplicated(cleaned_temp_data), ])))
unique_city_per_dt <- cleaned_temp_data %>% group_by(City) %>% summarise(nrows_per_dt = n_distinct(dt), .groups = 'drop')
# Plotting bar chart to visually check if the cities have differing dates (randomly sampling 50 records as there are too much datapoints to plot)
set.seed(1234)
limit_unique_city_per_dt <- unique_city_per_dt %>% sample_n(50)
ggplot(limit_unique_city_per_dt, aes(x = City, y = nrows_per_dt, fill = City)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Number of Unique Dates by Cites",  x = "City", y = "Number of Unique Dates") +   
  geom_text(aes(label = nrows_per_dt), position = position_stack(vjust = 1.05), angle = 90, size = 2.5,fontface = "bold") + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Finding overlapping dates
unique_dt_per_city <- cleaned_temp_data %>% group_by(dt) %>% summarise(nrows_per_city = n_distinct(City), .groups = 'drop')
distinct_cities <- n_distinct(cleaned_temp_data$City)
overlapping_dt <- unique_dt_per_city %>%  filter(nrows_per_city == distinct_cities) %>% pull(dt)
# Removing non-overlapping dates
cleaned_temp_data <- cleaned_temp_data %>% filter(dt %in% overlapping_dt)
unique_city_per_dt <- cleaned_temp_data %>% group_by(City) %>% summarise(nrows_per_dt = n_distinct(dt), .groups = 'drop')
set.seed(1234)
limit_unique_city_per_dt <- unique_city_per_dt %>% sample_n(50)
# Replotting to check if the cities still have differing dates
ggplot(limit_unique_city_per_dt, aes(x = City, y = nrows_per_dt, fill = City)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Number of Unique Dates by Cites",  x = "City", y = "Number of Unique Dates") +   
  geom_text(aes(label = nrows_per_dt), position = position_stack(vjust = 1.05), angle = 90, size = 2.5,fontface = "bold") + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 2. DATA TRANSFORMATION
## Checking and modifying the data types of each columns
## Mathematical transformation on data columns (Exponentially Weighted Average)

# Convert 'dt' to Date
cleaned_temp_data$dt <- as.Date(cleaned_temp_data$dt)
# Function to clean Latitude
#might need to wokr on this
clean_latitude <- function(lat) {
  lat <- trimws(lat)                      # Remove leading/trailing whitespace
  lat <- gsub("N$", "\\1", lat)               # Remove 'N' at the end
  lat <- gsub("S$", "-\\1", lat)              # Replace 'S' at the end and add a negative sign infront
  lat <- gsub("[^0-9.\\-]", "", lat)      # Remove any non-numeric characters except '-' and '.'
  as.numeric(lat)                         # Convert to numeric
}
# Function to clean Longitude
clean_longitude <- function(lon) {
  lon <- trimws(lon)                      # Remove leading/trailing whitespace
  lon <- gsub("E$", "\\1", lon)               # Remove 'E' at the end
  lon <- gsub("W$", "-\\1", lon)              # Replace 'W' at the end and add a negative sign infront
  lon <- gsub("[^0-9.\\-]", "", lon)      # Remove any non-numeric characters except '-' and '.'
  as.numeric(lon)                         # Convert to numeric
}
# Apply the cleaning functions and removing NA's due to coercion
cleaned_temp_data$Latitude <- clean_latitude(cleaned_temp_data$Latitude)
cleaned_temp_data$Longitude <- clean_longitude(cleaned_temp_data$Longitude)

# Function to calculate the weighted average temperature (placing greater emphasis on dates that are closer to the max date -> 1/(2^N))
weighted_avg_func <- function(avg_temperatures, date){
  ref_date <- max(cleaned_temp_data$dt)
  # Calculate the difference (in days) between date against max date
  # The return value will always be positive as the present date is always later than any dates in our data set
  day_diff <- as.numeric(difftime(ref_date, date, units = "days"))
  # Adjusting the weight to years, as using days is too restrictive
  day_diff <- day_diff/365
  w <- 1/(2^day_diff)
  weighted_temp <- round((avg_temperatures * w), 10)
  return(weighted_temp)
}
cleaned_temp_data$weighted_city_avg_temp <- weighted_avg_func(cleaned_temp_data$city_avg_temp, cleaned_temp_data$dt)

## Spitting of data (Train and test)


# 3. EXPLORATORY DATA ANALYSIS (EDA)

#########################################
# Histogram for Country Average Temperature
ggplot(cleaned_temp_data, aes(x = country_avg_temp)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Country Average Temperature", x = "Country Avg Temp", y = "Count")

# Histogram for City Average Temperature
ggplot(cleaned_temp_data, aes(x = city_avg_temp)) +
  geom_histogram(binwidth = 1, fill = "salmon", color = "black") +
  labs(title = "Distribution of City Average Temperature", x = "City Avg Temp", y = "Count")


# Boxplot for Country Average Temperature
ggplot(cleaned_temp_data, aes(y = country_avg_temp)) +
  geom_boxplot(fill = "lightyellow") +
  labs(title = "Boxplot of Country Average Temperature", y = "Country Avg Temp")

# Boxplot for City Average Temperature
ggplot(cleaned_temp_data, aes(y = city_avg_temp)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot of City Average Temperature", y = "City Avg Temp")


# Correlation matrix
cor_matrix <- cor(cleaned_temp_data %>% select(country_avg_temp, city_avg_temp, country_temp_uncertainty, city_temp_uncertainty), use = "complete.obs")

# Visualize the correlation matrix
library(corrplot)
corrplot(cor_matrix, method = "circle")

# Average temperature by Country
avg_temp_country <- cleaned_temp_data %>%
  group_by(Country) %>%
  summarise(mean_country_temp = mean(country_avg_temp, na.rm = TRUE)) %>%
  arrange(desc(mean_country_temp))

# View the top 10 countries
head(avg_temp_country, 10)

#monthly
avg_temp_monthly <- cleaned_temp_data %>%
  group_by(month = floor_date(dt, "month")) %>%
  summarise(mean_temp = mean(city_avg_temp, na.rm = TRUE))

# Plotting average temperature by month
ggplot(avg_temp_monthly, aes(x = month, y = mean_temp)) +
  geom_line(color = "blue") +
  labs(title = "Average City Temperature Over Time (Monthly)", x = "Month", y = "Average Temperature") +
  theme_minimal()


#yearly
avg_temp_yearly <- cleaned_temp_data %>%
  group_by(year = year(dt)) %>%
  summarise(mean_temp = mean(city_avg_temp, na.rm = TRUE))

# Plotting average temperature by year
ggplot(avg_temp_yearly, aes(x = year, y = mean_temp)) +
  geom_line(color = "blue") +
  labs(title = "Average City Temperature Over Time (Yearly)", x = "Year", y = "Average Temperature") +
  theme_minimal()


###Plotting world map with avg country temp

# Aggregate temperature data across all years for each country
overall_avg_temp <- cleaned_temp_data %>%
  group_by(Country) %>%
  summarise(avg_temp = mean(city_avg_temp, na.rm = TRUE))

# Join with world shapefile data
world_temp <- world %>%
  inner_join(overall_avg_temp, by = c("name" = "Country"))

# Plot the map
ggplot(data = world_temp) +
  geom_sf(aes(fill = avg_temp), color = "gray70") +
  scale_fill_viridis_c(option = "plasma", name = "Avg Temp (°C)", na.value = "lightgray") +
  labs(title = "Average Temperature by Country", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # Equator line
  coord_sf(ylim = c(-60, 90))  # Limit latitude to -60, 90 to avoid polar distortion


####################################################################################   

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


#Plotting average temp by country and city (1970 to 1980)

# Convert 'dt' columns to Date format
temp_country_data$dt <- as.Date(temp_country_data$dt)
temp_cities_data$dt <- as.Date(temp_cities_data$dt)

# Extract the year from the 'dt' column using lubridate
temp_country_data$year <- year(temp_country_data$dt)
temp_cities_data$year <- year(temp_cities_data$dt)

# Filter the data for years between 1970 and 1980
filtered_country_data <- temp_country_data %>%
  filter(year >= 1970 & year <= 1980)

filtered_city_data <- temp_cities_data %>%
  filter(year >= 1970 & year <= 1980)

# Aggregate the temperature by country (mean temperature for each country)
country_avg_temp <- filtered_country_data %>%
  group_by(Country) %>%
  summarise(avg_temp = mean(country_avg_temp, na.rm = TRUE))

# Aggregate the temperature by city (mean temperature for each city)
city_avg_temp <- filtered_city_data %>%
  group_by(City, Country, Latitude, Longitude) %>%
  summarise(avg_temp = mean(city_avg_temp, na.rm = TRUE))

# Plot the map with country temperatures
ggplot() +
  # Add the world map, filling by average country temperature
  geom_sf(data = world_map, aes(fill = avg_temp), color = "black") +
  scale_fill_viridis_c(option = "plasma", name = "Country Temp") +
  # Add points for city temperatures
  geom_point(data = city_avg_temp, aes(x = Longitude, y = Latitude, color = avg_temp), size = 2) +
  scale_color_viridis_c(option = "magma", name = "City Temp") +
  theme_minimal() +
  labs(title = "World Map with Country and City Temperatures",
       x = "Longitude",
       y = "Latitude") +
  coord_sf(lims_method = "geometry_bbox", default_crs = NULL)

# 4. MODELLING ANALYSIS


#Testing >>>>>>>>>>>>>>>>>>>>>

# to use KDE contour plots for temperature density etc.

