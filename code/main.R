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
download_file_fun <- function(file_id) {
  # Download the file to the temporary location
  temp_file <- tempfile(fileext = ".csv")
  drive_download(as_id(file_id), path = temp_file, overwrite = TRUE)
  # Read the CSV file from the temporary location
  return(read.csv(temp_file))
}

##### File ID from Google Drive URL (For GlobalLandTemperaturesByCity)
temp_cities_data <- download_file_fun("17lPSwGwt5HTMbIiaTkotfmuKlkKZAPcM")
##### File ID from Google Drive URL (For GlobalLandTemperaturesByCountry)
temp_country_data <- download_file_fun("1eUG98W8Mz6OURXim17SEYiFFtsz3oGpG")
##### File ID from Google Drive URL (For world_population)
population_data <- download_file_fun("1sAMaGYknHeDDwdTICCsqgfWBXcmw5Wtl")
##### File ID from Google Drive URL (For energy)
energy_data <- download_file_fun("1Oha-hiaJZCH9d4jJW-SN5GqcUJew9s1Z")
##### File ID from Google Drive URL (For Share_of_green_areas_and_green_area_per_capita_in_cities_and_urban_areas_1990_-_2020)
green_area_data <- download_file_fun("1Yu75p6z9dXVbfIe9n2rb9QU7YBntvsyz")

##### File ID from Google Drive URL (For HydroLAKES_polys_v10_shp)
### Commented out cause not possible to unzip and open all files in the folder
# #to be added https://drive.google.com/file/d/1la5j_6CXWYZ4bHbaRMacMppdaV3ojNiX/view?usp=sharing
# 
# file_id_hydrolakes <- "1la5j_6CXWYZ4bHbaRMacMppdaV3ojNiX"
# 
# # Download the zip file to a temporary location
# temp_zip_hydrolakes <- tempfile(fileext = ".zip")
# drive_download(as_id(file_id_hydrolakes), path = temp_zip_hydrolakes, overwrite = TRUE)
# 
# # Unzip the file to a temporary directory
# temp_dir_hydrolakes <- tempdir()
# unzip(temp_zip_hydrolakes, exdir = temp_dir_hydrolakes)
# 
# # Select the HydroLAKES shapefile
# shp_file <- "/var/folders/xp/0vgps1xn0lx45nm1jfnyvlrw0000gn/T//RtmpJHdXCV/HydroLAKES_polys_v10.shp"
# 
# # Read the shapefile
# lakes_data <- st_read(shp_file)

# Google Drive File ID for the HydroLAKES zip file
file_id_hydrolakes <- "1la5j_6CXWYZ4bHbaRMacMppdaV3ojNiX"

# Step 1: Download the zip file to a temporary location
temp_zip_hydrolakes <- tempfile(fileext = ".zip")
drive_download(as_id(file_id_hydrolakes), path = temp_zip_hydrolakes, overwrite = TRUE)

# Step 2: Unzip the file to a temporary directory
temp_dir_hydrolakes <- tempdir()
unzip(temp_zip_hydrolakes, exdir = temp_dir_hydrolakes)

# Step 3: Locate the .shp file within the unzipped files
shp_file <- list.files(temp_dir_hydrolakes, pattern = "\\.shp$", full.names = TRUE)

# Ensure only one .shp file is found; if not, stop with an error
if (length(shp_file) != 1) {
  stop("Error: Multiple or no .shp files found in the directory.")
}

# Step 4: Load the shapefile
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
## Checking and modifying the data types of each columns
### Identifying overlapping dates across all cities to standardize the dataset and minimize data biasness
# Remove rows with NA values
cleaned_temp_data <- drop_na(joined_country_city_temp)
# Checking that lat is within -90 and 90 and log is within -18- and 180
lat_pat <- "^(-?([0-9]{1,2}(\\.\\d+)?))([NS])?$"
lon_pat <- "^(-?([0-9]{1,3}(\\.\\d+)?))([EW])?$"
cleaned_temp_data <- cleaned_temp_data %>%  filter(grepl(lat_pat, Latitude), grepl(lon_pat, Longitude))
# Checking and removing duplicated values if there are any
print(paste("Total number of duplicated records: ", nrow(cleaned_temp_data[duplicated(cleaned_temp_data), ])))
# Convert 'dt' to Date
cleaned_temp_data$dt <- as.Date(cleaned_temp_data$dt)
# Feature engineering a new column for data transformation
# Extract the year using format()
cleaned_temp_data$year <- format(cleaned_temp_data$dt, "%Y")
cleaned_temp_data$year <- as.numeric(cleaned_temp_data$year)
# Function to clean Latitude
clean_latitude <- function(lat) {
  lat <- trimws(lat)                      # Remove leading/trailing whitespace
  lat <- gsub("N$", "", lat)              # Remove 'N' at the end
  lat <- gsub("S$", "-", lat)             # Replace 'S' at the end and add a negative sign infront
  if (grepl("-$", lat)) {
    lat <- paste0("-", sub("-$", "", lat))
  }
  lat <- gsub("[^0-9.\\-]", "", lat)      # Remove any non-numeric characters except '-' and '.'
  lat <- as.numeric(lat)                  # Convert to numeric
  return(lat)
}
# Function to clean Longitude
clean_longitude <- function(lon) {
  lon <- trimws(lon)                      # Remove leading/trailing whitespace
  lon <- gsub("E$", "", lon)              # Remove 'E' at the end
  lon <- gsub("W$", "-", lon)             # Replace 'W' at the end and add a negative sign infront
  lon <- sub("^-", "-", lon)
  if (grepl("-$", lon)) {
    lon <- paste0("-", sub("-$", "", lon))
  }
  lon <- gsub("[^0-9.\\-]", "", lon)      # Remove any non-numeric characters except '-' and '.'
  lon <- as.numeric(lon)                  # Convert to numeric
  return(lon)
}
# Apply the cleaning functions and removing NA's due to coercion
cleaned_temp_data$Latitude <- sapply(cleaned_temp_data$Latitude, clean_latitude)
cleaned_temp_data$Longitude <- sapply(cleaned_temp_data$Longitude, clean_longitude)
# Plotting bar chart to visually check if the cities have differing dates (randomly sampling 100000 records as there are too much data points to plot)
choropleth_fun <- function(data){
  set.seed(1234)
  unique_city_per_dt <- data %>% group_by(City) %>% summarise(nrows_per_dt = n_distinct(dt), .groups = 'drop', Longitude = Longitude, Latitude = Latitude)
  limit_unique_city_per_dt <- unique_city_per_dt %>% sample_n(100000)
  ggplot(limit_unique_city_per_dt) + borders("world", colour = "gray80", fill = "gray90") +
    geom_point(aes(x = Longitude, y = Latitude, color = nrows_per_dt), alpha = 0.7,  size = 3) +
    scale_color_viridis_c() + coord_sf(datum = st_crs(4326), crs = "+proj=moll") + 
    labs(
      title = "Total Number of Unique Dates by City",
      x = "Longitude",
      y = "Latitude",
      color = "Count of Dates"
    ) +
    theme_minimal()
}
choropleth_fun(cleaned_temp_data)
# Finding overlapping dates
unique_dt_per_city <- cleaned_temp_data %>% group_by(dt) %>% summarise(nrows_per_city = n_distinct(City), .groups = 'drop')
distinct_cities <- n_distinct(cleaned_temp_data$City)
overlapping_dt <- unique_dt_per_city %>%  filter(nrows_per_city == distinct_cities) %>% pull(dt)
# Removing non-overlapping dates
cleaned_temp_data <- cleaned_temp_data %>% filter(dt %in% overlapping_dt)
unique_city_per_dt <- cleaned_temp_data %>% group_by(City) %>% summarise(nrows_per_dt = n_distinct(dt), .groups = 'drop')
# Replotting to check if the cities still have differing dates
choropleth_fun(cleaned_temp_data)

# 2. DATA TRANSFORMATION
## Mathematical transformation on data columns (Exponentially Weighted Average)
# Function to calculate the weighted average temperature (placing greater emphasis on dates that are closer to present date -> 1/(2^index))
unique_years <- unique(cleaned_temp_data$year)
unique_years <- sort(unique_years, decreasing = TRUE)

weighted_avg_func <- function(avg_temperatures, date){
  year <- format(date, "%Y")
  year <- as.numeric(year)
  index <- match(year, unique_years)
  w <- 1/(2^index)
  weighted_temp <- round((avg_temperatures * w), 10)
  return(weighted_temp)
}
cleaned_temp_data$weighted_city_avg_temp <- mapply(weighted_avg_func, avg_temperatures = cleaned_temp_data$city_avg_temp, date = cleaned_temp_data$dt)

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
world_temp <- world_map %>%
  inner_join(overall_avg_temp, by = c("name" = "Country"))

# Plot the map
ggplot(data = world_temp) +
  geom_sf(aes(fill = avg_temp), color = "gray70") +
  scale_fill_viridis_c(option = "plasma", name = "Avg Temp (?C)", na.value = "lightgray") +
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

####################################################################
# Average temp of specific year (2010)
overall_avg_temp_2010 <- cleaned_temp_data %>%
  filter(year == 2010) %>%
  group_by(Country) %>%
  summarise(avg_temp = mean(city_avg_temp, na.rm = TRUE))

# Join with world shapefile data
world_temp_2010 <- world_map %>%
  inner_join(overall_avg_temp_2010, by = c("name" = "Country"))

# Create a center point for the countries to display on map
world_temp_centerpoint <- world_temp %>%
  st_centroid() %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  )

# Add 2010 PoP to the countries centerpoints with temperature data to find relationship
world_temp_and_pop_2010 <- world_temp_centerpoint %>%
  left_join(population_data %>% select('Country.Territory', 'X2010.Population'), by = c("name" = "Country.Territory"))


# Plot the map
ggplot() +
  geom_sf(data = world_temp_2010, aes(fill = avg_temp), color = "gray70") +
  geom_point(data = world_temp_and_pop_2010, aes(x = x, y = y, size = X2010.Population), color = "cyan", alpha = 0.8) +
  scale_fill_viridis_c(option = "plasma", name = "Avg Temp (?C)", na.value = "lightgray") +
  scale_size(range = c(0.5, 5), name = "Population") +
  labs(title = "Average Temperature by Country with Population in 2010", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # Equator line
  coord_sf(ylim = c(-60, 90))

# Scatter plot to see if there is relationship between pop and temp, NO CORRELATION OBSERVED
ggplot(data = world_temp_and_pop_2010, aes(x = X2010.Population, y = avg_temp)) +
  geom_point(color = "cyan4", alpha = 0.6) +
  labs(title = "Relationship Between Population and Average Temperature 2010",
       x = "Population",
       y = "Average Temperature") 

#++++++++++++++++++++++++++++++++++++++++++++++++

# Average temp of specific year 1970
overall_avg_temp_1970 <- cleaned_temp_data %>%
  filter(year == 1970) %>%
  group_by(Country) %>%
  summarise(avg_temp = mean(city_avg_temp, na.rm = TRUE))

# Join with world shapefile data
world_temp_1970 <- world_map %>%
  inner_join(overall_avg_temp_1970, by = c("name" = "Country"))

# Add 1970 PoP to the countries centerpoints with temperature data to find relationship
world_temp_and_pop_1970 <- world_temp_centerpoint %>%
  left_join(population_data %>% select('Country.Territory', 'X1970.Population'), by = c("name" = "Country.Territory"))

# Plot the map
ggplot() +
  geom_sf(data = world_temp_1970, aes(fill = avg_temp), color = "gray70") +
  geom_point(data = world_temp_and_pop_1970, aes(x = x, y = y, size = X1970.Population), color = "cyan", alpha = 0.8) +
  scale_fill_viridis_c(option = "plasma", name = "Avg Temp (?C)", na.value = "lightgray") +
  scale_size(range = c(0.5, 5), name = "Population") +
  labs(title = "Average Temperature by Country with Population in 1970", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # Equator line
  coord_sf(ylim = c(-60, 90))

# Scatter plot to see if there is relationship between pop and temp, NO CORRELATION OBSERVED
ggplot(data = world_temp_and_pop_1970, aes(x = X1970.Population, y = avg_temp)) +
  geom_point(color = "cyan4", alpha = 0.6) +
  labs(title = "Relationship Between Population and Average Temperature",
       x = "Population",
       y = "Average Temperature")

##########################################################
#Find green area by country
country_green_area_2010 <- green_area_data %>%
  mutate(`Average share of green area in city/ urban area 2010 (%)` = as.numeric(`Average share of green area in city/ urban area 2010 (%)`)) %>%
  group_by(`Country or Territory Name`) %>%
  summarise(avg_green_area = mean(`Average share of green area in city/ urban area 2010 (%)`, na.rm = TRUE))

# Add 2010 green to the countries centerpoints with temperature data to find relationship
world_temp_and_green_2010 <- world_temp_centerpoint %>%
  left_join(country_green_area_2010 %>% select(`Country or Territory Name`, `avg_green_area`), by = c("name" = "Country or Territory Name"))


# Plot the map
ggplot() +
  geom_sf(data = world_temp_2010, aes(fill = avg_temp), color = "gray70") +
  geom_point(data = world_temp_and_green_2010, aes(x = x, y = y, size = avg_green_area), color = "green4", alpha = 0.8) +
  scale_fill_viridis_c(option = "plasma", name = "Avg Temp (?C)", na.value = "lightgray") +
  scale_size(range = c(0.5, 5), name = "Greens") +
  labs(title = "Average Temperature by Country with Greens in 2010", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # Equator line
  coord_sf(ylim = c(-60, 90))

# Scatter plot to see if there is relationship between green and temp, HAVE SOME RELATIONSHIP TRENDS BUT NOT STRONG
ggplot(data = world_temp_and_green_2010, aes(x = avg_green_area, y = avg_temp)) +
  geom_point(color = "cyan4", alpha = 0.6) +
  labs(title = "Relationship Between Greens and Average Temperature 2010",
       x = "Greens",
       y = "Average Temperature") 



# 5. SPATIAL ANALYTICS



# to use KDE contour plots for temperature density etc.

