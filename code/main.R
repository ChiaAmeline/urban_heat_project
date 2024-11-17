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
install.packages("scales")
install.packages("readr")
install.packages("rlang")
install.packages("corrplot")
install.packages("spatstat")
install.packages("eks")
install.packages("ks")    
install.packages("tmap")
install.packages("spdep")
install.packages("spatialreg")
install.packages("GWmodel")
install.packages("patchwork")




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
library(scales)
library(readr)
library(rlang)
library(corrplot)
library(spatstat)
library(eks)
library(ks)
library(tmap)
library(spdep)
library(spatialreg)
library(GWmodel)
library(patchwork)



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
# file_id_hydrolakes <- "1la5j_6CXWYZ4bHbaRMacMppdaV3ojNiX"
# 
# # Step 1: Download the zip file to a temporary location
# temp_zip_hydrolakes <- tempfile(fileext = ".zip")
# drive_download(as_id(file_id_hydrolakes), path = temp_zip_hydrolakes, overwrite = TRUE)
# 
# # Step 2: Unzip the file to a temporary directory
# temp_dir_hydrolakes <- tempdir()
# unzip(temp_zip_hydrolakes, exdir = temp_dir_hydrolakes)
# 
# # Step 3: Locate the .shp file within the unzipped files
# shp_file <- list.files(temp_dir_hydrolakes, pattern = "\\.shp$", full.names = TRUE)
# 
# # Ensure only one .shp file is found; if not, stop with an error
# if (length(shp_file) != 1) {
#   stop("Error: Multiple or no .shp files found in the directory.")
# }
# 
# # Step 4: Load the shapefile
# lakes_data <- st_read(shp_file)

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
### Transpose population data
population_data <- population_data %>%
  pivot_longer(
    cols = ends_with("Population"),          
    names_to = "Year",                       
    names_prefix = "",                       
    values_to = "Population"                
  ) %>% mutate( Year = as.integer(sub(" Population", "", Year)))

# Remove rows with NA values
cleaned_temp_data <- joined_country_city_temp %>% drop_na()
population_data <- population_data %>% drop_na()
energy_data <- energy_data %>% drop_na()
green_area_data <- green_area_data %>% drop_na()
# Checking that lat is within -90 and 90 and log is within -18- and 180
lat_pat <- "^(-?([0-9]{1,2}(\\.\\d+)?))([NS])?$"
lon_pat <- "^(-?([0-9]{1,3}(\\.\\d+)?))([EW])?$"
cleaned_temp_data <- cleaned_temp_data %>% filter(grepl(lat_pat, Latitude), grepl(lon_pat, Longitude))
# Checking and removing duplicated values if there are any
print(paste("Total number of duplicated records: ", nrow(cleaned_temp_data[duplicated(cleaned_temp_data), ])))
# Convert 'dt' to Date
cleaned_temp_data$dt <- as.Date(cleaned_temp_data$dt)
# Feature engineering a new column for data transformation
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
# Apply the cleaning functions
cleaned_temp_data$Latitude <- sapply(cleaned_temp_data$Latitude, clean_latitude)
cleaned_temp_data$Longitude <- sapply(cleaned_temp_data$Longitude, clean_longitude)
# Plotting bar chart to visually check if the cities have differing dates (randomly sampling 100000 records as there are too much data points to plot)
unique_dt_map_fun <- function(data){
  set.seed(1234)
  unique_city_per_dt <- data %>% group_by(City) %>% reframe(nrows_per_dt = n_distinct(dt), Longitude = Longitude, Latitude = Latitude)
  limit_unique_city_per_dt <- unique_city_per_dt %>% sample_n(100000)
  ggplot(limit_unique_city_per_dt) + borders("world", colour = "gray80", fill = "gray90") +
    geom_point(aes(x = Longitude, y = Latitude, color = nrows_per_dt), alpha = 0.7, size = 2) +
    scale_color_viridis_c() + coord_sf(datum = st_crs(4326), crs = "+proj=moll") + 
    labs(
      title = "Total Number of Unique Dates by City",
      x = "Longitude",
      y = "Latitude",
      color = "Count of Dates"
    ) +
    theme_minimal()
}
unique_dt_map_fun(cleaned_temp_data)
# Finding overlapping dates
unique_dt_per_city <- cleaned_temp_data %>% group_by(dt) %>% summarise(nrows_per_city = n_distinct(City), .groups = 'drop')
distinct_cities <- n_distinct(cleaned_temp_data$City)
overlapping_dt <- unique_dt_per_city %>%filter(nrows_per_city == distinct_cities) %>% pull(dt)
# Removing non-overlapping dates
cleaned_temp_data <- cleaned_temp_data %>% filter(dt %in% overlapping_dt)
# Replotting to check if the cities still have differing dates
unique_dt_map_fun(cleaned_temp_data)

#################################################################################################################

# 2. DATA TRANSFORMATION
## Mathematical transformation on data columns (Exponentially Weighted Average)
# Function to calculate the weighted average temperature (placing greater emphasis on dates that are closer to present date -> 1/(2^index))
unique_years <- unique(cleaned_temp_data$year)
unique_years <- sort(unique_years, decreasing = TRUE)
cleaned_temp_data$decade <- floor(cleaned_temp_data$year / 10) * 10
avg_temp_across_decade <- cleaned_temp_data %>% group_by(decade, City, Longitude, Latitude) %>% reframe(avg_temp = mean(city_avg_temp, na.rm = TRUE))

ggplot(avg_temp_across_decade) +
  borders("world", colour = "gray80", fill = "gray90") +
  geom_point(aes(x = Longitude, y = Latitude, color = avg_temp), size = 1, alpha = 0.7) +
  scale_color_viridis_c(name = "Avg Temp (ï¿½C)", limits = c(10, 25), oob = squish) + 
  labs(
    title = "Average Temperature by City Across Decades",
    x = "Longitude",
    y = "Latitude"
  ) +
  facet_wrap(~ decade, ncol = 5) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )

weighted_avg_func <- function(avg_temperatures, year){
  index <- match(year, unique_years)
  w <- 1/(2^index)
  weighted_temp <- (avg_temperatures * w)
  return(weighted_temp)
}
cleaned_temp_data$weighted_city_avg_temp <- mapply(weighted_avg_func, avg_temperatures = cleaned_temp_data$city_avg_temp, year = cleaned_temp_data$year)

# Extract the year from the date column
cleaned_temp_data$year <- format(cleaned_temp_data$dt, "%Y")

# Group by City and Year to calculate yearly weighted temperatures for each city. needed for spatial analysis.
yearly_weighted_temp <- cleaned_temp_data %>%
  group_by(City, year, Longitude, Latitude) %>%
  summarise(yearly_weighted_temp = sum(weighted_city_avg_temp, na.rm = TRUE), .groups = 'drop')

#################################################################################################################

# 3. EXPLORATORY DATA ANALYSIS (EDA)
## Plotting population throughout the decades
avail_pop_years <- population_data %>% group_by(Year) %>% summarise(n = n())
filtered_cleaned_temp_with_population_years <- cleaned_temp_data %>% filter(year %in% avail_pop_years$Year)
joined_city_pop_df <- inner_join(filtered_cleaned_temp_with_population_years, population_data, by = c("year" = "Year", "Country" = "Country/Territory"))
select_col_of_joined_city_pop_df <- joined_city_pop_df %>% select(Country,Population, decade) %>% group_by(Country,Population, decade) %>% summarise(n=n(), .groups = 'drop')
joined_city_pop_geo_df <- world_map %>% inner_join(select_col_of_joined_city_pop_df, by = c("name" = "Country"))

ggplot(joined_city_pop_geo_df) +
  geom_sf(aes(fill = Population), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    name = "Population",
    option = "C",
    na.value = "grey90",
    limits = c(0, max(joined_city_pop_geo_df$Population, na.rm = TRUE)),
    oob = squish
  ) +
  facet_wrap(~ decade, ncol = 2) +
  labs(
    title = "Population by Country Across Decades",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )

## Plotting energy throughout the decades
# avail_energy_years <- energy_data %>% group_by(Year, Country) %>% summarise(Energy_consumption = sum(Energy_consumption, na.rm = TRUE), .groups = "drop")
# filtered_cleaned_temp_with_energy_years <- cleaned_temp_data %>% filter(year %in% avail_energy_years$Year)
# joined_city_energy_df <- inner_join(filtered_cleaned_temp_with_energy_years, energy_data,by = c("year" = "Year", "Country" = "Country"))
# select_col_of_joined_city_energy_df <- joined_city_energy_df %>% select(Country, Energy_consumption, decade)
# min_world_map <- world_map %>% select(name, geometry)
# joined_city_energy_geo_df <- min_world_map %>% inner_join(select_col_of_joined_city_energy_df, by = c("name" = "Country"))
# joined_city_energy_geo_df <-  joined_city_energy_geo_df %>% distinct()
# 
# ggplot(joined_city_energy_geo_df) +
#   geom_sf(aes(fill = Energy_consumption), color = "white", size = 0.1) +
#   scale_fill_viridis_c(
#     name = "Energy Consumption",
#     option = "C",
#     na.value = "grey90",
#     limits = c(0, max(joined_city_energy_geo_df$Energy_consumption, na.rm = TRUE)),
#     oob = squish
#   ) +
#   facet_wrap(~ decade, ncol = 5) +
#   labs(
#     title = "Energy Consumption by Country Across Decades",
#     x = "Longitude",
#     y = "Latitude"
#   ) +
#   theme_minimal() +
#   theme(
#     strip.text = element_text(size = 8),
#     plot.title = element_text(hjust = 0.5),
#     legend.position = "right"
#   )


# Histogram for Country Average Temperature
ggplot(cleaned_temp_data, aes(x = country_avg_temp)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black") +
  labs(title = "Distribution of Country Average Temperature", x = "Country Avg Temp", y = "Count")

# Histogram for City Average Temperature
#ggplot(cleaned_temp_data, aes(x = city_avg_temp)) +
#  geom_histogram(binwidth = 1, fill = "salmon", color = "black") +
#  labs(title = "Distribution of City Average Temperature", x = "City Avg Temp", y = "Count")


# Boxplot for Country Average Temperature
#ggplot(cleaned_temp_data, aes(y = country_avg_temp)) +
#  geom_boxplot(fill = "lightyellow") +
#  labs(title = "Boxplot of Country Average Temperature", y = "Country Avg Temp")

# Boxplot for City Average Temperature
#ggplot(cleaned_temp_data, aes(y = city_avg_temp)) +
#  geom_boxplot(fill = "lightgreen") +
#  labs(title = "Boxplot of City Average Temperature", y = "City Avg Temp")



# Average temperature by Country
avg_temp_country <- cleaned_temp_data %>%
  group_by(Country) %>%
  summarise(mean_country_temp = mean(country_avg_temp, na.rm = TRUE)) %>%
  arrange(desc(mean_country_temp))


# Calculate the top 10 countries with the highest average temperature
top_10_countries <- avg_temp_country %>%
  top_n(10, mean_country_temp) %>%
  arrange(desc(mean_country_temp))

# Plot the top 10 countries with a color gradient based on temperature
ggplot(top_10_countries, aes(x = reorder(Country, -mean_country_temp), y = mean_country_temp, fill = mean_country_temp)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "yellow", high = "red", name = "Avg Temperature (°C)") +
  labs(title = "Top 10 Countries with Highest Average Temperature",
       x = "Country", y = "Average Temperature (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Plotting top 10 countries with the highest average temperature on map

# Mark countries as 'Top 10' or 'Others' and assign temperatures only for top 10
world_temp <- world_map %>%
  left_join(avg_temp_country, by = c("name" = "Country")) %>%
  mutate(
    fill_color = ifelse(name %in% top_10_countries$Country, mean_country_temp, NA) # Top 10 countries get temperature, others NA
  )

# Plot the map
ggplot(data = world_temp) +
  geom_sf(aes(fill = fill_color), color = "gray70", size = 0.2) + # Single fill aesthetic
  scale_fill_gradient(
    low = "yellow", high = "red",
    na.value = "lightgray",  # Gray for non-top-10 countries
    name = "Avg Temp (°C)"
  ) +
  labs(
    title = "Top 10 Hottest Countries by Average Temperature",
    subtitle = "Highlighted with a yellow-to-red gradient, others in gray",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  )

#Time series plot (monthly)
#avg_temp_monthly <- cleaned_temp_data %>%
#  group_by(month = floor_date(dt, "month")) %>%
#  summarise(mean_temp = mean(city_avg_temp, na.rm = TRUE))

# Plotting average temperature by month
#ggplot(avg_temp_monthly, aes(x = month, y = mean_temp)) +
#  geom_line(color = "blue") +
#  labs(title = "Average City Temperature Over Time (Monthly)", x = "Month", y = "Average Temperature") +
#  theme_minimal()


#Time series plot (yealry)
avg_temp_yearly <- cleaned_temp_data %>%
  group_by(year = year(dt)) %>%
  summarise(mean_temp = mean(city_avg_temp, na.rm = TRUE))

# Plotting 
ggplot(avg_temp_yearly, aes(x = year, y = mean_temp)) +
  geom_line(color = "blue") +
  labs(title = "Average City Temperature Over Time (Yearly)", x = "Year", y = "Average Temperature") +
  theme_minimal()


###Plotting world map with avg city and country temp
# Aggregate temperature data across all years for each country
overall_avg_temp <- cleaned_temp_data %>%
  group_by(Country) %>%
  summarise(avg_temp = mean(city_avg_temp, na.rm = TRUE))

# Prepare city data
city_points <- cleaned_temp_data %>%
  filter(!is.na(city_avg_temp)) %>%
  group_by(City, Latitude, Longitude) %>%
  summarise(city_avg_temp = mean(city_avg_temp, na.rm = TRUE))

# Join with world shapefile data
world_temp <- world_map %>%
  inner_join(overall_avg_temp, by = c("name" = "Country"))

# Plot the map
ggplot() +
  # Country temperature layer
  geom_sf(data = world_temp, aes(fill = avg_temp), color = "gray70") +
  scale_fill_viridis_c(option = "plasma", name = "Country Avg Temp (°C)", na.value = "lightgray") +
  # City temperature points
  geom_point(data = city_points, aes(x = as.numeric(Longitude), y = as.numeric(Latitude), color = city_avg_temp), size = 2, alpha = 0.8) +
  # Add border around city points for better visualization
  geom_point(data = city_points, aes(x = as.numeric(Longitude), y = as.numeric(Latitude)), size = 2.5, shape = 1, color = "black") +
  scale_color_viridis_c(option = "magma", name = "City Avg Temp (°C)") +
  # Add title, labels, and equator line
  labs(
    title = "Average Temperatures of Countries and Cities",
    subtitle = "Country temperatures represented by fill; city temperatures as points",
    x = "Longitude",
    y = "Latitude"
  ) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # Equator line
  theme_minimal() +
  coord_sf(ylim = c(-60, 90)) +  # Limit latitude to avoid polar distortion
  theme(
    legend.position = "bottom",  # Place legends at the bottom
    legend.box = "vertical"
  ) +
  guides(
    fill = guide_colorbar(title.position = "top", barwidth = 10, barheight = 0.5),  # Country temperature legend
    color = guide_colorbar(title.position = "top", barwidth = 10, barheight = 0.5)   # City temperature legend
  )

#################################################################################################################  

# 4. MODELLING ANALYSIS

# Transpose green area data
green_area_data <- green_area_data %>%
  pivot_longer(
    cols = starts_with("Average share of green area in") |
      starts_with("Green area per capita"),
    names_to = "Year_Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Year = as.integer(str_extract(Year_Metric, "\\d{4}")), 
    Metric = case_when(
      str_detect(Year_Metric, "Average share of green area") ~ "Average_share_of_green_area",
      str_detect(Year_Metric, "Green area per capita") ~ "Green_area_per_capita"
    )
  ) %>%
  select(-Year_Metric) %>%
  pivot_wider(names_from = "Metric", values_from = "Value")

# Convert cleaned_temp_data year column from char to num datatype
cleaned_temp_data$year <- as.integer(cleaned_temp_data$year)

### Join Pop, energy, green area data to Temp data
# 1. Filter out countries with year and average temp
cleaned_temp_data_with_popenergygreen <- cleaned_temp_data %>%
  group_by(Country, year) %>%
  summarise(avg_temp = mean(city_avg_temp, na.rm = TRUE))

# 2. Left join the pop data
cleaned_temp_data_with_popenergygreen <- cleaned_temp_data_with_popenergygreen %>%
  left_join(population_data %>% select(`Country/Territory`, `Year`, `Density (per kmÂ²)`, `Population`), by = c("Country" = "Country/Territory", "year" = "Year"))

# 3. Find average energy consumption by country and year then left join to temp data
energy_data_avg_byyear <- energy_data %>%
  group_by(Country, Year) %>%
  summarise(Average_Energy_Consumption = mean(Energy_consumption, na.rm = TRUE))

cleaned_temp_data_with_popenergygreen <- cleaned_temp_data_with_popenergygreen %>%
  left_join(energy_data_avg_byyear, by =  c("Country" = "Country", "year" = "Year"))

# 4. Find average green area by country then left join the green area data
green_area_data_avg_byyear <- green_area_data %>%
  group_by(`Country or Territory Name`, `Year`) %>%
  summarise(Average_share_of_green_area = mean(Average_share_of_green_area, na.rm = TRUE),
            Green_area_per_capita = mean(Green_area_per_capita, na.rm = TRUE))

cleaned_temp_data_with_popenergygreen <- cleaned_temp_data_with_popenergygreen %>%
  left_join(green_area_data_avg_byyear, by = c("Country" = "Country or Territory Name", "year" = "Year"))

# See the first few rows
head(cleaned_temp_data_with_popenergygreen)

# Remove NA rows and ungroup 
cleaned_temp_data_with_popenergygreen <- cleaned_temp_data_with_popenergygreen %>%
  na.omit() %>%
  ungroup()

# Extract columns for correlation and calculate
cor_with_temp <- cleaned_temp_data_with_popenergygreen %>%
  select(avg_temp, Population, Average_Energy_Consumption, Average_share_of_green_area)

cor_matrix_for_temp <- cor(cor_with_temp, use = "complete.obs")
corrplot(cor_matrix_for_temp, method = "circle")

# Regression model
regression_model <- lm(avg_temp ~ Population + Average_Energy_Consumption + Average_share_of_green_area, 
                       data = cleaned_temp_data_with_popenergygreen)
summary(regression_model)

# Set seed for prediction test
set.seed(1234)

# Randomly sample 3 rows from Cleaned_temp_data to test predict
random_data_for_predict <- cleaned_temp_data_with_popenergygreen %>% sample_n(5)

# Try predict with regression model
regression_predictions <- predict(regression_model, newdata = random_data_for_predict)
regression_predictions

# Polynomial Regression
poly_model <- lm(avg_temp ~ poly(Population, 2) + poly(Average_Energy_Consumption, 2) + poly(Average_share_of_green_area, 2), data = cleaned_temp_data_with_popenergygreen)
summary(poly_model)

# Try predict with polynomial model
poly_predictions <- predict(poly_model, newdata = random_data_for_predict)
poly_predictions

# Join prediction into random data for view
random_data_for_predict <- random_data_for_predict %>%
  mutate(Linear_regression_predictions = regression_predictions, Polynomial_regression_predictions = poly_predictions) 

view(random_data_for_predict)

# Try Random Forest
library(randomForest)

# Split data for random forest 
train_indices <- sample(1:nrow(cleaned_temp_data_with_popenergygreen), 0.8 * nrow(cleaned_temp_data_with_popenergygreen))
train_data <- cleaned_temp_data_with_popenergygreen[train_indices, ]
test_data <- cleaned_temp_data_with_popenergygreen[-train_indices, ]

# Random Forest model
rf_model <- randomForest(avg_temp ~ Population + Average_Energy_Consumption + Average_share_of_green_area, data = train_data)

# Predict using random forest
test_predictions <- predict(rf_model, newdata = test_data)
rmse <- sqrt(mean((test_predictions - test_data$avg_temp)^2))
print(rmse)
rsq <- 1 - sum((test_predictions - test_data$avg_temp)^2) / sum((test_data$avg_temp - mean(test_data$avg_temp))^2)
print(rsq)

importance(rf_model)

# Plot random forest the importance of each predictors
importance_values <- importance(rf_model)
importance_df <- data.frame(Feature = rownames(importance_values), Importance = importance_values[, 1])
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  geom_text(aes(label = round(importance_values, 2)), hjust = 1.2, color = "black", size = 3) +
  labs(title = "Importance of predictors in RF Model",
       x = "Predictors",
       y = "Importance Value") 

# Plot to see how many points' predictions are close to actual temp
ggplot(data.frame(Predicted = test_predictions, Actual = test_data$avg_temp), aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs. Actual Temperature",
       x = "Actual Temperature",
       y = "Predicted Temperature")

#################################################################################################################

# 5. SPATIAL ANALYTICS

# Plot temperature points directly on world map
ggplot() +
  # Plot the world map polygons as background
  geom_sf(data = world_map_df, fill = "grey80", color = "white") +
  # Plot the points with color indicating temperature
  geom_point(data = temp_points_2010s, aes(x = Longitude, y = Latitude, color = city_avg_temp), 
             size = 1, alpha = 0.8) +
  scale_color_viridis_c(option = "magma", name = "City Avg Temp (ï¿½C)") +
  theme_minimal() +
  labs(title = "City Temperature Across the World (2010s)",
       x = "Longitude",
       y = "Latitude")

##Point Pattern Analysis

# Filter data for the 2010s only
temp_data_2010s <- cleaned_temp_data %>%
  filter(year >= 2010 & year <= 2019) %>%
  dplyr::select(Longitude, Latitude, city_avg_temp)

# Convert to sf object for spatial processing
temp_points_sf <- st_as_sf(temp_data_2010s, coords = c("Longitude", "Latitude"), crs = 4326)

# Extract Longitude and Latitude from geometry and filter based on their ranges
temp_points_sf <- temp_points_sf %>%
  mutate(Longitude = st_coordinates(geometry)[, 1],
         Latitude = st_coordinates(geometry)[, 2]) %>%
  filter(Latitude >= -90, Latitude <= 90, Longitude >= -180, Longitude <= 180)

# Extract coordinates from temp_points_sf
coords <- st_coordinates(temp_points_sf)

# Compute the bandwidth matrix
H <- Hpi(coords)

# Perform KDE using the specified bandwidth matrix
temp_kde <- st_kde(temp_points_sf, H = H)

# Extract contour lines at specified levels
contours <- st_get_contour(temp_kde, cont = c(10, 25, 50, 75, 95))

# Ensure contlabel is a factor for discrete coloring
contours$contlabel <- as.factor(contours$contlabel)

# Plotting with ggplot2
ggplot() +
  # World map as background
  geom_sf(data = world_map, fill = "grey80", color = "white") +
  # Temperature points
  geom_sf(data = temp_points_sf, aes(color = city_avg_temp), size = 1, alpha = 0.7) +
  # Density contours
  geom_sf(data = contours, aes(fill = contlabel), color = "black", linetype = "solid") +
  # Color scales
  scale_color_viridis_c(name = "City Avg Temp (ï¿½C)") +
  scale_fill_viridis_d(name = "Density Contours") +
  # Labels and theme
  labs(title = "City Temperature Density and Contour Analysis (2010s)", 
       x = "Longitude", y = "Latitude") +
  theme_minimal()

####

# tmap version

# Filter data for the 2010s only
temp_data_2010s <- cleaned_temp_data %>%
  filter(year >= 2010 & year <= 2019) %>%
  dplyr::select(Longitude, Latitude, city_avg_temp)

# Remove rows with NA values in Longitude, Latitude, or city_avg_temp
temp_data_2010s <- temp_data_2010s %>%
  filter(!is.na(Longitude), !is.na(Latitude), !is.na(city_avg_temp))

# Convert to sf object for spatial processing
temp_points_sf <- st_as_sf(temp_data_2010s, coords = c("Longitude", "Latitude"), crs = 4326)

# Extract Longitude and Latitude from geometry and filter based on their ranges
temp_points_sf <- temp_points_sf %>%
  mutate(Longitude = st_coordinates(geometry)[, 1],
         Latitude = st_coordinates(geometry)[, 2]) %>%
  filter(Latitude >= -90, Latitude <= 90, Longitude >= -180, Longitude <= 180)

# Compute KDE (assuming `temp_kde` has been calculated separately as an sf object with contours)
contours <- st_get_contour(temp_kde, cont = c(10, 25, 50, 75, 95))
contours$contlabel <- as.factor(contours$contlabel)

# Set tmap mode to plot
tmap_mode("plot")

# Create the tmap plot
tm_shape(world_map) +
  tm_polygons(col = "grey80", border.col = "white") +
  tm_shape(temp_points_sf) +
  tm_dots(col = "city_avg_temp", palette = "viridis", size = 0.5, alpha = 0.7, title = "City Avg Temp (ï¿½C)") +
  tm_shape(contours) +
  tm_polygons(col = "contlabel", palette = "viridis", title = "Density Contours", alpha = 0.5) +
  tm_layout(title = "City Temperature Density and Contour Analysis (2010s)",
            legend.outside = TRUE,
            legend.outside.position = "right",
            frame = FALSE)


### Second Order Point Pattern Analysis


# Convert temperature points to a spatial weights object for Moran's I
coords <- st_coordinates(temp_points_sf)

# Convert to spatstat ppp object
# Define the study window (bounding box) based on your dataset extent
bbox <- st_bbox(temp_points_sf)
temp_ppp <- ppp(
  x = st_coordinates(temp_points_sf)[, 1],
  y = st_coordinates(temp_points_sf)[, 2],
  window = owin(xrange = c(bbox["xmin"], bbox["xmax"]), yrange = c(bbox["ymin"], bbox["ymax"]))
)

# Calculate Ripley's K-function
K_result <- Kest(temp_ppp, correction = "Ripley")

# Plot Ripley's K-function
plot(K_result, main = "Ripley's K-function for Temperature Points",
     xlab = "Distance", ylab = "K(d)")

# Calculate and plot the L-function
L_result <- Lest(temp_ppp, correction = "Ripley")
plot(L_result, main = "L-function for Temperature Points",
     xlab = "Distance", ylab = "L(d) - d")


## Dont run, takes too long, result quite similar to previous

# Generate envelopes for the K-function with simulations under CSR
# K_envelope <- envelope(temp_ppp, Kest, nsim = 999, correction = "Ripley")
# 
# # Plot with envelopes
# plot(K_envelope, main = "Ripley's K-function with Envelope Analysis",
#      xlab = "Distance", ylab = "K(d)")
# 
# # Generate envelopes for the L-function with simulations under CSR
# L_envelope <- envelope(temp_ppp, Lest, nsim = 999, correction = "Ripley")
# 
# # Plot with envelopes
# plot(L_envelope, main = "L-function with Envelope Analysis",
#      xlab = "Distance", ylab = "L(d) - d")

# Perform the MAD test for Ripleyï¿½s K-function
mad_K <- mad.test(temp_ppp, Kest)
print(mad_K)

# Perform the MAD test for the L-function
mad_L <- mad.test(temp_ppp, Lest)
print(mad_L)

### Spatial Attribute Analysis & Spatial Autocorrelation Analysis

#Filter cleaned_temp_data to include only rows from 2010 to 2019
cleaned_temp_data_2010s <- cleaned_temp_data %>%
  filter(year >= 2010 & year <= 2019)

#Rename and select relevant columns in population_data
population_data <- population_data %>%
  rename(Country = Country.Territory, Population_2015 = X2015.Population) %>%
  select(Country, Population_2015)

#Join population_data to cleaned_temp_data_2010s
# and filter out rows with missing Population_2015
cleaned_temp_data_2010s <- cleaned_temp_data_2010s %>%
  left_join(population_data, by = "Country") %>%
  filter(!is.na(Population_2015))

# Sum energy consumption for each country and year
total_energy_data <- energy_data %>%
  group_by(Country, Year) %>%
  summarize(Total_Energy_Consumption = sum(Energy_consumption, na.rm = TRUE)) %>%
  ungroup()  # Remove grouping for further analysis

# Convert `Year` in `total_energy_data` to character
total_energy_data <- total_energy_data %>%
  mutate(Year = as.character(Year))

# Perform the join
cleaned_temp_data_2010s <- cleaned_temp_data_2010s %>%
  left_join(total_energy_data, by = c("Country", "year" = "Year"))

# Remove rows with NA in Total_Energy_Consumption
cleaned_temp_data_2010s <- cleaned_temp_data_2010s %>%
  filter(!is.na(Total_Energy_Consumption))

# Create neighbors list and weight matrix
neighbors <- poly2nb(data)  # Using Queen's case for adjacency
weights <- nb2listw(neighbors, style = "W")  # Weights style "W" (row-standardized)

### Data prep

# Convert cleaned_temp_data_2010s to an sf object
cleaned_temp_data_2010s_sf <- st_as_sf(cleaned_temp_data_2010s, 
                                       coords = c("Longitude", "Latitude"), 
                                       crs = 4326)  # Adjust CRS if needed

# Check the structure to ensure it's spatial
print(cleaned_temp_data_2010s_sf)

# Create neighbors list using distance (e.g., within 500 km)
coords <- st_coordinates(cleaned_temp_data_2010s_sf)

## Unable to process all points, due R memory limit.
# neighbors <- dnearneigh(coords, 0, 500000)  # Adjust distance threshold as needed (in meters)
# # Reduce the distance threshold to 100 km (100,000 meters)
# neighbors <- dnearneigh(coords, 0, 100000)  # Adjust as needed
# weights <- nb2listw(neighbors, style = "W")  # Row-standardized weights

# Randomly sample 10% of the points for analysis
set.seed(42)  # For reproducibility
sample_data <- cleaned_temp_data_2010s_sf %>%
  sample_frac(0.1)

# Extract coordinates for the sampled data
coords_sample <- st_coordinates(sample_data)

# Create neighbors and weights for the sample data
neighbors_sample <- dnearneigh(coords_sample, 0, 100000)  # Adjust distance as needed
weights_sample <- nb2listw(neighbors_sample, style = "W")

# # Moran's I for city_avg_temp in the sample
# moran_temp_sample <- moran.test(sample_data$city_avg_temp, weights_sample)
# print(moran_temp_sample)
# 
# # Moran's I for Population_2015 in the sample
# moran_pop_sample <- moran.test(sample_data$Population_2015, weights_sample)
# print(moran_pop_sample)
# 
# # Moran's I for Total_Energy_Consumption in the sample
# moran_energy_sample <- moran.test(sample_data$Total_Energy_Consumption, weights_sample)
# print(moran_energy_sample)

# Convert `sample_data` from `sf` to `sp`
sample_data_sp <- as(sample_data, "Spatial")

# Find optimal bandwidth
optimal_bw <- bw.gwr(city_avg_temp ~ Population_2015 + Total_Energy_Consumption, 
                     data = sample_data_sp, approach = "AICc")  # 'AICc' minimizes Akaike Information Criterion

# Run GWR using the optimal bandwidth
gwr_model <- gwr.basic(city_avg_temp ~ Population_2015 + Total_Energy_Consumption, 
                       data = sample_data_sp, bw = optimal_bw)
print(gwr_model)

# Add GWR coefficients back to the data
sample_data$gwr_pop_coef <- gwr_model$SDF$Population_2015
sample_data$gwr_energy_coef <- gwr_model$SDF$Total_Energy_Consumption

library(tmap)

# Updated Plot for Population Density Coefficient
tm_shape(world_map) + 
  tm_polygons(col = "grey90", border.col = "white", lwd = 0.5) +  # Base world map with borders
  tm_shape(sample_data) +
  tm_dots("gwr_pop_coef", 
          palette = "RdYlGn",  # Use a lighter, high-contrast palette
          title = "Population Density Coef", 
          size = 0.2,           # Increase point size slightly
          border.col = NA,      # Remove borders on points
          alpha = 0.7) + 
  tm_layout(main.title = "GWR Coefficient for Population Density on Temperature",
            legend.outside = TRUE,
            inner.margins = c(0.05, 0.05, 0.05, 0.05))  # Adjust margins

# Updated Plot for Energy Consumption Coefficient
tm_shape(world_map) + 
  tm_polygons(col = "grey90", border.col = "white", lwd = 0.5) +  # Base world map with borders
  tm_shape(sample_data) +
  tm_dots("gwr_energy_coef", 
          palette = "Spectral",  # Another high-contrast palette with varied colors
          title = "Energy Consumption Coef", 
          size = 0.2,           # Increase point size slightly
          border.col = NA,      # Remove borders on points
          alpha = 0.7) + 
  tm_layout(main.title = "GWR Coefficient for Energy Consumption on Temperature",
            legend.outside = TRUE,
            inner.margins = c(0.05, 0.05, 0.05, 0.05))  # Adjust margins


### Plots

# Moran's I plot for city_avg_temp in the sample
# moran.plot(cleaned_temp_data_2010s$city_avg_temp, weights_sample, main = "Moran's I Scatterplot for City Avg Temperature")

# Calculate spatial lag for city_avg_temp
sample_data$lagged_temp <- lag.listw(weights_sample, sample_data$city_avg_temp)

# Convert sample data to an sf object if not already
sample_data_sf <- st_as_sf(sample_data, coords = c("Longitude", "Latitude"), crs = 4326)

# Plot for City Average Temperature
city_avg_plot <- ggplot() +
  geom_sf(data = world_map, fill = "gray90", color = "gray50") +
  geom_sf(data = sample_data_sf, aes(color = city_avg_temp), size = 1.5) +
  scale_color_viridis_c(option = "C", name = "City Avg Temperature", 
                        limits = c(-30, 40),  # Adjust limits based on your data
                        breaks = seq(-30, 40, by = 10)) +
  labs(title = "City Average Temperature in Sampled Data") +
  theme_minimal() +
  theme(legend.position = "right")

# Plot for Spatially Lagged Temperature
lagged_temp_plot <- ggplot() +
  geom_sf(data = world_map, fill = "gray90", color = "gray50") +
  geom_sf(data = sample_data_sf, aes(color = lagged_temp), size = 1.5) +
  scale_color_viridis_c(option = "C", name = "Lagged Temperature", 
                        limits = c(18.494, 18.499),  # Adjust limits based on your data
                        breaks = seq(18.494, 18.499, by = 0.001)) +
  labs(title = "Spatially Lagged Temperature") +
  theme_minimal() +
  theme(legend.position = "right")

# Display plots side by side if needed (using patchwork)
city_avg_plot + lagged_temp_plot

















