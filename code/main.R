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
library(corrplot)
library(spatstat)
library(eks)
library(ks)
library(tmap)
library(spdep)
library(spatialreg)
library(GWmodel)
library(patchwork)


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
# 
# # Google Drive File ID for the HydroLAKES zip file
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

# Step 4: Load the shapefile
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

# Extract the year from the date column
cleaned_temp_data$year <- format(cleaned_temp_data$dt, "%Y")

# Group by City and Year to calculate yearly weighted temperatures for each city. needed for spatial analysis.
yearly_weighted_temp <- cleaned_temp_data %>%
  group_by(City, year, Longitude, Latitude) %>%
  summarise(yearly_weighted_temp = sum(weighted_city_avg_temp, na.rm = TRUE), .groups = 'drop')

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

# Plot temperature points directly on world map
ggplot() +
  # Plot the world map polygons as background
  geom_sf(data = world_map_df, fill = "grey80", color = "white") +
  # Plot the points with color indicating temperature
  geom_point(data = temp_points_2010s, aes(x = Longitude, y = Latitude, color = city_avg_temp), 
             size = 1, alpha = 0.8) +
  scale_color_viridis_c(option = "magma", name = "City Avg Temp (°C)") +
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
  scale_color_viridis_c(name = "City Avg Temp (°C)") +
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
  tm_dots(col = "city_avg_temp", palette = "viridis", size = 0.5, alpha = 0.7, title = "City Avg Temp (°C)") +
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

# Perform the MAD test for Ripley’s K-function
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

















