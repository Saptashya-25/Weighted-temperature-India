# Clear the workspace
rm(list = ls())

# Load the ncdf4 package
library(ncdf4)

# Set working directory
setwd("C:/Users/saptashya.ghosh/Dropbox/SG/ICRISAT_GDD/Data/Grassland")

# Open the netCDF file
nc_file <- nc_open("cropland.nc")

# Extract latitude, longitude, and time variables
lats <- ncvar_get(nc_file, "lat")  # Replace with correct variable name if needed
lons <- ncvar_get(nc_file, "lon")  # Replace with correct variable name if needed
times <- ncvar_get(nc_file, "time")  # Replace with correct variable name if needed

# Define the desired latitude and longitude bounds
lat_min <- 7.5
lat_max <- 37.5
lon_min <- 67.5
lon_max <- 97.5

# Find indices for latitude and longitude within the desired range
lat_indices <- which(lats >= lat_min & lats <= lat_max)
lon_indices <- which(lons >= lon_min & lons <= lon_max)

# Convert time values to Date format
time_units <- ncatt_get(nc_file, "time", "units")$value  # Get the time units (e.g., "days since 1970-01-01")
time_origin <- as.Date(sub("days since ", "", time_units))
dates <- time_origin + times

# Get indices for the year 2016
time_indices <- which(format(dates, "%Y") == "2015")

# Ensure there are valid time indices for 2016
if (length(time_indices) == 0) {
  stop("No data found for the year 2016. Please check the time range.")
}

# Extract the relevant data for observation_count
# You can adjust "observation_count" to the actual variable name you are interested in
cropland_subset <- ncvar_get(nc_file, "lccs_class",
                             start = c(min(lon_indices), min(lat_indices), min(time_indices)),  # Start indices for lon, lat, time
                             count = c(length(lon_indices), length(lat_indices), length(time_indices)))  # Count for lon, lat, time

# Close the netCDF file
nc_close(nc_file)

# Reshape the extracted data into a data frame
# Create a grid of lon, lat, and time values for the subset
lon_values <- lons[lon_indices]
lat_values <- lats[lat_indices]
time_values <- dates[time_indices]

# Expand the grid for lon, lat, and time
grid <- expand.grid(lon_crop = lon_values, lat_crop = lat_values)

# Flatten the cropland_subset array and combine it with the grid
cropland <- data.frame(grid, lccs_class = as.vector(cropland_subset))

# Import the IMD file
imd_data <- read.csv("C:/Users/saptashya.ghosh/Dropbox/agmarket_spillover/2. raw/IMD new/IMD_all_new.csv")

# Select only the lat and lon columns and keep unique combinations
imd_temp <- unique(imd_data[, c("lat", "lon")])


# Define the values of interest for lccs_class
values_to_check <- c(10, 11, 12, 20, 30, 40)

# Initialize a weight column in imd_data
imd_temp$weight <- 0

#Finding weights Sir's method

# Loop over each row in imd_data
for (i in 1:nrow(imd_temp)) {
  lat <- imd_temp$lat[i]
  lon <- imd_temp$lon[i]
  
  # Find indices in cropland data within the grid cell corresponding to the current lat/lon in imd_data
  # Find the lat and lon indices within the bounds for cropland data
  
  index <- which(cropland$lat_crop >= lat-1 & cropland$lat_crop < lat  & cropland$lon_crop >= lon-1 & cropland$lon_crop < lon)
  
   if (length(index) > 0) {
    
  lccs_class_subset <- cropland$lccs_class[index]
  mask <- lccs_class_subset %in% values_to_check
  count_specified_values <- sum(mask)
  count <- length(mask)
  
  # Calculate the weight and assign it to imd_data
  weight <- ifelse(count > 0, count_specified_values / count, 0)
  imd_temp$weight[i] <- weight
} else {
  # Assign a weight of 0 if no matching indices are found
  imd_temp$weight[i] <- 0
}
}


# Merge imd_temp with imd_data by lat and lon
imd_data <- merge(imd_data, imd_temp, by = c("lat", "lon"))

# Export to CSV (just to save it)
write.csv(imd_data, "C:/Users/saptashya.ghosh/Dropbox/agmarket_spillover/2. raw/IMD new/IMD_Cropland_weights.csv", row.names = FALSE)

cropland_weighted <- read.csv("C:/Users/saptashya.ghosh/Dropbox/agmarket_spillover/2. raw/IMD new/IMD_Cropland_weights.csv")
