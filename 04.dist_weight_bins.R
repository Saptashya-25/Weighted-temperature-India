# Clear the workspace
rm(list = ls())

# Load the libraries
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
#library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(ecmwfr)
library(tidyverse)
library(lubridate)
library(abind)
library(dplyr)
library(reshape2)
library(sf)
library(dplyr)
library(raster)
library(data.table)
library(plm)
library(chillR)
library(foreign)


# Set working directory
setwd("C:/Users/saptashya.ghosh/Dropbox/agmarket_spillover/2. raw/IMD new/")


imd_crop_wg <- read.csv("IMD_Cropland_weights.csv")


#Weighted Temp Max Min
imd_crop_wg <- imd_crop_wg %>%
  mutate(
    max_temp_wg = max_temp * weight,
    min_temp_wg = min_temp * weight
  )


###Export the unique lat lon for district weighting
# Select only the lat and lon columns and keep unique combinations
imd_temp <- unique(imd_crop_wg[, c("lat", "lon")])

# Convert imd_temp to an sf object with point geometry
imd_temp_sf <- st_as_sf(imd_temp, coords = c("lon", "lat"), crs = 4326)

# Export as a shapefile
st_write(imd_temp_sf, "imd_shape_2021_2023/imd_temp_points.shp", delete_layer = TRUE)


####

#Import the district weighted imd
imd_dist_wg <- read.csv("C:/Users/saptashya.ghosh/Dropbox/agmarket_spillover/2. raw/IMD new/district_weighted_imd.csv")


##Merge the dist weighted lat lon with crop weights
imd_dist_weight_max_min <- merge(imd_crop_wg, imd_dist_wg, by = c("lat", "lon"))

# district weighted Temp Max Min
imd_dist_weight_max_min <- imd_dist_weight_max_min %>%
  mutate(
    max_temp_dist_cr = max_temp_wg * dist_prop,
    min_temp_dist_cr = min_temp_wg * dist_prop
  )

aggregated_dist <- imd_dist_weight_max_min %>%
  group_by(stname, stcode11, dtname, dtcode11, date) %>%
  summarise(
    sum_max_temp_dist_cr = sum(max_temp_dist_cr, na.rm = TRUE),
    sum_min_temp_dist_cr = sum(min_temp_dist_cr, na.rm = TRUE),
    .groups = 'drop'  # To remove the grouping after summarizing
  )



#Arrange the data
aggregated_dist <- aggregated_dist %>%
  arrange(stname, dtname, date) 


# Read the district shapefile as an sf object
district_shape <- st_read("C:/Users/saptashya.ghosh/Dropbox/CIBIL_Nirupama/1_raw/03_shapefiles/in_district.shp")

# Remove leading zero from stcode11 in district_shape
district_shape$stcode11 <- sub("^0", "", district_shape$stcode11)

# For District
district_shape$dtcode11 <- sub("^0+", "", district_shape$dtcode11)

# Convert stcode11 and dtcode11 to numeric
district_shape$stcode11 <- as.numeric(district_shape$stcode11)
district_shape$dtcode11 <- as.numeric(district_shape$dtcode11)

#Making the union
district_shape <- district_shape %>%
  group_by(dtname, stname) %>%
  mutate(district_id = cur_group_id()) %>%
  ungroup()

district_shape <- district_shape[order(district_shape$district_id),]


aggregated_districts <- district_shape %>%
  group_by(district_id) %>%
  summarize(stname = first(stname), # Keep the first 'nam' value encountered in each group
            dtname = first(dtname),
            geometry = st_union(geometry),
            .groups = 'drop')


# Calculate centroids
aggregated_districts <- st_centroid(aggregated_districts)
aggregated_districts$centroid_lat <- st_coordinates(aggregated_districts$geometry)[, 2] 
aggregated_districts$centroid_lon <- st_coordinates(aggregated_districts$geometry)[, 1]  # Longitude



# Perform a merge state name and district name

temp_data <- merge(aggregated_dist,aggregated_districts,by=c("stname","dtname"))

imd_dist_weight_max_min <- temp_data

imd_dist_weight_max_min <- imd_dist_weight_max_min[ , !(names(imd_dist_weight_max_min) %in% c("dtname.y", "stname.y", "year_stat", "Dist_LGD", 
                                                          "State_LGD", "JID"))]



# Use gsub to remove ".y" from column names
names(imd_dist_weight_max_min) <- gsub("\\.x$", "", names(imd_dist_weight_max_min))


#Arrange the data
imd_dist_weight_max_min <- imd_dist_weight_max_min %>%
  arrange(stname, dtname, date) 


#Making correct measure of Time
# Ensure `imd_dist_weight_max_min$date` is a date object
imd_dist_weight_max_min$date1 <- as.Date(imd_dist_weight_max_min$date, format = "%Y-%m-%d")

# Format `date1` as "dd-mm-yyyy"
imd_dist_weight_max_min$date1_formatted <- format(imd_dist_weight_max_min$date1, format = "%d-%m-%Y")

# Combine `date1_formatted` with a time string to create a POSIXct datetime
imd_dist_weight_max_min$DATE <- as.POSIXct(paste(imd_dist_weight_max_min$date1_formatted, "12:00:00"), format = "%d-%m-%Y %H:%M:%S")

# Extract the year from the original date object `date1`
imd_dist_weight_max_min$Year <- format(imd_dist_weight_max_min$date1, "%Y")
imd_dist_weight_max_min$Month <- format(imd_dist_weight_max_min$date1,"%m")
imd_dist_weight_max_min$Day <- format(imd_dist_weight_max_min$date1,"%d")

imd_dist_weight_max_min$Year <- as.integer(imd_dist_weight_max_min$Year)
imd_dist_weight_max_min$Month <- as.integer(imd_dist_weight_max_min$Month)
imd_dist_weight_max_min$Day <- as.integer(imd_dist_weight_max_min$Day)


#renaming the variables
names(imd_dist_weight_max_min)[names(imd_dist_weight_max_min) == "sum_max_temp_dist_cr"] <- "Tmax"
names(imd_dist_weight_max_min)[names(imd_dist_weight_max_min) == "sum_min_temp_dist_cr"] <- "Tmin"
names(imd_dist_weight_max_min)[names(imd_dist_weight_max_min) == "centroid_lat"] <- "lat_grid"
names(imd_dist_weight_max_min)[names(imd_dist_weight_max_min) == "centroid_lon"] <- "lon_grid"

temp_data_master <- imd_dist_weight_max_min
imd_dist_weight_max_min$geometry <- NULL
imd_dist_weight_max_min$date <- NULL

imd_dist_weight_max_min$lat_grid1 <- round(imd_dist_weight_max_min$lat_grid,1)

#imd_dist_weight_max_min <- imd_dist_weight_max_min[!(is.na(imd_dist_weight_max_min$Tmax) & is.na(imd_dist_weight_max_min$Tmin)), ]

all_hourtemps <- list() # Initialize as an empty list

for (i in unique(imd_dist_weight_max_min$lat_grid1)) {
  # Print the current lat_grid being processed
  print(paste("Reading lat_grid:", i))
  
  
  result <- imd_dist_weight_max_min[imd_dist_weight_max_min$lat_grid1 == i, ]
  hourtemps <- stack_hourly_temps(result, latitude = i)$hourtemps
  hourtemps <- hourtemps[order(hourtemps$Hour, hourtemps$lon_grid,hourtemps$JDay),]
  
  print(paste("Summing by Date lat_grid:", i))
  
  result <- hourtemps %>%
    group_by(stname, stcode11, dtname, dtcode11, DATE) %>%
    summarise(
      hours_lt_15 = sum(Temp < 15, na.rm = TRUE),
      hours_15_to_20 = sum(Temp >= 15 & Temp < 20, na.rm = TRUE),
      hours_20_to_25 = sum(Temp >= 20 & Temp < 25, na.rm = TRUE),
      hours_25_to_30 = sum(Temp >= 25 & Temp < 30, na.rm = TRUE),
      hours_30_to_35 = sum(Temp >= 30 & Temp < 35, na.rm = TRUE),
      hours_gt_35 = sum(Temp > 35, na.rm = TRUE)
    ) %>%
    ungroup() # To remove the grouping structure, if not needed further
  
  result$year <- as.numeric(substr(result$DATE, 1, 4))
  result$month <- as.numeric(substr(result$DATE, 6,7))
  
  # Summing by month and calendar year
  result <- result %>%
    group_by(stname, stcode11, dtname, dtcode11, month, year) %>%
    summarise(
      sum_hours_lt_15 = sum(hours_lt_15, na.rm = TRUE)/24,
      sum_hours_15_to_20 = sum(hours_15_to_20, na.rm = TRUE)/24,
      sum_hours_20_to_25 = sum(hours_20_to_25, na.rm = TRUE)/24,
      sum_hours_25_to_30 = sum(hours_25_to_30, na.rm = TRUE)/24,
      sum_hours_30_to_35 = sum(hours_30_to_35, na.rm = TRUE)/24,
      sum_hours_gt_35 = sum(hours_gt_35, na.rm = TRUE)/24,
      .groups = 'drop'  # This ensures the result is not grouped
    )
  
  
  all_hourtemps[[as.character(i)]] <- result
  
  rm(hourtemps)
}

final_yearly_gdd <- do.call(rbind, all_hourtemps)

# Export to CSV (just to save it)
write.csv(final_yearly_gdd, "C:/Users/saptashya.ghosh/Dropbox/agmarket_spillover/2. raw/IMD new/GDD_bins_new.csv", row.names = FALSE)


