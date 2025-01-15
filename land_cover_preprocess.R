install.packages("raster")
install.packages("ncdf4")

library(raster)
library(ncdf4)
library(terra)


setwd("Y:/sparing_sharing/sparing_rproject")

##### Land cover layers preprocessing #################

#Load 2005 Land cover data
LC2000_nc <- "Data/ESA_LC/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2000-v2.0.7cds.nc"

# Load only the first time slice of the variable 'lccs_class' as a raster layer
landcover_2000_raster <- brick(LC2000_nc, varname = "lccs_class", band = 1)  # 'band = 1' to specify first time slice

print(landcover_2005_raster)

# Save the raster as a GeoTIFF for easier future access
writeRaster(landcover_2000_raster, "Data/ESA_LC/landcover_2000raster.tif", format = "GTiff", overwrite = TRUE)

# Read the GeoTIFF file
lc2000_raster <- raster("Data/ESA_LC/landcover_2000raster.tif")

# Print basic information about the raster
print(lc2005_raster)

# Plot the raster
plot(lc2005_raster, main = "Land Cover 2005")

# Define cropland and non-cropland codes 
# Codes 10 and 20 are fully cropland
# For codes 30 and 40, we want cells with > 58% of code 30 and > 38% of code 40 to be cropland

# Define cropland classification function
expand_and_classify <- function(x) {
  # Create a vector to represent the 100 sub-cells
  sub_cells <- rep(0, 100)  # Start with all non-cropland (0)
  
  # Assign based on land cover code
  if (x == 10 || x == 20) {
    sub_cells <- rep(1, 100)  # All cropland for codes 10 and 20
  } else if (x == 30) {
    sub_cells[1:58] <- 1  # Assign 58 out of 100 as cropland
  } else if (x == 40) {
    sub_cells[1:38] <- 1  # Assign 38 out of 100 as cropland
  }
  
  return(mean(sub_cells))  # Return the proportion of cropland
}

# Apply the classification function to each cell in the raster
cropland_proportion_2000_raster <- calc(lc2000_raster, fun = expand_and_classify)
# Save the final binary cropland raster
writeRaster(cropland_proportion_2000_raster, "pre_proccessed/cropland_proportion_2000_raster1.tif", format = "GTiff", overwrite = TRUE)


# Threshold to create a binary map of cropland vs non-cropland
# If proportion >= 0.38, classify as cropland (1), otherwise non-cropland (0)
cropland_binary <- calc(cropland_proportion_2000_raster, fun = function(x) {
  if (x >= 0.38) {
    return(1)  # Cropland
  } else {
    return(0)  # Non-cropland
  }
})

# Save the final binary cropland raster
writeRaster(cropland_proportion_2000_raster, "pre_proccessed/cropland_proportion_2000_raster2.tif", format = "GTiff", overwrite = TRUE)

# Plot for verification
plot(cropland_raster, main = "Binary Cropland Map (2005)")

#Load 2005 Land cover data
LC2005_nc <- "Data/ESA_LC/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2005-v2.0.7cds.nc"

# Load only the first time slice of the variable 'lccs_class' as a raster layer
landcover_2005_raster <- brick(LC2005_nc, varname = "lccs_class", band = 1)  # 'band = 1' to specify first time slice

print(landcover_2005_raster)

# Save the raster as a GeoTIFF for easier future access
writeRaster(landcover_2005_raster, "Data/ESA_LC/landcover_2005raster.tif", format = "GTiff", overwrite = TRUE)

# Read the GeoTIFF file
lc2005_raster <- raster("Data/ESA_LC/landcover_2005raster.tif")

# Print basic information about the raster
print(lc2005_raster)

# Plot the raster
plot(lc2005_raster, main = "Land Cover 2005")

# Define cropland and non-cropland codes 
# Codes 10 and 20 are fully cropland
# For codes 30 and 40, we want cells with > 58% of code 30 and > 38% of code 40 to be cropland

# Define cropland classification function
expand_and_classify <- function(x) {
  # Create a vector to represent the 100 sub-cells
  sub_cells <- rep(0, 100)  # Start with all non-cropland (0)
  
  # Assign based on land cover code
  if (x == 10 || x == 20) {
    sub_cells <- rep(1, 100)  # All cropland for codes 10 and 20
  } else if (x == 30) {
    sub_cells[1:58] <- 1  # Assign 58 out of 100 as cropland
  } else if (x == 40) {
    sub_cells[1:38] <- 1  # Assign 38 out of 100 as cropland
  }
  
  return(mean(sub_cells))  # Return the proportion of cropland
}

# Apply the classification function to each cell in the raster
cropland_proportion_2005_raster <- calc(lc2005_raster, fun = expand_and_classify)

# Threshold to create a binary map of cropland vs non-cropland
# If proportion >= 0.38, classify as cropland (1), otherwise non-cropland (0)
cropland_binary <- calc(cropland_proportion_2005_raster, fun = function(x) {
  if (x >= 0.38) {
    return(1)  # Cropland
  } else {
    return(0)  # Non-cropland
  }
})

# Save the final binary cropland raster
writeRaster(cropland_proportion_2005_raster, "pre_proccessed/cropland_proportion_2005_raster.tif", format = "GTiff", overwrite = TRUE)

# Plot for verification
plot(cropland_raster, main = "Binary Cropland Map (2005)")

######### Load 2010 Land cover data
LC2010_nc <- "Data/ESA_LC/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2010-v2.0.7cds.nc"

# Load only the first time slice of the variable 'lccs_class' as a raster layer
landcover_2010_raster <- brick(LC2010_nc, varname = "lccs_class", band = 1)  # 'band = 1' to specify first time slice

print(landcover_2010_raster)

# Save the raster as a GeoTIFF for easier future access
writeRaster(landcover_2010_raster, "Data/ESA_LC/landcover_2010raster.tif", format = "GTiff", overwrite = TRUE)

# Read the GeoTIFF file
lc2010_raster <- raster("Data/ESA_LC/landcover_2010raster.tif")

# Print basic information about the raster
print(lc2010_raster)

# Plot the raster
plot(lc2010_raster, main = "Land Cover 2010")

# Define cropland and non-cropland codes 
# Codes 10 and 20 are fully cropland
# For codes 30 and 40, we want cells with > 58% of code 30 and > 38% of code 40 to be cropland

# Define cropland classification function
expand_and_classify <- function(x) {
  # Create a vector to represent the 100 sub-cells
  sub_cells <- rep(0, 100)  # Start with all non-cropland (0)
  
  # Assign based on land cover code
  if (x == 10 || x == 20) {
    sub_cells <- rep(1, 100)  # All cropland for codes 10 and 20
  } else if (x == 30) {
    sub_cells[1:58] <- 1  # Assign 58 out of 100 as cropland
  } else if (x == 40) {
    sub_cells[1:38] <- 1  # Assign 38 out of 100 as cropland
  }
  
  return(mean(sub_cells))  # Return the proportion of cropland
}

# Apply the classification function to each cell in the raster
cropland_proportion_2010_raster <- calc(lc2005_raster, fun = expand_and_classify)
# Save the raster as a GeoTIFF for easier future access
writeRaster(cropland_proportion_2010_raster, "pre_proccessed/cropland_proportion_2010_raster1.tif", format = "GTiff", overwrite = TRUE)

cropland_proportion_2010_raster <- raster ("pre_proccessed/cropland_proportion_2010_raster1.tif")
# Threshold to create a binary map of cropland vs non-cropland
# If proportion >= 0.38, classify as cropland (1), otherwise non-cropland (0)
cropland_binary <- calc(cropland_proportion_2010_raster, fun = function(x) {
  if (x >= 0.38) {
    return(1)  # Cropland
  } else {
    return(0)  # Non-cropland
  }
})

# Save the final binary cropland raster
writeRaster(cropland_proportion_2010_raster, "pre_proccessed/cropland_proportion_2010_raster2.tif", format = "GTiff", overwrite = TRUE)

######### Load 2017 Land cover data
LC2017_nc <- "Data/ESA_LC/C3S-LC-L4-LCCS-Map-300m-P1Y-2017-v2.1.1.nc"

# Load only the first time slice of the variable 'lccs_class' as a raster layer
landcover_2017_raster <- brick(LC2017_nc, varname = "lccs_class", band = 1)  # 'band = 1' to specify first time slice

print(landcover_2010_raster)

# Save the raster as a GeoTIFF for easier future access
writeRaster(landcover_2017_raster, "Data/ESA_LC/landcover_2017raster.tif", format = "GTiff", overwrite = TRUE)

# Read the GeoTIFF file
lc2017_raster <- raster("Data/ESA_LC/landcover_2017raster.tif")

# Print basic information about the raster
print(lc2017_raster)

# Plot the raster
plot(lc2010_raster, main = "Land Cover 2010")

# Define cropland and non-cropland codes 
# Codes 10 and 20 are fully cropland
# For codes 30 and 40, we want cells with > 58% of code 30 and > 38% of code 40 to be cropland

# Define cropland classification function
expand_and_classify <- function(x) {
  # Create a vector to represent the 100 sub-cells
  sub_cells <- rep(0, 100)  # Start with all non-cropland (0)
  
  # Assign based on land cover code
  if (x == 10 || x == 20) {
    sub_cells <- rep(1, 100)  # All cropland for codes 10 and 20
  } else if (x == 30) {
    sub_cells[1:58] <- 1  # Assign 58 out of 100 as cropland
  } else if (x == 40) {
    sub_cells[1:38] <- 1  # Assign 38 out of 100 as cropland
  }
  
  return(mean(sub_cells))  # Return the proportion of cropland
}

# Apply the classification function to each cell in the raster
cropland_proportion_2017_raster <- calc(landcover_2017_raster, fun = expand_and_classify)
# Save the raster as a GeoTIFF for easier future access
writeRaster(cropland_proportion_2017_raster, "pre_proccessed/cropland_proportion_2017_raster1.tif", format = "GTiff", overwrite = TRUE)

# Threshold to create a binary map of cropland vs non-cropland
# If proportion >= 0.38, classify as cropland (1), otherwise non-cropland (0)
cropland_binary <- calc(cropland_proportion_2017_raster, fun = function(x) {
  if (x >= 0.38) {
    return(1)  # Cropland
  } else {
    return(0)  # Non-cropland
  }
})

# Save the final binary cropland raster
writeRaster(cropland_proportion_2017_raster, "pre_proccessed/cropland_proportion_2017_raster2.tif", format = "GTiff", overwrite = TRUE)

######### Load 2020 Land cover data
LC2022_nc <- "Data/ESA_LC/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.nc"

# Load only the first time slice of the variable 'lccs_class' as a raster layer
landcover_2022_raster <- brick(LC2022_nc, varname = "lccs_class", band = 1)  # 'band = 1' to specify first time slice

print(landcover_2010_raster)

# Save the raster as a GeoTIFF for easier future access
writeRaster(landcover_2022_raster, "Data/ESA_LC/landcover_2022raster.tif", format = "GTiff", overwrite = TRUE)

# Print basic information about the raster
print(lc2022_raster)

# Plot the raster
plot(lc2022_raster, main = "Land Cover 2022")

# Define cropland and non-cropland codes 
# Codes 10 and 20 are fully cropland
# For codes 30 and 40, we want cells with > 58% of code 30 and > 38% of code 40 to be cropland

# Define cropland classification function
expand_and_classify <- function(x) {
  # Create a vector to represent the 100 sub-cells
  sub_cells <- rep(0, 100)  # Start with all non-cropland (0)
  
  # Assign based on land cover code
  if (x == 10 || x == 20) {
    sub_cells <- rep(1, 100)  # All cropland for codes 10 and 20
  } else if (x == 30) {
    sub_cells[1:58] <- 1  # Assign 58 out of 100 as cropland
  } else if (x == 40) {
    sub_cells[1:38] <- 1  # Assign 38 out of 100 as cropland
  }
  
  return(mean(sub_cells))  # Return the proportion of cropland
}

# Apply the classification function to each cell in the raster
cropland_proportion_2022_raster <- calc(landcover_2022_raster, fun = expand_and_classify)
# Save the raster as a GeoTIFF for easier future access
writeRaster(cropland_proportion_2022_raster, "pre_proccessed/cropland_proportion_2022_raster1.tif", format = "GTiff", overwrite = TRUE)

# Threshold to create a binary map of cropland vs non-cropland
# If proportion >= 0.38, classify as cropland (1), otherwise non-cropland (0)
cropland_binary <- calc(cropland_proportion_2022_raster, fun = function(x) {
  if (x >= 0.38) {
    return(1)  # Cropland
  } else {
    return(0)  # Non-cropland
  }
})

# Save the final binary cropland raster
writeRaster(cropland_proportion_2022_raster, "pre_proccessed/cropland_proportion_2022_raster2.tif", format = "GTiff", overwrite = TRUE)

##### Resample to 250m resolution#########
crop_2020 <- raster("pre_proccessed/cropland_proportion_2020_raster2.tif")

# Record the system start time
start_time <- Sys.time()
start_time
# Define the target resolution in degrees (250m resolution in longlat CRS)
# 1 degree = ~111 km, so 250m = 250 / 111000 degrees
target_resolution <- 250 / 111000

# Create a template raster with the new resolution
template_raster <- raster(extent(crop_2020), resolution = target_resolution, crs = crs(crop_2020))

# Resample the raster using nearest neighbor
resampled_raster <- resample(crop_2020, template_raster, method = "ngb")

# Save or plot the resampled raster
writeRaster(resampled_raster, "resampled_250m/cropland_2020_250m.tif", format = "GTiff", overwrite = TRUE)
# Record the system end time
end_time <- Sys.time()
end_time


plot(resampled_raster)

# Load the required library
library(terra)

# Load the raster
cropland_raster <- rast("path_to_your_raster.grd")

# Define the target resolution in degrees
target_resolution <- 250 / 111000

# Resample the raster using nearest neighbor
resampled_raster <- resample(cropland_raster, 
                             terra::rast(extent(cropland_raster), 
                                         resolution = target_resolution, 
                                         crs = crs(cropland_raster)), 
                             method = "near")

# Save or plot the resampled raster
writeRaster(resampled_raster, "cropland_resampled_250m.tif", overwrite = TRUE)
plot(resampled_raster)

resample <- raster("resampled_250m/cropland_2000_250m.tif")
yield <- raster("Data/2000/yield_2000/spam2000V3r107_global_Y_BANP_A.tif")

