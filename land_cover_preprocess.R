install.packages("raster")
install.packages("ncdf4")

library(raster)
library(ncdf4)


setwd("Y:/sparing_sharing/sparing_rproject")

##### Land cover layers preprocessing #################

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
cropland_binary <- calc(cropland_proportion_raster, fun = function(x) {
  if (x >= 0.38) {
    return(1)  # Cropland
  } else {
    return(0)  # Non-cropland
  }
})

# Save the final binary cropland raster
writeRaster(cropland_2005, "pre_proccessed/Cropland_2005_binary.tif", format = "GTiff", overwrite = TRUE)

# Plot for verification
plot(cropland_raster, main = "Binary Cropland Map (2005)")