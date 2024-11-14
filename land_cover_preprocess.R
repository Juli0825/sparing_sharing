install.packages("raster")
install.packages("ncdf4")

library(raster)
library(ncdf4)

##### Land cover layers preprocessing #################
#Load Land cover data
LC_2005_nc <- "Data/ESA_LC/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2005-v2.0.7cds.nc"

# Open the NC file
land_cover_2005 <- nc_open(LC_2005_nc)

# Print the variable names
print(land_cover_2005$var)

# Load the NetCDF file as a raster brick
land_cover_2005_raster <- brick(LC_2005_nc, varname = "lccs_class")  # 

print(land_cover_2005_raster)

# Define cropland and non-cropland codes 
# Codes 10 and 20 are fully cropland
# For codes 30 and 40, we want cells with > 58% of code 30 and > 38% of code 40 to be cropland

# Create initial binary classification
cropland_2005 <- calc(land_cover_2005_raster, fun = function(x) {
  if (x == 10 || x == 20) {
    return(1)  # Cropland
  } else if (x == 30) {
    return(0.58)  # 58% weight towards cropland
  } else if (x == 40) {
    return(0.38)  # 38% weight towards cropland
  } else {
    return(0)  # Non-cropland
  }
})

# Apply a threshold of 0.38 to include code 40 as cropland
cropland_2005 <- calc(cropland_2005, fun = function(x) {
  if (x >= 0.38) {
    return(1)  # Classified as cropland
  } else {
    return(0)  # Classified as non-cropland
  }
})

# Save the final binary cropland raster
writeRaster(cropland_2005, "Data/ESA_LC/Cropland_2005_binary.tif", format = "GTiff", overwrite = TRUE)

# Plot for verification
plot(cropland_raster, main = "Binary Cropland Map (2005)")