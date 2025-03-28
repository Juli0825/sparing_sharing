library(terra)
library(tidyverse)
library(viridis)



# Function to create a simple bivariate map of cropland change vs sharing index change
create_simple_bivariate_map <- function(cropland_change_file, sharing_change_file, output_file = NULL) {
  # Load required packages
  if (!requireNamespace("terra", quietly = TRUE)) stop("Package 'terra' is needed")
  
  # Read the input rasters
  crop_change <- terra::rast("only_cropland/cropland_change_2010_2020.tif")
  sharing_change <- terra::rast("only_cropland/sharing_change_co_2010_2020.tif")
  
  # Make sure the rasters are aligned
  sharing_change <- terra::resample(sharing_change, crop_change, method = "near")
  
  # Create binary categories for both variables
  crop_cat <- crop_change
  crop_cat[] <- NA  # Initialize with NA
  crop_cat[crop_change[] < 0] <- 0  # Contraction
  crop_cat[crop_change[] >= 0] <- 1  # Expansion
  
  share_cat <- sharing_change
  share_cat[] <- NA  # Initialize with NA
  share_cat[sharing_change[] < 0] <- 0  # Decrease
  share_cat[sharing_change[] >= 0] <- 1  # Increase
  
  # Combine the two categorical rasters to create a bivariate raster (values 0-3)
  bivariate <- crop_cat + share_cat * 2
  
  # Define clear, intuitive colors for the 4 categories
  bivariate_colors <- c(
    "#E8A77C",  # 0: Cropland contraction + Sharing decrease (orange-brown)
    "#8FB5DB",  # 1: Cropland expansion + Sharing decrease (light blue)
    "#E0678F",  # 2: Cropland contraction + Sharing increase (pink-red)
    "#7352B3"   # 3: Cropland expansion + Sharing increase (purple)
  )
  
  # Plot the map and legend
  par(mfrow = c(1, 1), mar = c(1, 1, 2, 8))
  
  # Plot the bivariate map
  plot(bivariate, 
       col = bivariate_colors,
       breaks = c(-0.5, 0.5, 1.5, 2.5, 3.5),
       legend = FALSE,
       main = "Cropland Change vs Sharing Index Change (2010-2020)")
  
  # Add a simple 2x2 legend with clear labels
  xmax <- par("usr")[2]
  ymax <- par("usr")[4]
  ymin <- par("usr")[3]
  legend_height <- (ymax - ymin) * 0.4
  legend_y_pos <- ymin + (ymax - ymin) * 0.3
  
  # Draw legend boxes
  rect(xmax + (xmax * 0.005), legend_y_pos, 
       xmax + (xmax * 0.035), legend_y_pos + legend_height/2, 
       col = bivariate_colors[1], border = "black")
  rect(xmax + (xmax * 0.035), legend_y_pos, 
       xmax + (xmax * 0.065), legend_y_pos + legend_height/2, 
       col = bivariate_colors[2], border = "black")
  rect(xmax + (xmax * 0.005), legend_y_pos + legend_height/2, 
       xmax + (xmax * 0.035), legend_y_pos + legend_height, 
       col = bivariate_colors[3], border = "black")
  rect(xmax + (xmax * 0.035), legend_y_pos + legend_height/2, 
       xmax + (xmax * 0.065), legend_y_pos + legend_height, 
       col = bivariate_colors[4], border = "black")
  
  # Add labels
  text(xmax + (xmax * 0.02), legend_y_pos - (legend_height * 0.1), "Contraction", cex = 0.8)
  text(xmax + (xmax * 0.05), legend_y_pos - (legend_height * 0.1), "Expansion", cex = 0.8)
  text(xmax + (xmax * 0.09), legend_y_pos + legend_height/4, "Decrease", cex = 0.8, srt = 270)
  text(xmax + (xmax * 0.09), legend_y_pos + legend_height*3/4, "Increase", cex = 0.8, srt = 270)
  
  # Add axis titles
  text(xmax + (xmax * 0.035), legend_y_pos - (legend_height * 0.25), "Cropland Change", cex = 0.9, font = 2)
  text(xmax + (xmax * 0.12), legend_y_pos + legend_height/2, "Sharing Index Change", cex = 0.9, font = 2, srt = 270)
  
  # Highlight the area of interest (purple box)
  rect(xmax + (xmax * 0.035), legend_y_pos + legend_height/2, 
       xmax + (xmax * 0.065), legend_y_pos + legend_height, 
       col = NULL, border = "black", lwd = 2)
  text(xmax + (xmax * 0.15), legend_y_pos + legend_height*3/4, 
       "Area of Interest:\nExpansion + Sharing", cex = 0.8)
  
  # Optionally save the bivariate raster
  if (!is.null(output_file)) {
    terra::writeRaster(bivariate, output_file, overwrite = TRUE)
  }
  
  # Reset plot parameters
  par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))
  
  # Return the bivariate raster
  return(bivariate)
}

# Example usage
# bivariate_result <- create_simple_bivariate_map(
#   "cropland_change_2010_2020.tif", 
#   "sharing_change_co_2010_2020.tif",
#   "crop_sharing_bivariate.tif"
# )