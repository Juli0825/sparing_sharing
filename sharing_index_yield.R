# Load required libraries
library(terra)  # For raster processing (newer replacement for raster package)
library(ggplot2)  # For plotting
library(sf)  # For spatial data
library(classInt)  # For classification of values
library(RColorBrewer)  # For color palettes
library(dplyr)  # For data manipulation
library(grid)  # For custom legend
library(cowplot)

# Read raster files
sharing_index <- rast("ESA_intensity/sharing_index_2000.tif")
total_yield <- rast("Aggregated_crops/total_yield_2000_mean.tif")

# Align the rasters by resampling total_yield to match sharing_index
total_yield_aligned <- resample(total_yield, sharing_index, method="bilinear")

# Create a mask for valid data in both rasters
mask <- !is.na(sharing_index) & !is.na(total_yield_aligned)

# Apply the mask
sharing_index_masked <- mask(sharing_index, mask)
total_yield_masked <- mask(total_yield_aligned, mask)

# Create a data frame from the masked rasters
values <- c(sharing_index_masked, total_yield_masked)
df <- as.data.frame(values, xy = TRUE)
names(df) <- c("x", "y", "sharing_index", "total_yield")

# Remove any remaining NA values
df <- df %>% filter(!is.na(sharing_index), !is.na(total_yield))

# Create quantile breaks for each variable (3 classes)
sharing_breaks <- quantile(df$sharing_index, probs = seq(0, 1, length.out = 4))
yield_breaks <- quantile(df$total_yield, probs = seq(0, 1, length.out = 4))

# Assign classes (1, 2, 3) to each observation
df$sharing_class <- cut(df$sharing_index, 
                        breaks = sharing_breaks, 
                        include.lowest = TRUE, 
                        labels = FALSE)
df$yield_class <- cut(df$total_yield, 
                      breaks = yield_breaks, 
                      include.lowest = TRUE, 
                      labels = FALSE)

# Create a bivariate class (1 to 9)
df$bivar_class <- (df$sharing_class - 1) * 3 + df$yield_class

### save as a tif file ####
# Simplest approach to create bivariate GeoTIFF
# First, get the cell indices for each point in our dataframe
cells <- cellFromXY(sharing_index, as.matrix(df[, c("x", "y")]))

# Create a new raster filled with NA values
biv_rast <- sharing_index
values(biv_rast) <- NA
names(biv_rast) <- "bivariate_class"

# Assign bivariate class values to the cells
biv_rast[cells] <- df$bivar_class

# Save the raster
writeRaster(biv_rast, "sharing_index_yield/sharing_yield_2000.tif", overwrite=TRUE)

#### PLOT IN R ####
# Create a 3x3 bivariate color palette
# Purple top, blue left, green right, light green/white bottom
bivar_colors <- c(
  "#e8f4d9", "#9ed98e", "#52b353",  # Low sharing index (bottom right to middle right)
  "#93c5df", "#b8b8b8", "#7aba7a",  # Medium sharing index (lower left to middle)
  "#4575b5", "#8a6fb0", "#8860a7"   # High sharing index (top left to top right)
)

# Plot the bivariate map
p <- ggplot(df) +
  geom_raster(aes(x = x, y = y, fill = factor(bivar_class))) +
  scale_fill_manual(values = bivar_colors, guide = "none") +
  coord_equal() +
  theme_minimal() +
  labs(title = "Relationship between Sharing Index and Total Yield (2000)",
       x = "Longitude", y = "Latitude") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5)
  )

# Create a rotated diamond legend function
create_rotated_diamond_legend <- function() {
  # Create a data frame for the legend 
  # We'll use a different approach to create a rotated diamond
  x_values <- c(0, 1, 2, 1, 0) # Pentagon points
  y_values <- c(1, 0, 1, 2, 1) # Pentagon points
  
  # Create the base polygon for rotation reference
  base_poly <- data.frame(x = x_values, y = y_values)
  
  # Create a grid of points for our 3x3 matrix
  legend_df <- expand.grid(
    x = seq(0, 2, length.out = 3), 
    y = seq(0, 2, length.out = 3)
  )
  
  # Assign bivariate classes
  # We need to flip the sharing_class assignment to match the example image orientation
  legend_df$yield_class <- cut(legend_df$x, breaks = c(-Inf, 0.67, 1.33, Inf), labels = FALSE)
  legend_df$sharing_class <- cut(legend_df$y, breaks = c(-Inf, 0.67, 1.33, Inf), labels = FALSE)
  legend_df$bivar_class <- (legend_df$sharing_class - 1) * 3 + legend_df$yield_class
  
  # Create the rotated legend plot
  legend_plot <- ggplot() +
    # Add the colored squares
    geom_tile(data = legend_df, aes(x = x, y = y, fill = factor(bivar_class)), color = "white", size = 0.5) +
    scale_fill_manual(values = bivar_colors, guide = "none") +
    # Add text labels for the axes
    annotate("text", x = 1, y = 2.5, label = "High Sharing Index", size = 3.5) +
    annotate("text", x = 1, y = -0.5, label = "Low Sharing Index", size = 3.5) +
    annotate("text", x = -0.5, y = 1, label = "Low Yield", size = 3.5, angle = 90) +
    annotate("text", x = 2.5, y = 1, label = "High Yield", size = 3.5, angle = 90) +
    # Set plot limits to make space for labels
    coord_equal(xlim = c(-1, 3), ylim = c(-1, 3)) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(20, 20, 20, 20)
    )
  
  return(legend_plot)
}

# Create the legend with the rotated diamond approach
legend <- create_rotated_diamond_legend()

# Arrange the map and legend using cowplot
final_plot <- plot_grid(p, legend, ncol = 2, rel_widths = c(3, 1))
final_plot

# Save the final plot
ggsave("sharing_index_yield/sharing_yield_2000.png", final_plot, width = 12, height = 8, dpi = 300)



















