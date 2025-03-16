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

# Create a 3x3 bivariate color palette
# Red represents high sharing index, Blue represents high yield
bivar_colors <- c(
  "#e8e8e8", "#b5d4e9", "#35978f",  # Low sharing index
  "#dfb0d6", "#a3a0cb", "#5560AA",  # Medium sharing index
  "#e4419d", "#ab12a3", "#6a51a3"   # High sharing index
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

# Function to create the diamond legend
create_bivariate_legend <- function() {
  # Create a 3x3 matrix for the legend
  legend_df <- expand.grid(x = 1:3, y = 1:3)
  legend_df$bivar_class <- (legend_df$y - 1) * 3 + legend_df$x
  
  # Create the plot
  legend_plot <- ggplot(legend_df) +
    geom_tile(aes(x = x, y = y, fill = factor(bivar_class))) +
    scale_fill_manual(values = bivar_colors, guide = "none") +
    coord_equal() +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "white"),
      plot.margin = unit(c(1, 1, 1, 1), "cm")
    ) +
    # Add labels
    annotate("text", x = 0.5, y = 2, label = "Low", angle = 90) +
    annotate("text", x = 3.5, y = 2, label = "High", angle = 90) +
    annotate("text", x = 2, y = 0.5, label = "Low") +
    annotate("text", x = 2, y = 3.5, label = "High") +
    # Add variable names
    annotate("text", x = 0, y = 2, label = "Sharing Index", angle = 90, hjust = 0.5) +
    annotate("text", x = 2, y = 0, label = "Total Yield", hjust = 0.5)
  
  return(legend_plot)
}

# Create the legend
legend <- create_bivariate_legend()

# Use cowplot to arrange the plots
final_plot <- plot_grid(p, legend, ncol = 2, rel_widths = c(3, 1))

# Save the final plot
ggsave("sharing_index_yield/sharing_yield_2000.png", final_plot, width = 12, height = 8, dpi = 300)
