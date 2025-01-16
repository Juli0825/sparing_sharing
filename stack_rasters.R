# Load necessary libraries
library(terra)

########## Crop 2000, only 21 crops ################

# Define the parent directories for each data type
crop_yield_folder <- "Data/2000/yield_2000"
harvested_area_folder <- "Data/2000/harv_2000"
physical_area_folder <- "Data/2000/phy_2000"

# Define the pattern to match (_a for all technologies)
file_pattern <- "_A"

# Function to create a raster stack for each crop
process_crops <- function(yield_folder, harvested_folder, physical_folder, pattern) {
  # List all files in each folder matching the pattern
  yield_files <- list.files(yield_folder, pattern = pattern, full.names = TRUE)
  harvested_files <- list.files(harvested_folder, pattern = pattern, full.names = TRUE)
  physical_files <- list.files(physical_folder, pattern = pattern, full.names = TRUE)
  
  # Extract crop names from file names (e.g., BANP from spam2000V3r107_global_Y_BANP_A.tif)
  crop_names <- unique(gsub(".*_([A-Z]+)_A\\.tif$", "\\1", basename(yield_files)))
  
  # Initialize a list to store stacks for each crop
  crop_stacks <- list()
  
  # Loop through each crop name
  for (crop in crop_names) {
    # Find matching files for the current crop
    yield_file <- grep(paste0("_", crop, "_A\\.tif$"), yield_files, value = TRUE)
    harvested_file <- grep(paste0("_", crop, "_A\\.tif$"), harvested_files, value = TRUE)
    physical_file <- grep(paste0("_", crop, "_A\\.tif$"), physical_files, value = TRUE)
    
    # Check if all files exist for the crop
    if (length(yield_file) == 1 && length(harvested_file) == 1 && length(physical_file) == 1) {
      # Load rasters and create a stack
      yield_raster <- rast(yield_file)
      harvested_raster <- rast(harvested_file)
      physical_raster <- rast(physical_file)
      
      crop_stack <- c(yield_raster, harvested_raster, physical_raster)
      names(crop_stack) <- c("Yield", "Harvested_Area", "Physical_Area")
      
      # Store the stack in the list
      crop_stacks[[crop]] <- crop_stack
    } else {
      warning(paste("Missing files for crop:", crop))
    }
  }
  
  # Return the list of stacks
  return(crop_stacks)
}

# Run the function for the folders
crop_stacks <- process_crops(crop_yield_folder, harvested_area_folder, physical_area_folder, file_pattern)

# Save or process the stacks as needed
output_folder <- "Y:/sparing_sharing/sparing_rproject/stack_trial"
for (crop in names(crop_stacks)) {
  writeRaster(crop_stacks[[crop]], 
              filename = file.path(output_folder, paste0(crop, "_stack.tif")), 
              overwrite = TRUE)
}

############ verify the stack
# Load the stack file (replace with the actual stack file path)
stack_file <- "stack_trial/BANP_stack.tif"
stack <- rast(stack_file)

# Check the structure and metadata of the stack
print(stack)
cat("Number of layers in stack: ", nlyr(stack), "\n")
cat("Layer names: ", names(stack), "\n")

# Check basic statistics for each layer
stack_stats <- summary(stack)
print(stack_stats)

# Visualize the stack layers
plot(stack, main = "Stack Layers")

# Check for unexpected values (e.g., large areas with values outside expected ranges)
for (i in 1:nlyr(stack)) {
  cat("\nChecking layer: ", names(stack)[i], "\n")
  cat("Min: ", min(stack[[i]][], na.rm = TRUE), "\n")
  cat("Max: ", max(stack[[i]][], na.rm = TRUE), "\n")
  cat("Unique values (first 10): ", unique(stack[[i]][])[1:10], "\n")
}

# Check if there are any black areas (e.g., large areas with 0 or specific values)
black_area_check <- stack == 0
black_area <- sum(black_area_check[], na.rm = TRUE)

cat("\nNumber of black (value = 0) pixels in the stack: ", black_area, "\n")


# Remove pixels with NA in any layer
stack_no_na <- mask(stack, !is.na(stack))

# Check statistics of the cleaned stack
stack_stats_no_na <- summary(stack_no_na)
print(stack_stats_no_na)


# Create a mask where all layers have non-NA values
non_na_mask <- !is.na(stack[[1]]) & !is.na(stack[[2]]) & !is.na(stack[[3]])

# Apply the mask to all layers
stack_no_na <- mask(stack, non_na_mask)

# Check statistics of the cleaned stack
stack_stats_no_na <- summary(stack_no_na)
print(stack_stats_no_na)

# Save the cleaned stack
writeRaster(stack_no_na, "cleaned_stack_no_na.tif", overwrite = TRUE)
# Check the extent of each layer in the stack
print(ext(stack[[1]]))
print(ext(stack[[2]]))
print(ext(stack[[3]]))

print(crs(stack[[1]]))
print(crs(stack[[2]]))
print(crs(stack[[3]]))

# Save the cleaned stack
writeRaster(stack_no_na, "cleaned_stack.tif", overwrite = TRUE)

bn <- raster("Data/2000/harv_2000/spam2000V3r107_global_H_BANP_A.tif")


#### One single stack ###

# Define the directory containing the individual stacks
input_folder <- "Y:/sparing_sharing/sparing_rproject/stack_trial"

# List all individual stack files (assuming they have '_stack.tif' in their names)
stack_files <- list.files(input_folder, pattern = "_stack\\.tif$", full.names = TRUE)

# Initialize an empty list to store all layers
all_layers <- list()

# Loop through each stack file and extract its layers
for (file in stack_files) {
  # Load the individual stack
  crop_stack <- rast(file)
  
  # Add layers to the list with meaningful names
  crop_name <- gsub("_stack\\.tif$", "", basename(file)) # Extract crop name from the file name
  
  # Rename the layers to include crop name
  names(crop_stack) <- paste(crop_name, names(crop_stack), sep = "_")
  
  # Append the layers to the list
  all_layers <- c(all_layers, unlist(crop_stack, use.names = FALSE))
}

# Combine all layers into a single stack
cropstack_2000 <- rast(all_layers)

# Check the combined stack
print(cropstack_2000)
cat("Number of layers in combined stack: ", nlyr(cropstack_2000), "\n")

# Save the combined stack
output_file <- "Y:/sparing_sharing/sparing_rproject/stack_trial/cropstack_2000.tif"
writeRaster(cropstack_2000, filename = output_file, overwrite = TRUE)

cat("Combined stack saved to: ", output_file, "\n")

stack_files <- list.files(input_folder, pattern = "_stack\\.tif$", full.names = TRUE)

for (file in stack_files) {
  crop_stack <- rast(file)
  cat("File:", file, "- Layers:", nlyr(crop_stack), "\n")
}

################# Crop stack 2005 ############################################
# Record the system start time
start_time <- Sys.time()
start_time
# Define the parent directories for each data type (update for 2005)
crop_yield_folder <- "Data/2005/yield_2005"
harvested_area_folder <- "Data/2005/harv_2005"
physical_area_folder <- "Data/2005/phy_2005"

# Define the pattern to match (_A for all technologies)
file_pattern <- "_A"

# Function to create a raster stack for each crop
process_crops <- function(yield_folder, harvested_folder, physical_folder, pattern) {
  # List all files in each folder matching the pattern
  yield_files <- list.files(yield_folder, pattern = pattern, full.names = TRUE)
  harvested_files <- list.files(harvested_folder, pattern = pattern, full.names = TRUE)
  physical_files <- list.files(physical_folder, pattern = pattern, full.names = TRUE)
  
  # Extract crop names from file names (e.g., BANP from spam2000V3r107_global_Y_BANP_A.tif)
  crop_names <- unique(gsub(".*_([A-Z]+)_A\\.tif$", "\\1", basename(yield_files)))
  
  # Initialize a list to store stacks for each crop
  crop_stacks <- list()
  
  # Loop through each crop name
  for (crop in crop_names) {
    # Find matching files for the current crop
    yield_file <- grep(paste0("_", crop, "_A\\.tif$"), yield_files, value = TRUE)
    harvested_file <- grep(paste0("_", crop, "_A\\.tif$"), harvested_files, value = TRUE)
    physical_file <- grep(paste0("_", crop, "_A\\.tif$"), physical_files, value = TRUE)
    
    # Check if all files exist for the crop
    if (length(yield_file) == 1 && length(harvested_file) == 1 && length(physical_file) == 1) {
      # Load rasters and create a stack
      yield_raster <- rast(yield_file)
      harvested_raster <- rast(harvested_file)
      physical_raster <- rast(physical_file)
      
      crop_stack <- c(yield_raster, harvested_raster, physical_raster)
      names(crop_stack) <- c("Yield", "Harvested_Area", "Physical_Area")
      
      # Store the stack in the list
      crop_stacks[[crop]] <- crop_stack
    } else {
      warning(paste("Missing files for crop:", crop))
    }
  }
  
  # Return the list of stacks
  return(crop_stacks)
}

# Run the function for the 2005 folders
crop_stacks_2005 <- process_crops(crop_yield_folder, harvested_area_folder, physical_area_folder, file_pattern)

# Save or process the stacks as needed
output_folder_2005 <- "Y:/sparing_sharing/sparing_rproject/stack_trial/2005"
dir.create(output_folder_2005, recursive = TRUE, showWarnings = FALSE)

for (crop in names(crop_stacks_2005)) {
  writeRaster(crop_stacks_2005[[crop]], 
              filename = file.path(output_folder_2005, paste0(crop, "_stack_2005.tif")), 
              overwrite = TRUE)
}

#### Combine all stacks into one
# Define the directory containing the individual stacks for 2005
input_folder_2005 <- output_folder_2005

# List all individual stack files (assuming they have '_stack_2005.tif' in their names)
stack_files_2005 <- list.files(input_folder_2005, pattern = "_stack_2005\\.tif$", full.names = TRUE)

# Initialize an empty list to store all layers
all_layers_2005 <- list()

# Loop through each stack file and extract its layers
for (file in stack_files_2005) {
  # Load the individual stack
  crop_stack <- rast(file)
  
  # Add layers to the list with meaningful names
  crop_name <- gsub("_stack_2005\\.tif$", "", basename(file)) # Extract crop name from the file name
  
  # Rename the layers to include crop name
  names(crop_stack) <- paste(crop_name, names(crop_stack), sep = "_")
  
  # Append the layers to the list
  all_layers_2005 <- c(all_layers_2005, unlist(crop_stack, use.names = FALSE))
}

# Combine all layers into a single stack for 2005
cropstack_2005 <- rast(all_layers_2005)

# Check the combined stack
print(cropstack_2005)
cat("Number of layers in combined stack: ", nlyr(cropstack_2005), "\n")

# Save the combined stack for 2005
output_file_2005 <- "Y:/sparing_sharing/sparing_rproject/stack_trial/cropstack_2005.tif"
writeRaster(cropstack_2005, filename = output_file_2005, overwrite = TRUE)

cat("Combined stack for 2005 saved to: ", output_file_2005, "\n")

# Optional: Verify individual stack layers
for (file in stack_files_2005) {
  crop_stack <- rast(file)
  cat("File:", file, "- Layers:", nlyr(crop_stack), "\n")
}

# Record the system end time
end_time <- Sys.time()
end_time


################# Crop stack 2010 ############################################
# Record the system start time
start_time <- Sys.time()
start_time

# Define the parent directories for each data type (update for 2010)
crop_yield_folder <- "Data/2010/yield_2010"
harvested_area_folder <- "Data/2010/harv_2010"
physical_area_folder <- "Data/2010/phy_2010"

# Define the pattern to match (_A for all technologies)
file_pattern <- "_A"

# Function to create a raster stack for each crop
process_crops <- function(yield_folder, harvested_folder, physical_folder, pattern) {
  # List all files in each folder matching the pattern
  yield_files <- list.files(yield_folder, pattern = pattern, full.names = TRUE)
  harvested_files <- list.files(harvested_folder, pattern = pattern, full.names = TRUE)
  physical_files <- list.files(physical_folder, pattern = pattern, full.names = TRUE)
  
  # Extract crop names from file names (e.g., BANP from spam2000V3r107_global_Y_BANP_A.tif)
  crop_names <- unique(gsub(".*_([A-Z]+)_A\\.tif$", "\\1", basename(yield_files)))
  
  # Initialize a list to store stacks for each crop
  crop_stacks <- list()
  
  # Loop through each crop name
  for (crop in crop_names) {
    # Find matching files for the current crop
    yield_file <- grep(paste0("_", crop, "_A\\.tif$"), yield_files, value = TRUE)
    harvested_file <- grep(paste0("_", crop, "_A\\.tif$"), harvested_files, value = TRUE)
    physical_file <- grep(paste0("_", crop, "_A\\.tif$"), physical_files, value = TRUE)
    
    # Check if all files exist for the crop
    if (length(yield_file) == 1 && length(harvested_file) == 1 && length(physical_file) == 1) {
      # Load rasters and create a stack
      yield_raster <- rast(yield_file)
      harvested_raster <- rast(harvested_file)
      physical_raster <- rast(physical_file)
      
      crop_stack <- c(yield_raster, harvested_raster, physical_raster)
      names(crop_stack) <- c("Yield", "Harvested_Area", "Physical_Area")
      
      # Store the stack in the list
      crop_stacks[[crop]] <- crop_stack
    } else {
      warning(paste("Missing files for crop:", crop))
    }
  }
  
  # Return the list of stacks
  return(crop_stacks)
}

# Run the function for the 2010 folders
crop_stacks_2010 <- process_crops(crop_yield_folder, harvested_area_folder, physical_area_folder, file_pattern)

# Save or process the stacks as needed
output_folder_2010 <- "Y:/sparing_sharing/sparing_rproject/stack_trial/2010"
dir.create(output_folder_2010, recursive = TRUE, showWarnings = FALSE)

for (crop in names(crop_stacks_2010)) {
  writeRaster(crop_stacks_2010[[crop]], 
              filename = file.path(output_folder_2010, paste0(crop, "_stack_2010.tif")), 
              overwrite = TRUE)
}

#### Combine all stacks into one
# Define the directory containing the individual stacks for 2010
input_folder_2010 <- output_folder_2010

# List all individual stack files (assuming they have '_stack_2010.tif' in their names)
stack_files_2010 <- list.files(input_folder_2010, pattern = "_stack_2010\\.tif$", full.names = TRUE)

# Initialize an empty list to store all layers
all_layers_2010 <- list()

# Loop through each stack file and extract its layers
for (file in stack_files_2010) {
  # Load the individual stack
  crop_stack <- rast(file)
  
  # Add layers to the list with meaningful names
  crop_name <- gsub("_stack_2010\\.tif$", "", basename(file)) # Extract crop name from the file name
  
  # Rename the layers to include crop name
  names(crop_stack) <- paste(crop_name, names(crop_stack), sep = "_")
  
  # Append the layers to the list
  all_layers_2010 <- c(all_layers_2010, unlist(crop_stack, use.names = FALSE))
}

# Combine all layers into a single stack for 2010
cropstack_2010 <- rast(all_layers_2010)

# Check the combined stack
print(cropstack_2010)
cat("Number of layers in combined stack: ", nlyr(cropstack_2010), "\n")

# Save the combined stack for 2010
output_file_2010 <- "Y:/sparing_sharing/sparing_rproject/stack_trial/cropstack_2010.tif"
writeRaster(cropstack_2010, filename = output_file_2010, overwrite = TRUE)

cat("Combined stack for 2010 saved to: ", output_file_2010, "\n")

# Optional: Verify individual stack layers
for (file in stack_files_2010) {
  crop_stack <- rast(file)
  cat("File:", file, "- Layers:", nlyr(crop_stack), "\n")
}

# Record the system end time
end_time <- Sys.time()
end_time

################# Crop stack 2020 ############################################
# Record the system start time
start_time <- Sys.time()
start_time

# Define the parent directories for each data type (update for 2020)
crop_yield_folder <- "Data/2020/yield_2020"
harvested_area_folder <- "Data/2020/harv_2020"
physical_area_folder <- "Data/2020/phy_2020"

# Define the pattern to match (_A for all technologies)
file_pattern <- "_A"

# Function to create a raster stack for each crop
process_crops <- function(yield_folder, harvested_folder, physical_folder, pattern) {
  # List all files in each folder matching the pattern
  yield_files <- list.files(yield_folder, pattern = pattern, full.names = TRUE)
  harvested_files <- list.files(harvested_folder, pattern = pattern, full.names = TRUE)
  physical_files <- list.files(physical_folder, pattern = pattern, full.names = TRUE)
  
  # Extract crop names from file names (e.g., BANP from spam2000V3r107_global_Y_BANP_A.tif)
  crop_names <- unique(gsub(".*_([A-Z]+)_A\\.tif$", "\\1", basename(yield_files)))
  
  # Initialize a list to store stacks for each crop
  crop_stacks <- list()
  
  # Loop through each crop name
  for (crop in crop_names) {
    # Find matching files for the current crop
    yield_file <- grep(paste0("_", crop, "_A\\.tif$"), yield_files, value = TRUE)
    harvested_file <- grep(paste0("_", crop, "_A\\.tif$"), harvested_files, value = TRUE)
    physical_file <- grep(paste0("_", crop, "_A\\.tif$"), physical_files, value = TRUE)
    
    # Check if all files exist for the crop
    if (length(yield_file) == 1 && length(harvested_file) == 1 && length(physical_file) == 1) {
      # Load rasters and create a stack
      yield_raster <- rast(yield_file)
      harvested_raster <- rast(harvested_file)
      physical_raster <- rast(physical_file)
      
      crop_stack <- c(yield_raster, harvested_raster, physical_raster)
      names(crop_stack) <- c("Yield", "Harvested_Area", "Physical_Area")
      
      # Store the stack in the list
      crop_stacks[[crop]] <- crop_stack
    } else {
      warning(paste("Missing files for crop:", crop))
    }
  }
  
  # Return the list of stacks
  return(crop_stacks)
}

# Run the function for the 2020 folders
crop_stacks_2020 <- process_crops(crop_yield_folder, harvested_area_folder, physical_area_folder, file_pattern)

# Save or process the stacks as needed
output_folder_2020 <- "Y:/sparing_sharing/sparing_rproject/stack_trial/2020"
dir.create(output_folder_2020, recursive = TRUE, showWarnings = FALSE)

for (crop in names(crop_stacks_2020)) {
  writeRaster(crop_stacks_2020[[crop]], 
              filename = file.path(output_folder_2020, paste0(crop, "_stack_2020.tif")), 
              overwrite = TRUE)
}

#### Combine all stacks into one
# Define the directory containing the individual stacks for 2020
input_folder_2020 <- output_folder_2020

# List all individual stack files (assuming they have '_stack_2020.tif' in their names)
stack_files_2020 <- list.files(input_folder_2020, pattern = "_stack_2020\\.tif$", full.names = TRUE)

# Initialize an empty list to store all layers
all_layers_2020 <- list()

# Loop through each stack file and extract its layers
for (file in stack_files_2020) {
  # Load the individual stack
  crop_stack <- rast(file)
  
  # Add layers to the list with meaningful names
  crop_name <- gsub("_stack_2020\\.tif$", "", basename(file)) # Extract crop name from the file name
  
  # Rename the layers to include crop name
  names(crop_stack) <- paste(crop_name, names(crop_stack), sep = "_")
  
  # Append the layers to the list
  all_layers_2020 <- c(all_layers_2020, unlist(crop_stack, use.names = FALSE))
}

# Combine all layers into a single stack for 2020
cropstack_2020 <- rast(all_layers_2020)

# Check the combined stack
print(cropstack_2020)
cat("Number of layers in combined stack: ", nlyr(cropstack_2020), "\n")

# Save the combined stack for 2020
output_file_2020 <- "Y:/sparing_sharing/sparing_rproject/stack_trial/cropstack_2020.tif"
writeRaster(cropstack_2020, filename = output_file_2020, overwrite = TRUE)

cat("Combined stack for 2020 saved to: ", output_file_2020, "\n")

# Optional: Verify individual stack layers
for (file in stack_files_2020) {
  crop_stack <- rast(file)
  cat("File:", file, "- Layers:", nlyr(crop_stack), "\n")
}

# Record the system end time
end_time <- Sys.time()
end_time










