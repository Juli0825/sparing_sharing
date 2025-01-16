

# For individual harvested area files
harvested_area_folder <- "Data/2000/harv_2000"
output_file <- "Y:/sparing_sharing/sparing_rproject/harvested_area_2000.tif"
total_harvested_area_2000 <- sum_harvested_area(harvested_area_folder, output_file)

# For visualization
plot(total_harvested_area_2000, main = "Total Harvested Area 2000")