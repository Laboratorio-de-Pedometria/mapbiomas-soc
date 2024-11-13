# title: SoilData - Soil Organic Carbon Stock
# subtitle: Export Particle Size Distribution
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2024 CC-BY
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}

# Source helper functions
source("src/00_helper_functions.r")

# Read SoilData data processed in the previous script
soildata <- data.table::fread("data/14_soildata_soc.txt", sep = "\t")
summary_soildata(soildata)
# Layers: 29465
# Events: 15499
# Georeferenced events: 13151

# Create a data.table with the particle size distribution
psd_data <- soildata[
  !is.na(argila) & !is.na(silte) & !is.na(areia) & !is.na(coord_x) & !is.na(coord_y),
  .(id, coord_x, coord_y, profund_sup, profund_inf, argila, silte, areia)
]
psd_data[, depth := profund_sup + (profund_inf - profund_sup) / 2, by = .I]
psd_data[, profund_sup := NULL]
psd_data[, profund_inf := NULL]

# Compute additive log ratio transformation
psd_data[, log_clay_sand := log(argila / areia)]
psd_data[, log_silt_sand := log(silte / areia)]
summary(psd_data[, .(log_clay_sand, log_silt_sand)])

# Remove original columns
psd_data[, c("argila", "silte", "areia") := NULL]

# Rename columns
data.table::setcolorder(
  psd_data,
  c("id", "coord_x", "coord_y", "depth", "log_clay_sand", "log_silt_sand")
)
summary_soildata(psd_data)
# Layers: 20979
# Events: 11473
# Georeferenced events: 11473

# Plot using mapview
if (FALSE) {
  psd_data_sf <- sf::st_as_sf(psd_data, coords = c("coord_x", "coord_y"), crs = 4326)
  mapview::mapview(psd_data_sf, zcol = "log_clay_sand")
}

# Write data to disk
folder_path <- "~/Insync/MapBiomas Solo/Trainning samples/"
file_name <- "-clay-silt-sand-percent.csv"
# List existing files in the folder_path and get the last one. Then read it.
existing_files <- list.files(path = folder_path, pattern = file_name)
last_file <- existing_files[length(existing_files)]
last_psd_data <- data.table::fread(paste0(folder_path, last_file))
# Check if last_psd_data == psd_data. If not, write psd_data to disk.
if (!identical(last_psd_data, psd_data)) {
  file_path <- paste0(folder_path, format(Sys.time(), "%Y-%m-%d"), file_name)
  file_path <- path.expand(file_path)
  data.table::fwrite(psd_data, file_path)
}
