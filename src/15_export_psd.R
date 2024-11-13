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
data.table::setnames(psd_data, old = "argila", new = "clay")
data.table::setnames(psd_data, old = "silte", new = "silt")
data.table::setnames(psd_data, old = "areia", new = "sand")
data.table::setcolorder(psd_data, c("id", "coord_x", "coord_y", "depth", "clay", "silt", "sand"))
summary_soildata(psd_data)
# Layers: 20979
# Events: 11473
# Georeferenced events: 11473

# Plot using mapview
if (FALSE) {
  psd_data_sf <- sf::st_as_sf(psd_data, coords = c("coord_x", "coord_y"), crs = 4326)
  mapview::mapview(psd_data_sf, zcol = "argila")
}

# Write data to disk
file_path <- "~/Insync/MapBiomas Solo/Trainning samples/"
file_path <- paste0(file_path, format(Sys.time(), "%Y-%m-%d"), "-clay-silt-sand-percent.csv")
file_path <- path.expand(file_path)
data.table::fwrite(psd_data, file_path)
