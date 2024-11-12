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
psd_data <- soildata[, .(id, coord_x, coord_y, profund_sup, profund_inf, argila, silte, areia)]
psd_data[, depth_mid := profund_sup + (profund_inf - profund_sup) / 2, by = .I]
psd_data[, has_psd := is.na(argila) + is.na(silte) + is.na(areia) == 0, by = id]
psd_data[, has_coord := is.na(coord_x) + is.na(coord_y) == 0, by = id]
print(psd_data)

# Compute the clay content at 5, 15, and 25 cm depth
psd_data <- psd_data[
  has_psd == TRUE & has_coord == TRUE,
  .(
    coord_x = mean(coord_x, na.rm = TRUE),
    coord_y = mean(coord_y, na.rm = TRUE),
    clay0_10 = round(spline(x = depth_mid, y = argila, xout = 5, method = "natural")$y),
    silt0_10 = round(spline(x = depth_mid, y = silte, xout = 5, method = "natural")$y),
    sand0_10 = round(spline(x = depth_mid, y = areia, xout = 5, method = "natural")$y),
    clay10_20 = round(spline(x = depth_mid, y = argila, xout = 15, method = "natural")$y),
    silt10_20 = round(spline(x = depth_mid, y = silte, xout = 15, method = "natural")$y),
    sand10_20 = round(spline(x = depth_mid, y = areia, xout = 15, method = "natural")$y),
    clay20_30 = round(spline(x = depth_mid, y = argila, xout = 25, method = "natural")$y),
    silt20_30 = round(spline(x = depth_mid, y = silte, xout = 25, method = "natural")$y),
    sand20_30 = round(spline(x = depth_mid, y = areia, xout = 25, method = "natural")$y)
  ),
  by = id
]

soildata[id %in% psd_data[clay10_20 < 0, id]]

# Check the sum of the particle size distribution
psd_data[, psd0_10 := clay0_10 + silt0_10 + sand0_10]
psd_data[, psd10_20 := clay10_20 + silt10_20 + sand10_20]
psd_data[, psd20_30 := clay20_30 + silt20_30 + sand20_30]
summary(psd_data[, .(psd0_10, psd10_20, psd20_30)])

# Correct the particle size distribution to sum 100%
# 0-10 cm depth
psd_data[, clay0_10 := round(clay0_10 / psd0_10 * 100)]
psd_data[, sand0_10 := round(sand0_10 / psd0_10 * 100)]
psd_data[, silt0_10 := 100 - clay0_10 - sand0_10]

# 10-20 cm depth
psd_data[, clay10_20 := round(clay10_20 / psd10_20 * 100)]
psd_data[, sand10_20 := round(sand10_20 / psd10_20 * 100)]
psd_data[, silt10_20 := 100 - clay10_20 - sand10_20]

# 20-30 cm depth
psd_data[, clay20_30 := round(clay20_30 / psd20_30 * 100)]
psd_data[, sand20_30 := round(sand20_30 / psd20_30 * 100)]
psd_data[, silt20_30 := 100 - clay20_30 - sand20_30]



summary(psd_data)

# Plot using mapview
if (FALSE) {
  psd_data_sf <- sf::st_as_sf(psd_data, coords = c("coord_x", "coord_y"), crs = 4326)
  mapview::mapview(psd_data_sf, zcol = "clay0_10")
}

