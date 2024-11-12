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

# We use natural splines to estimate the particle size distribution (PSD) at three target depths for
# each event. The target depths are 0-10 cm, 10-20 cm, and 20-30 cm.
clay_data <- soildata[, .(id, coord_x, coord_y, profund_inf, profund_sup, argila, silte, areia)]
clay_data[, depth_mid := mean(c(profund_inf, profund_sup)), by = .I]
clay_data[, clay0_10 := spline(x = depth_mid, y = argila, xout = 5, method = "natural")$y, by = id]
clay_data[, clay10_20 := spline(x = depth_mid, y = argila, xout = 15, method = "natural")$y, by = id]
clay_data[, clay20_30 := spline(x = depth_mid, y = argila, xout = 25, method = "natural")$y, by = id]

