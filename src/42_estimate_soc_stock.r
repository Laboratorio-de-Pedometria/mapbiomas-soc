# title: SoilData - Soil Organic Carbon Stock
# subtitle: Estimate SOC stock
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2024 CC-BY
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("randomForestSRC")) {
  install.packages("randomForestSRC")
}

# Source helper functions
source("src/00_helper_functions.r")

# Read data processed in the previous script
soildata <- data.table::fread("data/41_soildata_soc.txt", sep = "\t")
summary_soildata(soildata)
# Layers: 10378
# Events: 10378
# Georeferenced events: 8311

