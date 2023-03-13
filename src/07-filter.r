rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("sf")) {
  install.packages("sf")
}
if (!require("geobr")) {
  install.packages("geobr")
}
if (!require("rgee")) {
  install.packages("rgee", dependencies = TRUE)
  rgee::ee_install()
}
rgee::ee_Initialize()

# Read data from disk
febr_data <- data.table::fread("mapbiomas-solos/data/06-febr-data.txt", dec = ",", sep = "\t")
colnames(febr_data)

# Create spatial object
sf_febr_data <- sf::st_as_sf(febr_data, coords = c("coord_x", "coord_y"), crs = 4326)
brazil <- geobr::read_country()
x11()
plot(brazil, reset = FALSE)
plot(sf_febr_data, add = TRUE, col = "black", cex = 0.5)

# MapBiomas Land Cover Land Use
n_years <- unique(febr_data[])
mapbiomas <- list()
for (i in 1:n_lags) {
  bdod_mean[[i]] <- rgee::ee_extract(
    x = ee$Image("projects/soilgrids-isric/bdod_mean"),
    y = sf_febr_data[lags == i, ], scale = 250
  )
}

