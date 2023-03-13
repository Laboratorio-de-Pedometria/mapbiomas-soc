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

# Prepare for sampling on GEE
n_points <- nrow(sf_febr_data)
n_lags <- ceiling(n_points / 5000)
lag_width <- ceiling(n_points / n_lags)
lags <- rep(1:n_lags, each = lag_width)
lags <- lags[1:n_points]


# MapBiomas Land Cover Land Use
mapbiomas <- list()
x <- rgee::ee$ImageCollection("projects/mapbiomas-workspace/public/collection7/mapbiomas_collection70_integration_v2")
for (i in 1:n_lags) {
  bdod_mean[[i]] <- rgee::ee_extract(
    x = rgee::ee$ImageCollection$filterDate(x, paste0(year, "-01-01"), paste0(year, "-12-31")),
    y = sf_febr_data[lags == i, ],
    scale = 30,
    fun = ee$Reducer$first()
  )
}
