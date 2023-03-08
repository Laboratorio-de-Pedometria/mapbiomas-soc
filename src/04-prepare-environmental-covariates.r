rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("sf")) {
  install.packages("sf")
}
if (!require("rgee")) {
  install.packages("rgee", dependencies = TRUE)
}
if (!require("geobr")) {
  install.packages("geobr")
}

# Initialize Google Earth Engine
rgee::ee_Initialize()

# Ler dados do disco
febr_data <- data.table::fread("mapbiomas-solos/data/03-febr-data.txt", dec = ",", sep = "\t")
colnames(febr_data)

# Create spatial object
# First filter out those samples without coordinates
# Also keep a single sample per soil profile
is_na_coordinates <- is.na(febr_data[, coord_x]) | is.na(febr_data[, coord_y])
sp_febr_data <- febr_data[!is_na_coordinates, ]
sf_febr_data <- sp_febr_data[,
  data.frame(coord_x = mean(coord_x), coord_y = mean(coord_y)),
  by = c("dataset_id", "id")
]
sf_febr_data <- sf::st_as_sf(sf_febr_data, coords = c("coord_x", "coord_y"), crs = 4326)
brazil <- geobr::read_country()
x11()
plot(brazil, reset = FALSE)
plot(sf_febr_data, add = TRUE)

# Prepare for sampling on GEE
n_points <- nrow(sf_febr_data)
n_lags <- ceiling(n_points / 5000)
lag_width <- ceiling(n_points / n_lags)
lags <- rep(1:n_lags, each = lag_width)
lags <- lags[1:n_points]

# Soil Grids 250m v2.0
bdod_mean <- list()
for (i in 1:n_lags) {
  bdod_mean[[i]] <- rgee::ee_extract(
    x = ee$Image("projects/soilgrids-isric/bdod_mean"),
    y = sf_febr_data[lags == i, ], scale = 250, quiet = FALSE
  )
}
bdod_mean <- data.table::rbindlist(bdod_mean)
sf_febr_data <- cbind(sf_febr_data, bdod_mean[, 4:ncol(bdod_mean)])
sf_febr_data

tmp <- merge(sf_febr_data, sp_febr_data, by = c("dataset_id", "id"))

