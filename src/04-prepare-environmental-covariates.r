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

# Initialize Google Earth Engine
rgee::ee_Initialize()

# Ler dados do disco
febr_data <- data.table::fread("mapbiomas-solos/data/03-febr-data.txt", dec = ",", sep = "\t")
colnames(febr_data)

# Create spatial object
# First filter out those samples without coordinates
# Also keep a single sample per soil profile
is_na_coordinates <- is.na(febr_data[, coord_x]) | is.na(febr_data[, coord_y])
sf_febr_data <- febr_data[!is_na_coordinates,
  data.frame(coord_x = mean(coord_x), coord_y = mean(coord_y)),
  by = c("dataset_id", "id")
]
sf_febr_data <- sf::st_as_sf(sf_febr_data, coords = c("coord_x", "coord_y"), crs = 4326)

# Soil Grids 250m v2.0
bdod_mean <- rgee::ee_extract(
  x = ee$Image("projects/soilgrids-isric/bdod_mean"),
  y = sf_febr_data[1:5000, ], scale = 250
)
# sf_febr_data <- cbind(sf_febr_data, bdod_mean)

