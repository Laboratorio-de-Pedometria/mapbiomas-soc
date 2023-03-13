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
  rgee::ee_install()
}
if (!require("geobr")) {
  install.packages("geobr")
}

# Missingness in attribute
mia <-
  function(x) {
    is_na <- is.na(x)
    Xplus <- as.numeric(x)
    Xplus[is_na] <- as.numeric(+Inf)
    Xminus <- as.numeric(x)
    Xminus[is_na] <- as.numeric(-Inf)
    Xna <- rep("ISNOTNA", length(x))
    Xna[is_na] <- as.character("ISNA")
    out <- data.frame(Xplus = Xplus, Xminus = Xminus, Xna = Xna)
    return(out)
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
plot(sf_febr_data, add = TRUE, col = "black", cex = 0.5)

# Prepare for sampling on GEE
n_points <- nrow(sf_febr_data)
n_lags <- ceiling(n_points / 5000)
lag_width <- ceiling(n_points / n_lags)
lags <- rep(1:n_lags, each = lag_width)
lags <- lags[1:n_points]

# Soil Grids 250m v2.0: bdod_mean
bdod_mean <- list()
for (i in 1:n_lags) {
  bdod_mean[[i]] <- rgee::ee_extract(
    x = ee$Image("projects/soilgrids-isric/bdod_mean"),
    y = sf_febr_data[lags == i, ], scale = 250
  )
}
bdod_mean <- data.table::rbindlist(bdod_mean)
bdod_mean[, c("bdod_30.60cm_mean", "bdod_60.100cm_mean", "bdod_100.200cm_mean") := NULL]
bdod_mean[, c("dataset_id", "id") := NULL]

# Soil Grids 250m v2.0: clay_mean
clay_mean <- list()
for (i in 1:n_lags) {
  clay_mean[[i]] <- rgee::ee_extract(
    x = ee$Image("projects/soilgrids-isric/clay_mean"),
    y = sf_febr_data[lags == i, ], scale = 250
  )
}
clay_mean <- data.table::rbindlist(clay_mean)
clay_mean[, c("clay_30.60cm_mean", "clay_60.100cm_mean", "clay_100.200cm_mean") := NULL]
clay_mean[, c("dataset_id", "id") := NULL]

# Soil Grids 250m v2.0: sand_mean
sand_mean <- list()
for (i in 1:n_lags) {
  sand_mean[[i]] <- rgee::ee_extract(
    x = ee$Image("projects/soilgrids-isric/sand_mean"),
    y = sf_febr_data[lags == i, ], scale = 250
  )
}
sand_mean <- data.table::rbindlist(sand_mean)
sand_mean[, c("sand_30.60cm_mean", "sand_60.100cm_mean", "sand_100.200cm_mean") := NULL]
sand_mean[, c("dataset_id", "id") := NULL]

# Soil Grids 250m v2.0: soc_mean
soc_mean <- list()
for (i in 1:n_lags) {
  soc_mean[[i]] <- rgee::ee_extract(
    x = ee$Image("projects/soilgrids-isric/soc_mean"),
    y = sf_febr_data[lags == i, ], scale = 250
  )
}
soc_mean <- data.table::rbindlist(soc_mean)
soc_mean[, c("soc_30.60cm_mean", "soc_60.100cm_mean", "soc_100.200cm_mean") := NULL]
soc_mean[, c("dataset_id", "id") := NULL]

# Collate data from SoilGrids
SoilGrids <- cbind(bdod_mean, clay_mean, sand_mean, soc_mean)
colnames(SoilGrids) <- gsub("_mean", "", colnames(SoilGrids), fixed = TRUE)
colnames(SoilGrids) <- gsub("_", "", colnames(SoilGrids), fixed = TRUE)
sf_febr_data <- cbind(sf_febr_data, SoilGrids)

# Merge data
sp_febr_data <- merge(sp_febr_data, sf_febr_data, by = c("dataset_id", "id"))
sp_febr_data[, geometry := NULL]
febr_data <- data.table::rbindlist(list(sp_febr_data, febr_data[is_na_coordinates, ]),
  use.names = TRUE, fill = TRUE)

# Impute missing
which_cols <- which(grepl("([0-9])\\.([0-9])", colnames(febr_data), perl = TRUE))
for (i in which_cols) {
  y <- mia(febr_data[[i]])
  colnames(y) <- gsub("X", toupper(colnames(febr_data)[i]), colnames(y))
  febr_data <- cbind(febr_data, y)
}
febr_data[, colnames(febr_data)[which_cols] := NULL]

# Write data to disk
data.table::fwrite(febr_data, "mapbiomas-solos/data/04-febr-data.txt", sep = "\t", dec = ",")
