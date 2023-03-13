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
}
rgee::ee_Initialize()

# Read data from disk
febr_data <- data.table::fread("mapbiomas-solos/data/06-febr-data.txt", dec = ",", sep = "\t")
colnames(febr_data)

febr_data[data_coleta_ano < 1985, data_coleta_ano := 1985]

# Create spatial object
febr_data <- sf::st_as_sf(febr_data, coords = c("coord_x", "coord_y"), crs = 4326)
brazil <- geobr::read_country()
x11()
plot(brazil, reset = FALSE)
plot(febr_data, add = TRUE, col = "black", cex = 0.5)




# Prepare for sampling on GEE
n_points <- nrow(febr_data)
n_lags <- ceiling(n_points / 5000)
lag_width <- ceiling(n_points / n_lags)
lags <- rep(1:n_lags, each = lag_width)
lags <- lags[1:n_points]


# MapBiomas Land Cover Land Use
mapbiomas <- list()
time_steps <- sort(unique(febr_data[["data_coleta_ano"]]))
gee_path <- "projects/mapbiomas-workspace/public/collection7/mapbiomas_collection70_integration_v2"
gee_path <- rgee::ee$Image(gee_path)
for (year in time_steps) {
  idx <- febr_data[["data_coleta_ano"]] == year
  mapbiomas[[year]] <- rgee::ee_extract(
    x = rgee::ee$Image$filterDate(gee_path,
      paste0(year, "-01-01"), paste0(year, "-12-31")),
    y = febr_data[idx, ],
    scale = 30,
    fun = ee$Reducer$first()
  )
}




# Escrever dados de estoque de carbono no solo em disco
febr_data[, coord_x := round(as.numeric(coord_x), 8)]
febr_data[, coord_y := round(as.numeric(coord_y), 8)]
write.table(febr_data, file = paste0("mapbiomas-solos/res/pontos-estoque.csv"),
  row.names = FALSE, sep = ",", dec = ".")