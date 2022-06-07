rm(list = ls())

# Install and load required packages
if (!require("febr")) {
  install.packages("febr", dependencies = TRUE)
}
if (!require("sf")) {
  install.packages("sf", dependencies = TRUE)
}
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer", dependencies = TRUE)
}

# Read data from disk
febr_data <- data.table::fread("mapbiomas-solos/data/febr-data.txt", dec = ",", sep = "\t")
colnames(febr_data)

# Compute carbon stock per layer
febr_data[, estoque := carbono * (espessura / 100) + dsi * (terrafina / 1000)]
hist(febr_data[, estoque])

# Create spatial object
sf_idx <- !is.na(febr_data[, coord_x]) & !is.na(febr_data[, coord_y])
febr_data_sf <- sf::st_as_sf(febr_data[sf_idx, ], coords = c("coord_x", "coord_y"), crs = 4623)
topsoil_idx <- febr_data_sf[["profund_sup"]] == 0
plot(febr_data_sf[topsoil_idx, "estoque"], pch = 20, cex = 0.5)