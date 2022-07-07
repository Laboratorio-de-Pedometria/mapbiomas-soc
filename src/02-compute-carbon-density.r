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

# Calculate organic carbon stock (kg/m^2) per layer
# carbono / 1000: kg/kg
# espessura / 100: m
# dsi * 1000: kg/m^3
# terrafina / 1000: 1
febr_data[, cos_estoque_kgm2 := (carbono / 1000) * (espessura / 100) * (dsi * 1000) * (terrafina / 1000)]
febr_data[, cos_estoque_kgm2 := round(cos_estoque_kgm2, 2)]
hist(febr_data[, cos_estoque_kgm2])

# Prepare time coordinate
febr_data[, evento_ano := as.integer(format(as.Date(observacao_data, format = "%Y-%m-%d"), "%Y"))]

# Create spatial object
topsoil_idx <- febr_data[["profund_sup"]] == 0
has_stock_idx <- !is.na(febr_data[["cos_estoque_kgm2"]])
has_coord_idx <- (!is.na(febr_data[["coord_x"]]) + !is.na(febr_data[["coord_y"]]) == 2)
has_time <- !is.na(febr_data[["evento_ano"]])
sf_idx <- !is.na(febr_data[, coord_x]) & !is.na(febr_data[, coord_y])
topsoil_has_stock_coord_idx <- which((topsoil_idx + has_stock_idx + sf_idx + has_time) == 4)

# Write table to disk with events missing date
colnames(febr_data)
no_time_coord <- febr_data[!has_time & topsoil_idx,
  c("dataset_id", "dataset_titulo", "estado_id", "municipio_id", "observacao_id", "observacao_data")]
no_time_coord[, dataset_id := paste0("https://www.pedometria.org/febr/", dataset_id, "/")]
write.table(
  x = no_time_coord,
  file = "mapbiomas-solos/data/no-time-coord.csv",
  sep = "\t", row.names = FALSE)

# Ignore event date
febr_data[, evento_ano := ifelse(evento_ano < 1985, 1985, evento_ano)]

# Write object to disk
colnames(febr_data)
vars_idx <- c("dataset_id", "observacao_id", "camada_id", "coord_x", "coord_y", "evento_ano", "cos_estoque_kgm2")
febr_data[, coord_x := round(as.numeric(coord_x), 8)]
febr_data[, coord_y := round(as.numeric(coord_y), 8)]
write.table(febr_data[topsoil_has_stock_coord_idx, ..vars_idx],
  file = "mapbiomas-solos/res/pontos-estoque.csv",
  row.names = FALSE, sep = ",", dec = ".")
