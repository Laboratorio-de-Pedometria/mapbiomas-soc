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

febr_data[, estoque := round(carbono * (espessura / 100) + dsi * (terrafina / 1000), 2)]
febr_data[, log_estoque := log1p(estoque)]
hist(febr_data[, log_estoque])

# Create spatial object
topsoil_idx <- febr_data[["profund_sup"]] == 0
has_stock_idx <- !is.na(febr_data[["estoque"]])
has_coord_idx <- (!is.na(febr_data[["coord_x"]]) + !is.na(febr_data[["coord_y"]]) == 2)
sf_idx <- !is.na(febr_data[, coord_x]) & !is.na(febr_data[, coord_y])
topsoil_has_stock_coord_idx <- which((topsoil_idx + has_stock_idx + sf_idx) == 3)

# Write object to disk
colnames(febr_data)
vars_idx <- c("dataset_id", "observacao_id", "camada_id", "coord_x", "coord_y", "estoque")
febr_data[, coord_x := round(as.numeric(coord_x), 8)]
febr_data[, coord_y := round(as.numeric(coord_y), 8)]
  write.table(febr_data[topsoil_has_stock_coord_idx, ..vars_idx],
    file = "mapbiomas-solos/res/pontos-estoque.csv",
    row.names = FALSE, sep = ",", dec = ".")



