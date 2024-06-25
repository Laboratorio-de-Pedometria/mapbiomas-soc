# MapBiomas Soil (beta): Script 01a. Process FEBR data - Rondônia
# Alessandro Samuel-Rosa & Taciara Zborowski Horst
# 2023 CC-BY
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
if (!require("febr")) {
  if (!require(remotes)) {
    install.packages(pkgs = "remotes")
  }
  remotes::install_github(repo = "laboratorio-de-pedometria/febr-package")
}

# Zoneamento Socioeconômico-Ecológico do Estado de Rondônia (ctb0033 and ctb0034)
# Download current version from FEBR: events
# ctb0033
event33 <- febr::observation("ctb0033", "all")
event33 <- data.table::as.data.table(event33)
event33[, data_coleta := as.Date(data_coleta, origin = "1899-12-30")]
# ctb0034
event34 <- febr::observation("ctb0034", "all")
event34 <- data.table::as.data.table(event34)
event34[, dataset_id34 := dataset_id]
event34[, dataset_id := NULL]
event34[, data_coleta := as.Date(data_coleta, origin = "1899-12-30")]
# event34[, data_coleta := NULL]
sapply(list(event33 = event33, event34 = event34), nrow) # 2998 and 107 events
eventRO <- merge(event33, event34, all = TRUE)
nrow(eventRO) # 2999 events after merge
eventRO[, dataset_id := "ctb0033"]
eventRO[, coord_datum_epsg := NULL]
eventRO[, coord_datum_epsg := "EPSG:4326"]
new_names <- c(
  evento_id_febr = "observacao_id",
  coord_longitude = "coord_x",
  coord_latitude = "coord_y",
  coord_municipio_nome = "municipio_id",
  coord_estado_sigla = "estado_id"
)
data.table::setnames(eventRO, old = names(new_names), new = new_names, skip_absent = TRUE)
eventRO[, estado_id := "RO"]
cols <- intersect(names(eventRO), tolower(names(eventRO)))
eventRO <- eventRO[, ..cols]
eventRO[, data_coleta_ano := as.integer(format(data_coleta, "%Y"))]
nrow(eventRO[is.na(data_coleta_ano), ]) # 87 events missing the sampling date
eventRO[is.na(data_coleta_ano), data_coleta_ano := 1996]
if (FALSE) {
  x11()
  plot(eventRO[, c("coord_x", "coord_y")])
}
str(eventRO)

# Download current version from FEBR: layers
# ctb0033
layer33 <- febr::layer("ctb0033", "all")
layer33 <- data.table::as.data.table(layer33)
layer33[, camada_id_sisb := NULL]
# ctb0034
layer34 <- febr::layer("ctb0034", "all")
layer34 <- data.table::as.data.table(layer34)
layer34[, dataset_id34 := dataset_id]
layer34[, dataset_id := NULL]
layer34[, camada_id_febr := camada_id_alt]
sapply(list(layer33, layer34), nrow) # 10 779 and 419 layers
# Merge layers from ctb0033 and ctb0034
layerRO <- merge(layer33, layer34,
  by = c("evento_id_febr", "camada_id_febr"),
  suffixes = c("", ".IGNORE"),
  all = TRUE
)
nrow(layerRO) # 10 785 layers after merge
layerRO[, dataset_id := "ctb0033"]
colnames(layerRO)
new_names <- c(
  evento_id_febr = "observacao_id",
  ph_2.5h2o_eletrodo = "ph",
  carbono_xxx_xxx = "carbono",
  areia.05mm2_xxx_xxx = "areia",
  silte.002mm.05_xxx_xxx = "silte",
  argila0mm.002_xxx_xxx = "argila",
  terrafina_xxx_xxx = "terrafina",
  ctc_soma_calc = "ctc",
  densidade_solo_xxx = "dsi"
)
data.table::setnames(layerRO, old = names(new_names), new = new_names)
cols <- intersect(names(layerRO), tolower(names(layerRO)))
layerRO <- layerRO[, ..cols]
layerRO[, dataset_id := NULL]
layerRO[, dataset_id34 := NULL]

# Merge events and layers
rondonia <- merge(eventRO, layerRO, all = TRUE)
nrow(rondonia) # 10 789 layers

# Standardize measurement units
rondonia[, areia := areia * 10]
rondonia[, argila := argila * 10]
rondonia[, silte := silte * 10]
rondonia[, terrafina := terrafina * 10]
rondonia[, carbono := carbono * 10]

# Deal with the identification of events containing duplicated layers
# These are extra samples for soil fertility assessment collected nearby the soil profile
rondonia[, EXTRA := duplicated(profund_sup), by = observacao_id]
nrow(rondonia[EXTRA == TRUE, ]) # 63 duplicated layers
nrow(unique(rondonia[EXTRA == TRUE, "observacao_id"])) # 24 events with duplicated layers
rondonia[EXTRA == TRUE, observacao_id := paste0(observacao_id, camada_id_febr)]
rondonia[, id := paste0(dataset_id, "-", observacao_id)]

# Add random perturbation to the coordinates of extra samples
# Use sf::st_jitter() with amount = 200 m, where runif(1, -amount, amount)
amount <- 200
extra_coords <- rondonia[EXTRA == TRUE & !is.na(coord_x), c("id", "coord_x", "coord_y")]
extra_coords <- sf::st_as_sf(extra_coords, coords = c("coord_x", "coord_y"), crs = 4326)
extra_coords <- sf::st_transform(extra_coords, crs = 32720)
set.seed(32720)
extra_coords <- sf::st_jitter(extra_coords, amount = amount)
extra_coords <- sf::st_transform(extra_coords, crs = 4326)
extra_coords <- sf::st_coordinates(extra_coords)
rondonia[EXTRA == TRUE & !is.na(coord_x), coord_x := extra_coords[, "X"]]
rondonia[EXTRA == TRUE & !is.na(coord_x), coord_y := extra_coords[, "Y"]]
rondonia[, EXTRA := NULL]
nrow(rondonia) # 10 789 layers
nrow(unique(rondonia[, "id"])) # 3061 events

# Read FEBR data processed in the previous script
febr_data <- data.table::fread("mapbiomas-solo/data/01a-febr-data.txt", dec = ",", sep = "\t")
febr_data[, coord_datum_epsg := 4326]

# Merge data from Rondônia with the FEBR snapshot
# First remove existing data from Rondônia (morphological descriptions)
length(unique(febr_data[, id])) # 14 043 events
febr_data <- febr_data[dataset_id != "ctb0032", ]
length(unique(febr_data[, id])) # 11 129 events
col_ro <- intersect(names(febr_data), names(rondonia))
febr_data <- data.table::rbindlist(list(febr_data, rondonia[, ..col_ro]), fill = TRUE)
length(unique(febr_data[, id])) # 14 190 events
nrow(febr_data) # 50 385 layers

# Write data to disk
data.table::fwrite(febr_data, "mapbiomas-solo/data/01b-febr-data.txt", sep = "\t", dec = ",")
