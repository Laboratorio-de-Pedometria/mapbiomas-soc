# 02. MERGE EXTERNAL DATA SETS #####################################################################
# SUMMARY
# Several data sets were selected to enter the FEBR during the last months. However, these data sets
# have not been modelled according to the data model used in the FEBR. The reason for this is that
# we were looking for data that could be readily used to model the spatio-temporal variation of soil
# organic carbon stocks in Brazil. In other words, our focus was on simply gathering data that could
# help improve the predictions, thus focusing on key soil variables. The main data source in these
# phase was the National Forest Inventory (IFN) (see URL below). We also ingested data from a
# private natural reserve in the Pantanal biome (SESC Pantanal). Due to limitations of resources,
# any differences in laboratory methods were ignored during data processing and the new data was
# merged with the existing data as is. The only exception is the standardization of the coordinate
# reference system, with EPSG:4326 used as target.
# During data processing, we noticed that 32 samples coming from the latest (2021) FEBR snapshot we
# missing data on the concentration of the fine earth fraction. After checking the horizon/layer
# designation, we attributed a concentration of fine earth of 1000 g/kg.
# We also noticed that some events were duplicated. These were events with the same ID but different
# spatial or temporal coordinates. We identify them computing the standard deviation of the
# coordinates of each event: for non-duplicated events, the standard deviation should be zero.
# IFN: https://snif.florestal.gov.br/pt-br/inventario-florestal-nacional-ifn/ifn-dados-abertos
# KEY RESULTS
# We started with 14 043 events (50 470 layers) in the data base and added another 1098 events
# (2226 layers) to the data base. All of these 1098 events contain the spatial and temporal
# coordinates. After removing 12 duplicated events (130 layers), we ended with 15 129 events
# (52 566 layers).
rm(list = ls())

pronasolos <- TRUE # ctb0064

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
brazil <- geobr::read_country()

# Rename columns following previous standards
rename <- c(
  "dados_id_febr",                "dataset_id",
  "evento_id_febr",               "id",
  "camada_id_febr",               "camada_id",
  "coord_longitude",              "coord_x",
  "coord_latitude",               "coord_y",
  "coord_estado_sigla",           "estado_id",
  "ph_h2o_25_eletrodo",           "ph",
  "ph_h2o",                       "ph",
  "ctc_soma_calc",                "ctc",
  "carbono_forno_1min950_cgdct",  "carbono",
  "argila_sodio_pipeta",          "argila",
  "densidade_solo_cilindro",      "dsi",
  "sibcs_20xx",                   "taxon_sibcs"
)
rename <- matrix(rename, ncol = 2, byrow = TRUE)

# Load external data sets
# Events
files_event <- list.files(
  path = path.expand("~/ownCloud/febr-repo/processamento"),
  pattern = "-evento.txt$",
  full.names = TRUE, recursive = TRUE)
data_event <- list()
for (i in seq_along(files_event)) {
  data_event[[i]] <- data.table::fread(files_event[i], dec = ",")
  id <- rev(strsplit(files_event[i], split = "/")[[1]])[1]
  id <- strsplit(id, "-")[[1]][1]
  data_event[[i]][, dados_id_febr := id]
  data.table::setnames(data_event[[i]], old = rename[, 1], new = rename[, 2], skip_absent = TRUE)
}
data_event <- data.table::rbindlist(data_event, fill = TRUE)
nrow(data_event)
# 10 098 (not all IFN events have layers)

# Standardize coordinate reference system
data_event[, coord_datum_epsg := as.integer(gsub("EPSG:", "", coord_datum_epsg))]
sf_data_event <- split(data_event, data_event[, coord_datum_epsg])
idx_transform <- which(names(sf_data_event) != 4326)
for (i in seq_along(sf_data_event)) {
  if (i %in% idx_transform) {
    crs <- as.integer(names(sf_data_event[i]))
    sf_data_event[[i]] <- sf::st_as_sf(
      sf_data_event[[i]],
      coords = c("coord_x", "coord_y"), crs = crs
    )
  sf_data_event[[i]] <- sf::st_transform(sf_data_event[[i]], crs = 4326)
  } else {
    sf_data_event[[i]] <- sf::st_as_sf(sf_data_event[[i]],
      coords = c("coord_x", "coord_y"),
      crs = 4326
    )
  }
}
data_event <- do.call(rbind, sf_data_event)
data_event <- cbind(sf::st_coordinates(data_event), as.data.frame(data_event))
data_event <- data.table::as.data.table(data_event)
data_event[coord_datum_epsg != 4326 & !is.na(coord_datum_epsg), coord_datum_epsg := 4326]
data.table::setnames(data_event, old = c("X", "Y"), new = c("coord_x", "coord_y"))
data_event[, geometry := NULL]

# Clean sampling date
data_event[data_coleta_ano < 1900, data_coleta_ano := NA_integer_]
data_event[data_coleta_ano > as.integer(format(Sys.time(), "%Y")), data_coleta_ano := NA_integer_]
nrow(data_event) # 10 098 (not all events have layers)

if (FALSE) {
  x11()
  plot(brazil, reset = FALSE, main = "PronaSolos")
  points(data_event[dataset_id == "ctb0064", coord_x],
    data_event[dataset_id == "ctb0064", coord_y],
    cex = 0.5, pch = 20
  )
}

# Camadas
files_layer <- list.files(
  path = path.expand("~/ownCloud/febr-repo/processamento"),
  pattern = "-camada.txt$",
  full.names = TRUE, recursive = TRUE)
data_layer <- list()
for (i in seq_along(files_layer)) {
  data_layer[[i]] <- data.table::fread(files_layer[i], dec = ",")
  id <- rev(strsplit(files_layer[i], split = "/")[[1]])[1]
  id <- strsplit(id, "-")[[1]][1]
  data_layer[[i]][, dados_id_febr := id]
  data.table::setnames(data_layer[[i]], old = rename[, 1], new = rename[, 2], skip_absent = TRUE)
}
data_layer <- data.table::rbindlist(data_layer, fill = TRUE)
nrow(data_layer) # 36 883
# (not all layers of the National Forest Inventory have events - there must be some error in their
# database)

# Merge data from events and layers
febr_data01 <- merge(data_event, data_layer, by = c("dataset_id", "id"))
colnames(febr_data01)
if (!"terrafina" %in% colnames(febr_data01)) {
  febr_data01[, terrafina := NA_real_]
}
if (!"camada_nome" %in% colnames(febr_data01)) {
  febr_data01[, camada_nome := NA_character_]
}
nrow(unique(febr_data01[, c("dataset_id", "id")])) # 9487 events (not all events have layers)
nrow(febr_data01) # 36 690 layers
# (not all layers of the National Forest Inventory have events - there must be some error in their
# database)

# Remove events/layers missing sampling date
# This step is necessary to discard unwanted data from PronaSolos
febr_data01 <- febr_data01[!is.na(data_coleta_ano), ]
nrow(unique(febr_data01[, "id"])) # 7683 events
nrow(febr_data01) # 28 422 layers

if (!pronasolos) {
  febr_data01 <- febr_data01[dataset_id != "ctb0064", ]
}

# Read FEBR data processed in the previous scripts
febr_data02 <- data.table::fread("mapbiomas-solos/data/01b-febr-data.txt", dec = ",", sep = "\t")
febr_data02[, coord_datum_epsg := 4326]

if (FALSE) {
  x11()
  plot(brazil, reset = FALSE, main = "FEBR")
  points(febr_data02[, coord_x], febr_data02[, coord_y], cex = 0.5, pch = 20)
}

# Corrigir amostras com terrafina = 0
# Assume-se que se tratam de amostras com dado faltante e que, quando faltante, o valor de terra
# fina é 1000 g/kg
febr_data02[terrafina == 0, terrafina := 1000]
nrow(unique(febr_data02[, "id"]))
# 14 190 events

# Merge FEBR data with external data
febr_data01[, observacao_id := id]
febr_data01[, id := paste0(dataset_id, "-", id)]
idx <- match(colnames(febr_data02), colnames(febr_data01))
idx01 <- na.exclude(idx)
idx02 <- which(!is.na(idx))
febr_data <- rbind(febr_data02[, ..idx02], febr_data01[, ..idx01])
nrow(unique(febr_data[, "id"]))
# FEBR: ???15 141; PronaSolos: 21 873
nrow(febr_data)
# FEBR: ???52 696; PronaSolos: 78 807

# Check if we have replicated sample points
# There are events in the FEBR data with the same ID but different spatial or temporal coordinates.
# This could also happen in the external data, but we have not checked it.
# We identify problem events by computing the standard deviation of the coordinates of each event:
# for non-duplicated events, the standard deviation should be zero.
nrow(unique(febr_data[, c("id", "data_coleta_ano", "coord_x", "coord_y")]))
# 21 914 --> should be equal to 21 873
# (there are events with the same ID but different coordinates)
febr_data[, std_x := sd(coord_x), by = c("dataset_id", "observacao_id")]
febr_data[is.na(std_x), std_x := 0]
febr_data[, std_y := sd(coord_y), by = c("dataset_id", "observacao_id")]
febr_data[is.na(std_y), std_y := 0]
febr_data[, std_t := sd(data_coleta_ano), by = c("dataset_id", "observacao_id")]
febr_data[is.na(std_t), std_t := 0]
febr_data[, std_xyt := (std_x + std_y + std_t)]
nrow(unique(febr_data[std_xyt > 0, c("dataset_id", "observacao_id")]))
# Result: 32 duplicated events
febr_data <- febr_data[std_xyt == 0, ] # remove duplicated events
nrow(unique(febr_data[, "id"]))
# Result: 21 841 events
nrow(febr_data)
# Result: 78 427 layers
febr_data[, c("std_x", "std_y", "std_t", "std_xyt") := NULL]

# Remove duplicated events: equal spatial and temporal coordinates
# Events are commonly reused in more than one data set. Besides, FEBR and PronaSolos data have a 
# large overlap.
first <- function(x) x[1, ]
tmp <- febr_data[, first(id),
  by = c("dataset_id", "observacao_id", "coord_x", "coord_y", "data_coleta_ano")
]
duplo <- duplicated(tmp[, c("coord_x", "coord_y", "data_coleta_ano")])
duplo <- tmp[duplo, V1]
febr_data <- febr_data[!(id %in% duplo), ] # remove duplicated events
nrow(unique(febr_data[, "id"]))
# Result: 12 731 events
nrow(febr_data)
# Result: 44 166 layers

# Set a random year between 1960 and 1984
febr_data[is.na(data_coleta_ano), data_coleta_ano := sample(1960:1984, 1)]

# Write data to disk
data.table::fwrite(febr_data, "mapbiomas-solos/data/02-febr-data.txt", sep = "\t", dec = ",")
