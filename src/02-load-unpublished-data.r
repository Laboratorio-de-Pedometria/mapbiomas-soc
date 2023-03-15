# LOAD UNPUBLISHED DATA ############################################################################
# SUMMARY. Several data sets were selected to enter the FEBR during the last months. However, these
# data sets have not been modelled according to the data model used in the FEBR. The reason for this
# is that we were looking for data that could be readily used to model the spatio-temporal variation
# of soil organic carbon stocks in Brazil. In other words, our focus was on simply gathering data
# that could help improve the predictions, thus focusing on key variables. The main data source
# in these phase was the National Forest Inventory, available at
# https://snif.florestal.gov.br/pt-br/inventario-florestal-nacional-ifn/ifn-dados-abertos. We also
# ingested data from a private natural reserve in the Pantanal biome (SESC Pantanal). Differences
# in laboratory methods were ignored during data processing and the new data was merged with the
# existing data -- the exception is the coordinate reference system, with EPSG:4326 set as target.
# KEY RESULTS. We had 14043 events (50470 layers) and added another 1079 events (2419) layers to
# the data base.
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("sf")) {
  install.packages("sf")
}

# Renomear colunas de acordo com padrão antigo
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

# Carregar dados de conjuntos de dados ainda não disponíveis no FEBR
# Eventos
files_event <- list.files(
  path = "/home/alessandro/ownCloud/febr-repo/processamento", pattern = "-evento.txt$",
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

# Standardize coordinate reference system
data_event[, coord_datum_epsg := as.integer(gsub("EPSG:", "", coord_datum_epsg))]
sf_data_event <- split(data_event, data_event[, coord_datum_epsg])
idx_transform <- which(names(sf_data_event) != 4326)
for (i in idx_transform) {
  crs <- as.integer(names(sf_data_event[i]))
  sf_data_event[[i]] <- sf::st_as_sf(
    sf_data_event[[i]], coords = c("coord_x", "coord_y"), crs = crs)
  sf_data_event[[i]] <- sf::st_transform(sf_data_event[[i]], crs = 4326)
}
data_event <- do.call(rbind, sf_data_event)
data_event <- cbind(sf::st_coordinates(data_event), as.data.frame(data_event))
data_event <- data.table::as.data.table(data_event)
data_event[coord_datum_epsg != 4326 & !is.na(coord_datum_epsg), coord_datum_epsg := 4326]
data.table::setnames(data_event, old = c("X", "Y"), new = c("coord_x", "coord_y"))
nrow(data_event)

# Camadas
files_layer <- list.files(
  path = "/home/alessandro/ownCloud/febr-repo/processamento", pattern = "-camada.txt$",
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
nrow(data_layer)

# Juntar dados de eventos e camadas
febr_data01 <- merge(data_event, data_layer)
colnames(febr_data01)
if (!"terrafina" %in% colnames(febr_data01)) {
  febr_data01[, terrafina := NA_real_]
}
if (!"camada_nome" %in% colnames(febr_data01)) {
  febr_data01[, camada_nome := NA_character_]
}

# Ler dados do disco
# Corrigir amostras com terrafina = 0
# Assume-se que se tratam de amostras com dado faltante e que, quando faltante, o valor de terra
# fina é 1000 g/kg
febr_data02 <- data.table::fread("mapbiomas-solos/data/01-febr-data.txt", dec = ",", sep = "\t")
febr_data02[, coord_datum_epsg := 4326]
febr_data02[terrafina == 0, terrafina := 1000]
length(unique(febr_data02[, id]))
nrow(febr_data02)

# Juntar dados
febr_data01[, id := paste0(dataset_id, "-", id)]
idx <- match(colnames(febr_data02), colnames(febr_data01))
idx01 <- na.exclude(idx)
idx02 <- which(!is.na(idx))
febr_data <- rbind(febr_data01[, ..idx01], febr_data02[, ..idx02])

# Escrever dados em disco
data.table::fwrite(febr_data, "mapbiomas-solos/data/02-febr-data.txt", sep = "\t", dec = ",")
