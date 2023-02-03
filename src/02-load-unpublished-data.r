rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("sf")) {
  install.packages("sf")
}

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
}
data_event <- do.call(rbind, data_event)

# Standardize coordinate reference system
data_event[, coord_datum_epsg := as.integer(gsub("EPSG:", "", coord_datum_epsg))]
sf_data_event <- split(data_event, data_event[, coord_datum_epsg])
idx_transform <- which(names(sf_data_event) != 4326)
for (i in idx_transform) {
  crs <- as.integer(names(sf_data_event[i]))
  sf_data_event[[i]] <- sf::st_as_sf(
    sf_data_event[[i]], coords = c("coord_longitude", "coord_latitude"), crs = crs)
  sf_data_event[[i]] <- sf::st_transform(sf_data_event[[i]], crs = 4326)
}
data_event <- do.call(rbind, sf_data_event)
data_event <- cbind(sf::st_coordinates(data_event), as.data.frame(data_event))
data_event <- data.table::as.data.table(data_event)
data_event[coord_datum_epsg != 4326 & !is.na(coord_datum_epsg), coord_datum_epsg := 4326]
data.table::setnames(data_event, old = c("X", "Y"), new = c("coord_longitude", "coord_latitude"))

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
}
data_layer <- do.call(rbind, data_layer)

# Juntar dados de eventos e camadas
febr_data01 <- merge(data_event, data_layer)

# Renomear colunas de acordo com padrão antigo
colnames(febr_data01)
rename <- c(
  "dados_id_febr", "dataset_id",
  "evento_id_febr", "id",
  "camada_id_febr", "camada_id",
  "coord_longitude", "coord_x",
  "coord_latitude", "coord_y",
  "coord_estado_sigla", "estado_id",
  "ph_h2o_25_eletrodo", "ph",
  "ctc_soma_calc", "ctc",
  "carbono_forno_1min950_cgdct", "carbono",
  "argila_sodio_pipeta", "argila",
  "densidade_solo_cilindro", "dsi"
)
rename <- matrix(rename, ncol = 2, byrow = TRUE)
data.table::setnames(febr_data01, old = rename[, 1], new = rename[, 2])
febr_data01[, areia := NA_real_]
febr_data01[, silte := NA_real_]
febr_data01[, terrafina := NA_real_]
febr_data01[, taxon_sibcs := NA_character_]
febr_data01[, estado_id := NA_character_]
febr_data01[, camada_nome := NA_character_]

# Ler dados do disco
febr_data02 <- data.table::fread("mapbiomas-solos/data/01-febr-data.txt", dec = ",", sep = "\t")
febr_data02[, coord_datum_epsg := 4326]

# Juntar dados
idx <- match(colnames(febr_data02), colnames(febr_data01))
idx01 <- na.exclude(idx)
idx02 <- which(!is.na(idx))
febr_data <- rbind(febr_data01[, ..idx01], febr_data02[, ..idx02])

# Escrever dados em disco
data.table::fwrite(febr_data, "mapbiomas-solos/data/02-febr-data.txt", sep = "\t", dec = ",")
