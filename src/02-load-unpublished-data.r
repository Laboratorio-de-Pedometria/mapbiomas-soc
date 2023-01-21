rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
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
# data.table::setnames(data_event, old = c("evento_id_febr", "coord_longitude", "coord_latitude"),
  # new = c("id", "coord_x", "coord_y"))
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
febr_data <- merge(data_event, data_layer)

# Escrever dados em disco
data.table::fwrite(febr_data, "mapbiomas-solos/data/02-febr-data.txt", sep = "\t", dec = ",")
