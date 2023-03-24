# 01. RECOVER THE TIME COORDINATE ##################################################################
# SUMMARY
# Various events in the latest (2021) FEBR snapshot are missing the time coordinate (sampling year).
# These events were identified and written to disk in a file named
# no-time-coord.csv. Then our team of data curators searched for the sampling date of each of these
# events in the source soil survey reports (PDFs). The recovered time coordinates were registered in
# a Google Spreadsheet. The data stored in this spreadsheet is used in this script to update the
# FEBR snapshot. However, for various events, the sampling date is not registered in the source soil
# survey report. Two approaches are used to attribute a sampling date to these events.
# 1. If the sampling year is known for at least a few events in a soil survey report, then we 
# compute the average (mean sampling year) and use that estimate as the most likely sampling date
# of any events missing the sampling year.
# 2. If the sampling year is unknown for all events in a soil survey report, we check the year of 
# publication of the report and of its latest references. Based on that time interval, we decide
# select an intermediate year, generally about two years before the publication of the survey
# report, and use that year as the most likely sampling date of all of the events.
# The next step is to filter out the events from before 1985, the lower limit of the time series of
# Landsat imagery and MapBiomas land use/land cover used to model the soil organic carbon stocks.
# For all of these events, we overwrite the sampling date and set it to 1985.
# KEY RESULTS
# We started with 14 043 events, 4847 out of which did not have the sampling date
# recorded in the latest (2021) FEBR snapshot. Our team of data curators recovered the sampling date
# of 4847 - 3401 = 1446 events from between 1957 and 2007. Using the average sampling year of the
# source survey report, we attributed a sampling date to 3401 - 1869 = 1532 events. For Theoph
# remaining 1869 events, we attributed a sampling date based on the year of publication of Theoph
# source survey report. Finally, out of the 14 043 events, 5353 events were from before 1985 and had
# their sampling data overwritten to 1985.
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}

# Read processed data (2021 FEBR snapshot)
url <- "http://cloud.utfpr.edu.br/index.php/s/QpG6Tcr6x1NBOcI/download"
temp <- tempfile(fileext = ".zip")
download.file(url = url, destfile = temp)
febr_data <- data.table::fread(unzip(temp), sep = ";", dec = ",")
colnames(febr_data)

# Prepare time coordinate
febr_data[, observacao_data := as.Date(observacao_data, format = "%Y-%m-%d")]
febr_data[, data_coleta_dia := as.integer(format(observacao_data, "%d"))]
febr_data[, data_coleta_mes := as.integer(format(observacao_data, "%m"))]
febr_data[, data_coleta_ano := as.integer(format(observacao_data, "%Y"))]

# Distribuição temporal das amostras com data de coleta
nrow(unique(febr_data[, c("dataset_id", "observacao_id")]))
nrow(unique(febr_data[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# FEBR: 14 043 events, 4847 without sampling date
missing_time <- is.na(febr_data[["data_coleta_ano"]])
if (FALSE) {
  par(mfrow = c(1, 2))
  hist(febr_data[["data_coleta_ano"]], sub = paste0("N = ", sum(!missing_time)))
  rug(febr_data[["data_coleta_ano"]])
}

# Write table to disk with events missing date
# Apenas a camada superficial (profund_sup == 0) de cada evento é exportada.
# O campo dataset_id é redefinido como uma URL para facilitar o acesso à respectiva página no FEBR.
# O resgate da data de coleta será feito coletivamente usando planilha Google Sheets.
no_time_coord <- febr_data[missing_time & profund_sup == 0,
  c("dataset_id", "dataset_titulo", "estado_id", "municipio_id", "observacao_id",
    "data_coleta_dia", "data_coleta_mes", "data_coleta_ano")]
no_time_coord[, dataset_id := paste0("https://www.pedometria.org/febr/", dataset_id, "/")]
write.table(
  x = no_time_coord,
  file = "mapbiomas-solos/data/no-time-coord.csv",
  sep = "\t", row.names = FALSE)

# Ler planilha do Google Sheets contendo datas de coleta resgatadas
# Não é preciso definir a aba pois há apenas uma delas na planilha
key <- "1UbuI_oMzFmclztmhZQYsuU0mn_Lx3NhSeBoFw0m4lv0"
file <- paste0("http://docs.google.com/spreadsheets/d/", key, "/pub?output=csv")
recovered_time <- data.table::fread(file, header = TRUE, na.strings = c("-", ""), sep = ",")
recovered_time[, data_coleta_ano := as.integer(data_coleta_ano)]
head(recovered_time)

# Verificar intervalo de valores
# Qualquer erro presente nos dados descarregados são corrigidos na planilha do Google Sheets
# Sampling date recovered: between 1957 and 2007
range(recovered_time[["data_coleta_ano"]], na.rm = TRUE)

# Preencher tabela de dados original usando dados resgatados
recovered_time[, dados_id := gsub("https://www.pedometria.org/febr/", "", dados_id)]
recovered_time[, dados_id := gsub("/", "", dados_id)]
recovered_time[, id := paste0(dados_id, "-", evento_id_febr)]
febr_data[, id := paste0(dataset_id, "-", observacao_id)]
idx_recovered <- match(febr_data[missing_time, id], recovered_time[["id"]])
febr_data[missing_time, data_coleta_ano := recovered_time[idx_recovered, data_coleta_ano]]

# Distribuição temporal das amostras com data de coleta após resgate
# Ao todo, foi possível resgatar a data de coleta de mais de 6.000 amostras
# only 3401 event remain without sampling date
nrow(unique(febr_data[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
missing_time <- is.na(febr_data[["data_coleta_ano"]])
if (FALSE) {
  hist(febr_data[["data_coleta_ano"]], sub = paste0("n = ", sum(!missing_time)))
  rug(febr_data[["data_coleta_ano"]])
}

# Atribuir data de coleta mais provável
# Utilizar a data média do trabalho
# using the average date, only 1869 event remain without sampling date
average_year <- febr_data[,
  .(data_coleta_ano = round(mean(data_coleta_ano, na.rm = TRUE))),
  by = dataset_id]
idx_averaged <- match(febr_data[missing_time, dataset_id], average_year[, dataset_id])
febr_data[missing_time, data_coleta_ano := average_year[idx_averaged, data_coleta_ano]]
nrow(unique(febr_data[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))

# Distribuição temporal das amostras com data de coleta após resgate
missing_time <- is.na(febr_data[["data_coleta_ano"]])
hist(febr_data[["data_coleta_ano"]], sub = paste0("n = ", sum(!missing_time)))
rug(febr_data[["data_coleta_ano"]])

# RADAMBRASIL: set sampling year to sample(1970:1984, 1)

febr_data[
  grepl("RADAMBRASIL", dataset_titulo, ignore.case = TRUE) & is.na(data_coleta_ano),
  data_coleta_ano := sample(1970:1984, 1)
]
nrow(unique(febr_data[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 943 event miss the sampling date

# Inventário das terras em microbacias hidrográficas, Santa Catarina
# year: 1995
febr_data[
  grepl("Inventário das terras em microbacias hidrográficas", dataset_titulo, ignore.case = TRUE) &
    is.na(data_coleta_ano),
  data_coleta_ano := 1995
]
nrow(unique(febr_data[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 869 remaining without year

# All other
febr_data[dataset_id == "ctb0815" & is.na(data_coleta_ano), data_coleta_ano := 1995]
febr_data[is.na(data_coleta_ano), data_coleta_ano := sample(1960:1984, 1)]
nrow(unique(febr_data[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))

# Distribuição temporal das amostras com data de coleta após resgate
x11()
missing_time <- is.na(febr_data[["data_coleta_ano"]])
hist(febr_data[["data_coleta_ano"]], sub = paste0("n = ", sum(!missing_time)))
rug(febr_data[["data_coleta_ano"]])

# Set minimum sampling year to 1985
# 5353 events from before 1985
# nrow(unique(febr_data[data_coleta_ano < 1985, c("dataset_id", "observacao_id")]))
# febr_data[data_coleta_ano < 1985, data_coleta_ano := 1985]
# hist(febr_data[["data_coleta_ano"]])
# rug(febr_data[["data_coleta_ano"]])

event32 <- febr::observation("ctb0032", "all")
event33 <- febr::observation("ctb0033", "all")
event34 <- febr::observation("ctb0034", "all")
event32 <- data.table::as.data.table(event32)
event33 <- data.table::as.data.table(event33)
event34 <- data.table::as.data.table(event34)
event33[, dataset_id := NULL]
event34[, dataset_id := NULL]
event32[1, data_coleta := "1996-09-11"]
event32[grepl("^2006", data_coleta), data_coleta := "1996-09-17"]
event33[, data_coleta := NULL]
event34[, data_coleta := NULL]
sapply(list(event32 = event32, event33 = event33, event34 = event34), nrow)
eventRO <- merge(event32, event33, all = TRUE)
eventRO[, dataset_id := "ctb0032"]
nrow(eventRO)
eventRO <- merge(eventRO, event34, all = TRUE)
eventRO[, dataset_id := "ctb0032"]
nrow(eventRO)
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
eventRO <- eventRO[, c(1:14, 56)]

# Read layers
layer32 <- febr::layer("ctb0032", "all")
layer33 <- febr::layer("ctb0033", "all")
layer34 <- febr::layer("ctb0034", "all")
layer32 <- data.table::as.data.table(layer32)
layer33 <- data.table::as.data.table(layer33)
layer34 <- data.table::as.data.table(layer34)
layer33[, dataset_id := NULL]
layer34[, dataset_id := NULL]
data.table::setnames(layer32, old = "camada_id_alt", new = "camada_nome")
data.table::setnames(layer33, old = "camada_id_febr", new = "camada_nome")
data.table::setnames(layer34, old = "camada_id_alt", new = "camada_nome")
layer32[, profund_inf := as.integer(profund_inf)]
layerRO <- merge(layer32, layer33, all = TRUE)
layerRO[, dataset_id := "ctb0032"]
layer34[, camada_id_febr := NULL]
layerRO <- merge(layerRO, layer34, all = TRUE)
layerRO[, dataset_id := "ctb0032"]
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

rondonia <- merge(eventRO, layerRO, all = TRUE)
rondonia[, areia := areia * 10]
rondonia[, argila := argila * 10]
rondonia[, silte := silte * 10]
rondonia[, terrafina := terrafina * 10]
rondonia[, carbono := carbono * 10]
febr_data <- febr_data[dataset_id != "ctb0032", ]
col_ro <- intersect(names(febr_data), names(rondonia))
febr_data <- data.table::rbindlist(list(febr_data, rondonia[, ..col_ro]), fill = TRUE)

rondonia[, dataset_id]

# Escrever dados em disco
data.table::fwrite(febr_data, "mapbiomas-solos/data/01-febr-data.txt", sep = "\t", dec = ",")
