# RECOVER THE TIME COORDINATE ######################################################################
# SUMMARY. Various events in the latest FEBR snapshot are missing the time coordinate. These events
# were identified and written to disk in file named no-time-coord.csv. Our team of data curators
# searched for the sampling date of each of these events in the original soil survey reports. The
# recovered time coordinates were registered in a Google Spreadsheet. The data stored in this
# spreadsheet is used to update the FEBR snapshot. However, for various events, the sampling date
# is not registered in the survey report. For each of these events, we attribute the average (mean)
# year of sampling of the survey report from which they were compiled -- we consider that to be the
# most likely sampling date. The exception are the events of the volumes 01-19 of the RADAM project.
# For these events, if the sampling date is missing, we set the sampling year to be 1970.
# * start with 13 112 events, 4249 without sampling date
# * sampling date recovered: between 1957 and 2007
# * only 2657 event remain without sampling date
# * using the average date, only 1790 event remain without sampling date
# * after processing the RADAM volumes, 837 event miss the sampling date
# * Santa Catarina - year 1995 - 762 remaining without year
# * Canchim: 1995
# * all others: 1985
# KEY RESULTS. We recovered or inferred the sampling date of 4249 events.
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}

# Read processed data (FEBR snapshot)
url <- "http://cloud.utfpr.edu.br/index.php/s/QpG6Tcr6x1NBOcI/download"
temp <- tempfile(fileext = ".zip")
download.file(url = url, destfile = temp)
febr_data <- data.table::fread(unzip(temp), sep = ";", dec = ",")
colnames(febr_data)

# Corrigir amostras com terrafina = 0
# febr_data[terrafina == 0, c("dataset_id", "observacao_id")]
# Assume-se que se tratam de amostras com dado faltante e que, quando faltante, o valor de terra
# fina é 1000 g/kg
febr_data[terrafina == 0, terrafina := 1000]

# Prepare time coordinate
febr_data[, observacao_data := as.Date(observacao_data, format = "%Y-%m-%d")]
febr_data[, data_coleta_dia := as.integer(format(observacao_data, "%d"))]
febr_data[, data_coleta_mes := as.integer(format(observacao_data, "%m"))]
febr_data[, data_coleta_ano := as.integer(format(observacao_data, "%Y"))]

# Distribuição temporal das amostras com data de coleta
# 13 112 events, 4249 without sampling date
nrow(febr_data[profund_sup == 0, ])
nrow(febr_data[is.na(data_coleta_ano) & profund_sup == 0, ])
missing_time <- is.na(febr_data[["data_coleta_ano"]])
par(mfrow = c(1, 2))
hist(febr_data[["data_coleta_ano"]], sub = paste0("N = ", sum(!missing_time)))
rug(febr_data[["data_coleta_ano"]])

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
# only 2657 event remain without sampling date
nrow(febr_data[is.na(data_coleta_ano) & profund_sup == 0, ])
# N = 38800
missing_time <- is.na(febr_data[["data_coleta_ano"]])
hist(febr_data[["data_coleta_ano"]], sub = paste0("n = ", sum(!missing_time)))
rug(febr_data[["data_coleta_ano"]])

# Atribuir data de coleta mais provável
# Utilizar a data média do trabalho
# using the average date, only 1790 event remain without sampling date
average_year <- febr_data[,
  .(data_coleta_ano = round(mean(data_coleta_ano, na.rm = TRUE))),
  by = dataset_id]
idx_averaged <- match(febr_data[missing_time, dataset_id], average_year[, dataset_id])
febr_data[missing_time, data_coleta_ano := average_year[idx_averaged, data_coleta_ano]]
nrow(febr_data[is.na(data_coleta_ano) & profund_sup == 0, ])

# Distribuição temporal das amostras com data de coleta após resgate
# N = 42.553
missing_time <- is.na(febr_data[["data_coleta_ano"]])
hist(febr_data[["data_coleta_ano"]], sub = paste0("n = ", sum(!missing_time)))
rug(febr_data[["data_coleta_ano"]])

# RADAMBRASIL: set sampling year to 1985
# after processing the RADAM volumes, 837 event miss the sampling date
febr_data[
  grepl("RADAMBRASIL", dataset_titulo, ignore.case = TRUE) & is.na(data_coleta_ano),
  data_coleta_ano := 1985
]
# idx_radam <- grepl("RADAMBRASIL", febr_data[, dataset_titulo]) &
#   !grepl("Volume 2", febr_data[, dataset_titulo]) &
#   !grepl("Volume 3", febr_data[, dataset_titulo])
# febr_data[idx_radam, data_coleta_ano := 1970]
nrow(febr_data[is.na(data_coleta_ano) & profund_sup == 0, ])

# Inventário das terras em microbacias hidrográficas, Santa Catarina
# year: 1995
# 762 remaining without year
febr_data[
  grepl("Inventário das terras em microbacias hidrográficas", dataset_titulo, ignore.case = TRUE) & is.na(data_coleta_ano),
  data_coleta_ano := 1995
]
nrow(febr_data[is.na(data_coleta_ano) & profund_sup == 0, ])

# All other
febr_data[dataset_id == "ctb0815" & is.na(data_coleta_ano), data_coleta_ano := 1995]
febr_data[is.na(data_coleta_ano), data_coleta_ano := 1985]
nrow(febr_data[is.na(data_coleta_ano) & profund_sup == 0, ])

# Distribuição temporal das amostras com data de coleta após resgate
# N = 46.221
missing_time <- is.na(febr_data[["data_coleta_ano"]])
hist(febr_data[["data_coleta_ano"]], sub = paste0("n = ", sum(!missing_time)))
rug(febr_data[["data_coleta_ano"]])

# Escrever dados em disco
data.table::fwrite(febr_data, "mapbiomas-solos/data/01-febr-data.txt", sep = "\t", dec = ",")
