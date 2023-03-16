# RECOVER THE TIME COORDINATE ######################################################################
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
# We started with 13 112 events, 4249 out of which did not have the sampling date
# recorded in the latest (2021) FEBR snapshot. Our team of data curators recovered the sampling date
# of 4249 - 2657 = 1592 events from between 1957 and 2007. Using the average sampling year of the
# source survey report, we attributed a sampling date to 2657 - 1790 = 897 events. For the remaining
# 1790 events, we attributed a sampling date based on the year of publication of the source survey
# report. Finally, out of the 13 112 events, 4834 events were from before 1985 and had their
# sampling data overwritten.
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
x11()
missing_time <- is.na(febr_data[["data_coleta_ano"]])
hist(febr_data[["data_coleta_ano"]], sub = paste0("n = ", sum(!missing_time)))
rug(febr_data[["data_coleta_ano"]])

# Set minimum sampling year to 1985
# 4834 events from before 1985
nrow(febr_data[data_coleta_ano < 1985 & profund_sup == 0, ])
febr_data[data_coleta_ano < 1985, data_coleta_ano := 1985]
hist(febr_data[["data_coleta_ano"]])
rug(febr_data[["data_coleta_ano"]])

# Escrever dados em disco
data.table::fwrite(febr_data, "mapbiomas-solos/data/01-febr-data.txt", sep = "\t", dec = ",")
