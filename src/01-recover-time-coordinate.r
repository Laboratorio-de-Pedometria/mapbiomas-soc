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
# 32145 camadas
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
# N = 38800
missing_time <- is.na(febr_data[["data_coleta_ano"]])
hist(febr_data[["data_coleta_ano"]], sub = paste0("n = ", sum(!missing_time)))
rug(febr_data[["data_coleta_ano"]])

# Atribuir data de coleta mais provável
# Utilizar a data média do trabalho
average_year <- febr_data[,
  .(data_coleta_ano = round(mean(data_coleta_ano, na.rm = TRUE))),
  by = dataset_id]
idx_averaged <- match(febr_data[missing_time, dataset_id], average_year[, dataset_id])
febr_data[missing_time, data_coleta_ano := average_year[idx_averaged, data_coleta_ano]]

# Distribuição temporal das amostras com data de coleta após resgate
# N = 42.553
missing_time <- is.na(febr_data[["data_coleta_ano"]])
hist(febr_data[["data_coleta_ano"]], sub = paste0("n = ", sum(!missing_time)))
rug(febr_data[["data_coleta_ano"]])

# Para dados do RADAM, volumes 1 a 19
idx_radam <- grepl("RADAMBRASIL", febr_data[, dataset_titulo]) &
  !grepl("Volume 2", febr_data[, dataset_titulo]) &
  !grepl("Volume 3", febr_data[, dataset_titulo])
febr_data[idx_radam, data_coleta_ano := 1970]

# Distribuição temporal das amostras com data de coleta após resgate
# N = 46.221
missing_time <- is.na(febr_data[["data_coleta_ano"]])
hist(febr_data[["data_coleta_ano"]], sub = paste0("n = ", sum(!missing_time)))
rug(febr_data[["data_coleta_ano"]])

# Escrever dados em disco
data.table::fwrite(febr_data, "mapbiomas-solos/data/01-febr-data.txt", sep = "\t", dec = ",")
