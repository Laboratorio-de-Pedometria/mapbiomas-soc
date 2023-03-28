# # MapBiomas Soil (beta)
# Script 01a. Process FEBR data - time coordinate
# Alessandro Samuel-Rosa & Taciara Zborowski Horst
# 2023 CC-BY
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
nrow(unique(febr_data[, c("dataset_id", "observacao_id")])) # 14 043 events
nrow(febr_data) # 50 470 layers
colnames(febr_data)

# Process time coordinate (sampling year)
febr_data[, observacao_data := as.Date(observacao_data, format = "%Y-%m-%d")]
febr_data[, data_coleta_dia := as.integer(format(observacao_data, "%d"))]
febr_data[, data_coleta_mes := as.integer(format(observacao_data, "%m"))]
febr_data[, data_coleta_ano := as.integer(format(observacao_data, "%Y"))]

# Clean odd sampling date
febr_data[data_coleta_ano == 1939, data_coleta_ano := NA_integer_]

# Temporal distribution of samples with known sampling date
nrow(unique(febr_data[, c("dataset_id", "observacao_id")]))
nrow(unique(febr_data[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# FEBR: 14 043 events, 4848 without sampling date
missing_time <- is.na(febr_data[["data_coleta_ano"]])
if (FALSE) {
  x11()
  hist(febr_data[["data_coleta_ano"]], sub = paste0("N = ", sum(!missing_time)))
  rug(febr_data[["data_coleta_ano"]])
}

# Write table to disk with events missing date
# Only the surface layer (profund_sup == 0) of each event is exported.
# The field dataset_id is reset as a URL to facilita the access to the respective webpage on FEBR.
# The recovery of the sampling date will be done collectively by our team of data curators using a
# Google Sheets spreadsheet to register the data.
no_time_coord <- febr_data[missing_time & profund_sup == 0,
  c("dataset_id", "dataset_titulo", "estado_id", "municipio_id", "observacao_id",
    "data_coleta_dia", "data_coleta_mes", "data_coleta_ano")]
no_time_coord[, dataset_id := paste0("https://www.pedometria.org/febr/", dataset_id, "/")]
write.table(
  x = no_time_coord,
  file = "mapbiomas-solos/data/no-time-coord.csv",
  sep = "\t", row.names = FALSE)

# Read Google Sheets spreadsheet containing the recovered sampling dates
# It is not necessary to set the table because the spreadsheet contains only one.
key <- "1UbuI_oMzFmclztmhZQYsuU0mn_Lx3NhSeBoFw0m4lv0"
file <- paste0("http://docs.google.com/spreadsheets/d/", key, "/pub?output=csv")
recovered_time <- data.table::fread(file, header = TRUE, na.strings = c("-", ""), sep = ",")
recovered_time[, data_coleta_ano := as.integer(data_coleta_ano)]
head(recovered_time)

# Check the range of values
# Any error present in the downloaded data is corrected in the Google Sheets spreadsheet
# Result: between 1957 and 2007
range(recovered_time[["data_coleta_ano"]], na.rm = TRUE)

# Fill up the original table using the data recovered by our team of data curators
recovered_time[, dados_id := gsub("https://www.pedometria.org/febr/", "", dados_id)]
recovered_time[, dados_id := gsub("/", "", dados_id)]
recovered_time[, id := paste0(dados_id, "-", evento_id_febr)]
febr_data[, id := paste0(dataset_id, "-", observacao_id)]
idx_recovered <- match(febr_data[missing_time, id], recovered_time[["id"]])
febr_data[missing_time, data_coleta_ano := recovered_time[idx_recovered, data_coleta_ano]]

# Temporal distribution of samples with known sampling date after data rescue
# In sum, we were able to recover the sampling date of more than 6000 samples
# Result: 3402 events remain without a known sampling date
nrow(unique(febr_data[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
febr_data[, na_year := FALSE]
febr_data[is.na(data_coleta_ano), na_year := TRUE]
missing_time <- is.na(febr_data[["data_coleta_ano"]])
if (FALSE) {
  x11()
  hist(febr_data[["data_coleta_ano"]], sub = paste0("n = ", sum(!missing_time)))
  rug(febr_data[["data_coleta_ano"]])
}

# Attribute the most likely temporal coordinate
# Use the mean of the source soil survey
# Result: Using the average date, only 1869 event remain without sampling date
average_year <- febr_data[,
  .(data_coleta_ano = round(mean(data_coleta_ano, na.rm = TRUE))),
  by = dataset_id]
idx_averaged <- match(febr_data[missing_time, dataset_id], average_year[, dataset_id])
febr_data[missing_time, data_coleta_ano := average_year[idx_averaged, data_coleta_ano]]
nrow(unique(febr_data[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))

# Temporal distribution of samples with known sampling date after data rescue
missing_time <- is.na(febr_data[["data_coleta_ano"]])
if (FALSE) {
  x11()
  hist(febr_data[["data_coleta_ano"]], sub = paste0("n = ", sum(!missing_time)))
  rug(febr_data[["data_coleta_ano"]])
}

# Inventário das terras em microbacias hidrográficas, Santa Catarina
# Target year: 1995
# Result: 1795 event remaining without year
febr_data[
  grepl("Inventário das terras em microbacias hidrográficas", dataset_titulo, ignore.case = TRUE) &
    is.na(data_coleta_ano),
  data_coleta_ano := 1995
]
nrow(unique(febr_data[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))

# LEVANTAMENTO SEMIDETALHADO DOS SOLOS DA FAZENDA CANCHIM SÃO CARLOS - SP.
# Target year: 1995
# Result: 1712 events remain without sampling year
febr_data[dataset_id == "ctb0815" & is.na(data_coleta_ano), data_coleta_ano := 1995]
nrow(unique(febr_data[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))

# RADAMBRASIL: set sampling year to any moment between 1970 and 1984
# Result: 786 events still miss the sampling date
idx <- febr_data[
  grepl("RADAMBRASIL", dataset_titulo, ignore.case = TRUE) & is.na(data_coleta_ano),
  id
]
febr_data[
  id %in% idx,
  data_coleta_ano := round(runif(length(idx), min = 1970, max = 1984))
]
nrow(unique(febr_data[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))

# All other
# Set a random year between 1960 and 1984
idx <- febr_data[is.na(data_coleta_ano), id]
febr_data[id %in% idx, data_coleta_ano := round(runif(n = length(idx), min = 1960, max = 1984))]
nrow(unique(febr_data[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))

# Temporal distribution of samples with known sampling date after data rescue
missing_time <- is.na(febr_data[["data_coleta_ano"]])
if (FALSE) {
  x11()
  hist(febr_data[["data_coleta_ano"]], sub = paste0("n = ", sum(!missing_time)))
  rug(febr_data[["data_coleta_ano"]])
}

# Write data to disk
data.table::fwrite(febr_data, "mapbiomas-solos/data/01a-febr-data.txt", sep = "\t", dec = ",")
