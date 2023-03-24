# 01a. PROCESS FEBR DATA - TIME COORDINATE #########################################################
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

# Process time coordinate (sampling year)
febr_data[, observacao_data := as.Date(observacao_data, format = "%Y-%m-%d")]
febr_data[, data_coleta_dia := as.integer(format(observacao_data, "%d"))]
febr_data[, data_coleta_mes := as.integer(format(observacao_data, "%m"))]
febr_data[, data_coleta_ano := as.integer(format(observacao_data, "%Y"))]

# Temporal distribution of samples with known sampling date
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
# Result: 3401 events remain without a known sampling date
nrow(unique(febr_data[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
missing_time <- is.na(febr_data[["data_coleta_ano"]])
if (FALSE) {
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
  hist(febr_data[["data_coleta_ano"]], sub = paste0("n = ", sum(!missing_time)))
  rug(febr_data[["data_coleta_ano"]])
}

# RADAMBRASIL: set sampling year to any moment between 1970 and 1984
# Result: 943 events still miss the sampling date
febr_data[
  grepl("RADAMBRASIL", dataset_titulo, ignore.case = TRUE) & is.na(data_coleta_ano),
  data_coleta_ano := sample(1970:1984, 1)
]
nrow(unique(febr_data[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))

# Inventário das terras em microbacias hidrográficas, Santa Catarina
# Target year: 1995
# Result: 869 event remaining without year
febr_data[
  grepl("Inventário das terras em microbacias hidrográficas", dataset_titulo, ignore.case = TRUE) &
    is.na(data_coleta_ano),
  data_coleta_ano := 1995
]
nrow(unique(febr_data[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))

# LEVANTAMENTO SEMIDETALHADO DOS SOLOS DA FAZENDA CANCHIM SÃO CARLOS - SP.
# Target year: 1995
# Result: 786 events remain without sampling year
febr_data[dataset_id == "ctb0815" & is.na(data_coleta_ano), data_coleta_ano := 1995]
nrow(unique(febr_data[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))

# All other
# Set a random year between 1960 and 1984
febr_data[is.na(data_coleta_ano), data_coleta_ano := sample(1960:1984, 1)]
nrow(unique(febr_data[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))

# Temporal distribution of samples with known sampling date after data rescue
missing_time <- is.na(febr_data[["data_coleta_ano"]])
if (FALSE) {
  hist(febr_data[["data_coleta_ano"]], sub = paste0("n = ", sum(!missing_time)))
  rug(febr_data[["data_coleta_ano"]])
}
# Escrever dados em disco
data.table::fwrite(febr_data, "mapbiomas-solos/data/01a-febr-data.txt", sep = "\t", dec = ",")
