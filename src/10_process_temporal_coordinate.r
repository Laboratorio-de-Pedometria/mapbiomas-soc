# title: SoilData - Soil Organic Carbon Stock
# subtitle: Process temporal coordinate
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2024 CC-BY
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("dataverse")) {
  install.packages("dataverse")
}

# Read the latest Brazilian Soil Dataset
# Check if "mapbiomas-soc/data/00_brazilian_soil_dataset_2023.txt" exists. If not, read the Brazilian 
# Soil Dataset 2023 using the 'dataverse' package. Next, write it to
# 'mapbiomas-soc/data/00_brazilian_soil_dataset_2023.txt'. The dataset is available at
# https://doi.org/10.60502/SoilData/TUI25K. If the file already exists, read it using the
# 'data.table' package.
file_path <- "00_mapbiomas-soc/data/brazilian_soil_dataset_2023.txt"
if (!file.exists(file_path)) {
  br_soil2023 <- dataverse::get_dataframe_by_name("brazilian-soil-dataset-2023.txt",
    server = "https://soildata.mapbiomas.org/dataverse/soildata",
    dataset = "10.60502/SoilData/TUI25K", .f = data.table::fread
  )
  data.table::fwrite(br_soil2023, file_path, dec = ",", sep = ";")
} else {
  br_soil2023 <- data.table::fread(file_path, dec = ",", sep = ";")
}
nrow(unique(br_soil2023[, c("dataset_id", "observacao_id")])) # 14 043 events
nrow(br_soil2023) # 50 470 layers

# Process time coordinate (sampling year)
br_soil2023[, observacao_data := as.Date(observacao_data, format = "%Y-%m-%d")]
br_soil2023[, data_coleta_ano := as.integer(format(observacao_data, "%Y"))]

# Clean odd sampling date
br_soil2023[data_coleta_ano < 1950, data_coleta_ano := NA_integer_]

# Temporal distribution of samples with known sampling date
nrow(unique(br_soil2023[, c("dataset_id", "observacao_id")]))
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 14 043 events, 4848 without sampling date
missing_time <- is.na(br_soil2023[["data_coleta_ano"]])
if (FALSE) {
  x11()
  hist(br_soil2023[["data_coleta_ano"]], sub = paste0("N = ", sum(!missing_time)))
  rug(br_soil2023[["data_coleta_ano"]])
}

# # Write table to disk with events missing date
# # Only the surface layer (profund_sup == 0) of each event is exported.
# # The field dataset_id is reset as a URL to facilitate access to the respective webpage on FEBR.
# # The recovery of the sampling date will be done collectively by our team of data curators using a
# # Google Sheets spreadsheet to register the data.
# no_time_coord <- br_soil2023[
#   is.na(data_coleta_ano) & profund_sup == 0,
#   c(
#     "dataset_id", "dataset_titulo", "estado_id", "municipio_id", "observacao_id",
#     "data_coleta_dia", "data_coleta_mes", "data_coleta_ano"
#   )
# ]
# no_time_coord[, dataset_id := paste0("https://www.pedometria.org/febr/", dataset_id, "/")]
# data.table::fwrite(no_time_coord, "mapbiomas-soc/data/no-time-coord.csv", sep = "\t", dec = ",")

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
br_soil2023[, id := paste0(dataset_id, "-", observacao_id)]
idx_recovered <- match(br_soil2023[missing_time, id], recovered_time[["id"]])
br_soil2023[missing_time, data_coleta_ano := recovered_time[idx_recovered, data_coleta_ano]]

# Temporal distribution of samples with known sampling date after data rescue
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 3397 events remain without a known sampling date
br_soil2023[, na_year := FALSE]
br_soil2023[is.na(data_coleta_ano), na_year := TRUE]
missing_time <- is.na(br_soil2023[["data_coleta_ano"]])
if (FALSE) {
  x11()
  hist(br_soil2023[["data_coleta_ano"]], sub = paste0("n = ", sum(!missing_time)))
  rug(br_soil2023[["data_coleta_ano"]])
}

# Attribute the most likely temporal coordinate
# Inventário das terras em microbacias hidrográficas, Santa Catarina
# Target year: 1995
br_soil2023[
  grepl("Inventário das terras em microbacias hidrográficas", dataset_titulo, ignore.case = TRUE) &
    is.na(data_coleta_ano),
  data_coleta_ano := 1995
]
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 3323 event remaining without year

# LEVANTAMENTO SEMIDETALHADO DOS SOLOS DA FAZENDA CANCHIM SÃO CARLOS - SP.
# Target year: 1995
br_soil2023[dataset_id == "ctb0815" & is.na(data_coleta_ano), data_coleta_ano := 1995]
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 3240 events remain without sampling year

# Define an arbitrarily low year below the actual minimum
# Use this year as the value for events with NAs
# This allows these data to be shown in the histogram in a separate column from the other data.
year_min <- min(br_soil2023[, data_coleta_ano], na.rm = TRUE)
year_min <- (floor(year_min / 10) * 10) - 2

# RADAMBRASIL: set sampling year to year_min
idx <- br_soil2023[
  grepl("RADAMBRASIL", dataset_titulo, ignore.case = TRUE) & is.na(data_coleta_ano),
  id
]
br_soil2023[id %in% idx, data_coleta_ano := year_min]
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1819 events remain without sampling date

# Identify how many events 1) have spatial coordinates (coord_x and coord_y) and 2) do not have a
# sampling date (data_coleta_ano)
nrow(unique(br_soil2023[
  is.na(data_coleta_ano) & !is.na(coord_x) & !is.na(coord_y),
  c("dataset_id", "observacao_id")
]))
# 665 events

# Set the sampling year to 1999 for the following datasets:
# ctb0801
br_soil2023[dataset_id == "ctb0801" & is.na(data_coleta_ano), data_coleta_ano := 1999]
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1807 events

# Set the sampling year to 1998 for the following datasets:
# ctb0807
br_soil2023[dataset_id == "ctb0807" & is.na(data_coleta_ano), data_coleta_ano := 1998]
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1806 events

# Set the sampling year to 1994 for the following datasets:
# ctb0779
br_soil2023[dataset_id == "ctb0779" & is.na(data_coleta_ano), data_coleta_ano := 1994]
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1789 events

# Set the sampling year to 1991 for the following datasets:
# ctb0802
br_soil2023[dataset_id == "ctb0802" & is.na(data_coleta_ano), data_coleta_ano := 1991]
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1776 events

# Set the sampling year to 1989 for the following datasets:
# ctb0604
br_soil2023[dataset_id == "ctb0604" & is.na(data_coleta_ano), data_coleta_ano := 1989]
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1753 events

# Set the sampling year to 1983 for the following datasets:
# ctb0658
br_soil2023[dataset_id == "ctb0658" & is.na(data_coleta_ano), data_coleta_ano := 1983]
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1742 events

# Set the sampling year to 1981 for the following datasets:
# ctb0655
br_soil2023[dataset_id == "ctb0655" & is.na(data_coleta_ano), data_coleta_ano := 1981]
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1732 events

# Set the sampling year to 1980 for the following datasets:
# ctb0810, ctb0814
br_soil2023[dataset_id %in% c("ctb0810", "ctb0814") & is.na(data_coleta_ano), data_coleta_ano := 1980]
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1617 events

# Set the sampling year to 1978 for the following datasets:
# ctb0776, ctb0819
br_soil2023[dataset_id %in% c("ctb0776", "ctb0819") & is.na(data_coleta_ano), data_coleta_ano := 1978]
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1552 events

# Set the sampling year to 1977 for the following datasets:
# ctb0660, ctb0788
br_soil2023[dataset_id %in% c("ctb0660", "ctb0788") & is.na(data_coleta_ano), data_coleta_ano := 1977]
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1488 events

# Set the sampling year to 1976 for the following datasets:
# ctb0648, ctb0785
br_soil2023[dataset_id %in% c("ctb0648", "ctb0785") & is.na(data_coleta_ano), data_coleta_ano := 1976]
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1387 events remain without sampling date

# Set the sampling year to 1974 for the following datasets:
# ctb0789, ctb0818
br_soil2023[dataset_id %in% c("ctb0789", "ctb0818") & is.na(data_coleta_ano), data_coleta_ano := 1974]
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1331 events remain without sampling date

# Set the sampling year to 1971 for the following datasets:
# ctb0783, ctb0827
br_soil2023[dataset_id %in% c("ctb0783", "ctb0827") & is.na(data_coleta_ano), data_coleta_ano := 1971]
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1198 events remain without sampling date

# Set the sampling year to 1970 for the following datasets:
# ctb0797
br_soil2023[dataset_id == "ctb0797" & is.na(data_coleta_ano), data_coleta_ano := 1970]
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1141 events remain without sampling date

# Set the sampling year to 1969 for the following datasets:
# ctb0798
br_soil2023[dataset_id == "ctb0798" & is.na(data_coleta_ano), data_coleta_ano := 1969]
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1130 events remain without sampling date

# Set the sampling year to 1967 for the following datasets:
# ctb0693, ctb0804
br_soil2023[dataset_id %in% c("ctb0693", "ctb0804") & is.na(data_coleta_ano), data_coleta_ano := 1967]
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1106 events remain without sampling date

# Set the sampling year to 1959 for the following datasets
# ctb0787
br_soil2023[dataset_id == "ctb0787" & is.na(data_coleta_ano), data_coleta_ano := 1959]
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 1020 events remain without sampling date

# Set sampling year to year_min for the following datasets:
# ctb0023, ctb0028, ctb0603, ctb0608, ctb0635, ctb0666, ctb0682, ctb0829, ctb0702
ctb <- c("ctb0023", "ctb0028", "ctb0603", "ctb0608", "ctb0635", "ctb0666", "ctb0682", "ctb0829", "ctb0702")
br_soil2023[dataset_id %in% ctb & is.na(data_coleta_ano), data_coleta_ano := year_min]
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 848 events remain without sampling date

# Use the average sampling date of the source soil survey for the following datasets:
# ctb0030, ctb0032, ctb0570, ctb0572, ctb0574, ctb0617, ctb0631, ctb0639, ctb0642, ctb0645, ctb0656
# ctb0657, ctb0663, ctb0667, ctb0668, ctb0672, ctb0673, ctb0674, ctb0675, ctb0677, ctb0679, ctb0684
# ctb0686, ctb0691, ctb0694, ctb0700, ctb0750, ctb0774, ctb0775, ctb0777, ctb0781, ctb0795, ctb0808
# ctb0809, ctb0811, ctb0820, ctb0821, ctb0822, ctb0826, ctb0831, ctb0832
ctb <- c(
  "ctb0030", "ctb0032", "ctb0570", "ctb0572", "ctb0574", "ctb0617", "ctb0631", "ctb0639",
  "ctb0642", "ctb0645", "ctb0656", "ctb0657", "ctb0663", "ctb0667", "ctb0668", "ctb0672",
  "ctb0673", "ctb0674", "ctb0675", "ctb0677", "ctb0679", "ctb0684", "ctb0686", "ctb0691",
  "ctb0694", "ctb0700", "ctb0750", "ctb0774", "ctb0775", "ctb0777", "ctb0781", "ctb0795",
  "ctb0808", "ctb0809", "ctb0811", "ctb0820", "ctb0821", "ctb0822", "ctb0826", "ctb0831",
  "ctb0832"
)
average_year <- br_soil2023[dataset_id %in% ctb,
  .(data_coleta_ano = round(mean(data_coleta_ano, na.rm = TRUE))),
  by = dataset_id
]
idx_averaged <- match(
  br_soil2023[is.na(data_coleta_ano) & dataset_id %in% ctb, dataset_id],
  average_year[, dataset_id]
)
br_soil2023[
  is.na(data_coleta_ano) & dataset_id %in% ctb,
  data_coleta_ano := average_year[idx_averaged, data_coleta_ano]
]
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 66 events remain without sampling date

# Filter out rows missing the sampling date in the following datasets:
# ctb0009, ctb0029
ctb <- c("ctb0009", "ctb0029")
br_soil2023 <- br_soil2023[!(dataset_id %in% ctb & is.na(data_coleta_ano)), ]
nrow(unique(br_soil2023[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 0 events remain without sampling date

nrow(unique(br_soil2023[, c("dataset_id", "observacao_id")])) # 13977 events
nrow(br_soil2023) # 50 404 layers

# Temporal distribution of samples with known sampling date after data rescue
missing_time <- is.na(br_soil2023[["data_coleta_ano"]])
if (FALSE) {
  x11()
  x <- br_soil2023[, data_coleta_ano[1], by = c("dataset_id", "observacao_id")][, V1]
  hist(x,
    xlab = "Year", main = "Temporal distribution of events",
    sub = paste0("n = ", sum(!missing_time))
  )
  rug(x)
}

# Write data to disk
data.table::fwrite(br_soil2023, "mapbiomas-soc/data/10_soildata_soc.txt", sep = "\t")

# OLD //////////////////////////////////////////////////////////////////////////////////////////////
# rm(list = ls())
# 
# # Install and load required packages
# if (!require("data.table")) {
#   install.packages("data.table")
# }
# 
# # Read processed data (2021 FEBR snapshot)
# url <- "http://cloud.utfpr.edu.br/index.php/s/QpG6Tcr6x1NBOcI/download"
# temp <- tempfile(fileext = ".zip")
# download.file(url = url, destfile = temp)
# febr_data <- data.table::fread(unzip(temp), sep = ";", dec = ",")
# nrow(unique(febr_data[, c("dataset_id", "observacao_id")])) # 14 043 events
# nrow(febr_data) # 50 470 layers
# colnames(febr_data)
# 
# # Process time coordinate (sampling year)
# febr_data[, observacao_data := as.Date(observacao_data, format = "%Y-%m-%d")]
# febr_data[, data_coleta_dia := as.integer(format(observacao_data, "%d"))]
# febr_data[, data_coleta_mes := as.integer(format(observacao_data, "%m"))]
# febr_data[, data_coleta_ano := as.integer(format(observacao_data, "%Y"))]
# 
# # Clean odd sampling date
# febr_data[data_coleta_ano < 1950, data_coleta_ano := NA_integer_]
# 
# # Temporal distribution of samples with known sampling date
# nrow(unique(febr_data[, c("dataset_id", "observacao_id")]))
# nrow(unique(febr_data[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# # FEBR: 14 043 events, 4848 without sampling date
# missing_time <- is.na(febr_data[["data_coleta_ano"]])
# if (FALSE) {
#   x11()
#   hist(febr_data[["data_coleta_ano"]], sub = paste0("N = ", sum(!missing_time)))
#   rug(febr_data[["data_coleta_ano"]])
# }
# 
# # Write table to disk with events missing date
# # Only the surface layer (profund_sup == 0) of each event is exported.
# # The field dataset_id is reset as a URL to facilita the access to the respective webpage on FEBR.
# # The recovery of the sampling date will be done collectively by our team of data curators using a
# # Google Sheets spreadsheet to register the data.
# no_time_coord <- febr_data[missing_time & profund_sup == 0,
#   c("dataset_id", "dataset_titulo", "estado_id", "municipio_id", "observacao_id",
#     "data_coleta_dia", "data_coleta_mes", "data_coleta_ano")]
# no_time_coord[, dataset_id := paste0("https://www.pedometria.org/febr/", dataset_id, "/")]
# write.table(
#   x = no_time_coord,
#   file = "mapbiomas-soc/data/no-time-coord.csv",
#   sep = "\t", row.names = FALSE)
# 
# # Read Google Sheets spreadsheet containing the recovered sampling dates
# # It is not necessary to set the table because the spreadsheet contains only one.
# key <- "1UbuI_oMzFmclztmhZQYsuU0mn_Lx3NhSeBoFw0m4lv0"
# file <- paste0("http://docs.google.com/spreadsheets/d/", key, "/pub?output=csv")
# recovered_time <- data.table::fread(file, header = TRUE, na.strings = c("-", ""), sep = ",")
# recovered_time[, data_coleta_ano := as.integer(data_coleta_ano)]
# head(recovered_time)
# 
# # Check the range of values
# # Any error present in the downloaded data is corrected in the Google Sheets spreadsheet
# # Result: between 1957 and 2007
# range(recovered_time[["data_coleta_ano"]], na.rm = TRUE)
# 
# # Fill up the original table using the data recovered by our team of data curators
# recovered_time[, dados_id := gsub("https://www.pedometria.org/febr/", "", dados_id)]
# recovered_time[, dados_id := gsub("/", "", dados_id)]
# recovered_time[, id := paste0(dados_id, "-", evento_id_febr)]
# febr_data[, id := paste0(dataset_id, "-", observacao_id)]
# idx_recovered <- match(febr_data[missing_time, id], recovered_time[["id"]])
# febr_data[missing_time, data_coleta_ano := recovered_time[idx_recovered, data_coleta_ano]]
# 
# # Temporal distribution of samples with known sampling date after data rescue
# # In sum, we were able to recover the sampling date of more than 6000 samples
# # Result: 3402 events remain without a known sampling date
# nrow(unique(febr_data[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# febr_data[, na_year := FALSE]
# febr_data[is.na(data_coleta_ano), na_year := TRUE]
# missing_time <- is.na(febr_data[["data_coleta_ano"]])
# if (FALSE) {
#   x11()
#   hist(febr_data[["data_coleta_ano"]], sub = paste0("n = ", sum(!missing_time)))
#   rug(febr_data[["data_coleta_ano"]])
# }
# 
# # Attribute the most likely temporal coordinate
# # Use the mean of the source soil survey
# # Result: Using the average date, only 1869 event remain without sampling date
# average_year <- febr_data[,
#   .(data_coleta_ano = round(mean(data_coleta_ano, na.rm = TRUE))),
#   by = dataset_id]
# idx_averaged <- match(febr_data[missing_time, dataset_id], average_year[, dataset_id])
# febr_data[missing_time, data_coleta_ano := average_year[idx_averaged, data_coleta_ano]]
# nrow(unique(febr_data[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 
# # Temporal distribution of samples with known sampling date after data rescue
# missing_time <- is.na(febr_data[["data_coleta_ano"]])
# if (FALSE) {
#   x11()
#   hist(febr_data[["data_coleta_ano"]], sub = paste0("n = ", sum(!missing_time)))
#   rug(febr_data[["data_coleta_ano"]])
# }
# 
# # Inventário das terras em microbacias hidrográficas, Santa Catarina
# # Target year: 1995
# # Result: 1795 event remaining without year
# febr_data[
#   grepl("Inventário das terras em microbacias hidrográficas", dataset_titulo, ignore.case = TRUE) &
#     is.na(data_coleta_ano),
#   data_coleta_ano := 1995
# ]
# nrow(unique(febr_data[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 
# # LEVANTAMENTO SEMIDETALHADO DOS SOLOS DA FAZENDA CANCHIM SÃO CARLOS - SP.
# # Target year: 1995
# # Result: 1712 events remain without sampling year
# febr_data[dataset_id == "ctb0815" & is.na(data_coleta_ano), data_coleta_ano := 1995]
# nrow(unique(febr_data[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 
# # Define an arbitrarily low year below the actual minimum
# # Use this year as the value for events with NAs
# # This allows these data to be shown in the histogram in a separate column from the other data.
# year_min <- min(febr_data[, data_coleta_ano], na.rm = TRUE)
# year_min <- (floor(year_min / 10) * 10) - 2
# 
# # RADAMBRASIL: set sampling year to year_min
# # Result: 786 events still miss the sampling date
# idx <- febr_data[
#   grepl("RADAMBRASIL", dataset_titulo, ignore.case = TRUE) & is.na(data_coleta_ano),
#   id
# ]
# febr_data[
#   id %in% idx,
#   # data_coleta_ano := round(runif(length(idx), min = 1970, max = 1984))
#   data_coleta_ano := year_min
# ]
# nrow(unique(febr_data[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 
# # All other
# # Set a sampling year to year_min
# idx <- febr_data[is.na(data_coleta_ano), id]
# # febr_data[id %in% idx, data_coleta_ano := round(runif(n = length(idx), min = 1960, max = 1984))]
# febr_data[id %in% idx, data_coleta_ano := year_min]
# nrow(unique(febr_data[is.na(data_coleta_ano), c("dataset_id", "observacao_id")]))
# 
# # Temporal distribution of samples with known sampling date after data rescue
# missing_time <- is.na(febr_data[["data_coleta_ano"]])
# if (FALSE) {
#   x11()
#   hist(febr_data[["data_coleta_ano"]], sub = paste0("n = ", sum(!missing_time)))
#   rug(febr_data[data_coleta_ano != year_min, data_coleta_ano])
#   text(x = year_min - 0.5, y = -400, labels = "NAs")
# }
# 
# # Write data to disk
# data.table::fwrite(febr_data, "mapbiomas-soc/data/01a-febr-data.txt", sep = "\t", dec = ",")
