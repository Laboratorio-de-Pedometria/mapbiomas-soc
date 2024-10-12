# title: SoilData - Soil Organic Carbon Stock
# subtitle: Merge curated data
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2024 CC-BY
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}

# Source helper functions
source("src/00_helper_functions.r")

# Read SoilData data processed in the previous script
soildata <- data.table::fread("data/12_soildata_soc.txt", sep = "\t")
summary_soildata(soildata)
# Layers: 52545
# Events: 15222
# Georeferenced events: 12092

# Identify datasets missing the carbon content
na_carbon <- soildata[is.na(carbono), .N, by = dataset_id]
na_carbon[N > 50]

# ctb0004 ##########################################################################################
# Google Sheet
gs <- "17vSVemrAdkXL7il6x0729rd_fy7i9fga3uxdjwuUWWw"

# evento
gid <- 1628657862
ctb0004_event <- google_sheet(gs, gid)
colnames(ctb0004_event)
print(ctb0004_event)

# Check if there is more than one coordinate reference system
# Datum (coord)
ctb0004_event[, .N, by = "Datum (coord)"]

# Prepare columns
colnames(soildata)
colnames(ctb0004_event)
ctb0004_event[, dataset_id := "ctb0004"]
# ID do evento -> observacao_id
data.table::setnames(ctb0004_event, old = "ID do evento", new = "observacao_id")
ctb0004_event[, id := paste0(dataset_id, "-", observacao_id)]
# Longitude -> coord_x
data.table::setnames(ctb0004_event, old = "Longitude", new = "coord_x")
# Latitude -> coord_y
data.table::setnames(ctb0004_event, old = "Latitude", new = "coord_y")
# Precisão (coord) [m] -> coord_precisao
data.table::setnames(ctb0004_event, old = "Precisão (coord) [m]", new = "coord_precisao")
# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0004_event, old = "Fonte (coord)", new = "coord_fonte")
# Estado (UF) -> estado_id
data.table::setnames(ctb0004_event, old = "Estado (UF)", new = "estado_id")
ctb0004_event[, estado_id := "PE"]
# Área amostrada [m^2] -> amostra_area
data.table::setnames(ctb0004_event, old = "Área amostrada [m^2]", new = "amostra_area")
# taxon_sibcs
ctb0004_event[, taxon_sibcs := NA_character_]
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0004_event, old = "Ano (coleta)", new = "data_coleta_ano")
# Datum (coord) -> coord_datum_epsg
data.table::setnames(ctb0004_event, old = "Datum (coord)", new = "coord_datum_epsg")

# camada
gid <- 771766248
ctb0004_layer <- google_sheet(gs, gid)
colnames(ctb0004_layer)
print(ctb0004_layer)

# Prepare columns
colnames(soildata)
colnames(ctb0004_layer)
# ID do evento -> observacao_id
data.table::setnames(ctb0004_layer, old = "ID do evento", new = "observacao_id")
# ID da camada -> camada_nome
data.table::setnames(ctb0004_layer, old = "ID da camada", new = "camada_nome")
ctb0004_layer[, camada_id := camada_nome]
# ID da amostra -> amostra_id
data.table::setnames(ctb0004_layer, old = "ID da amostra", new = "amostra_id")
# Profundidade inicial [cm] -> profund_sup
data.table::setnames(ctb0004_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
# Profundidade final [cm] -> profund_inf
data.table::setnames(ctb0004_layer, old = "Profundidade final [cm]", new = "profund_inf")
# terrafina
ctb0004_layer[, terrafina := 1000]
# Clay -> argila
data.table::setnames(ctb0004_layer, old = "Clay", new = "argila")
ctb0004_layer[, argila := argila * 10]
# Silt -> silte
data.table::setnames(ctb0004_layer, old = "Silt", new = "silte")
ctb0004_layer[, silte := silte * 10]
# Sand -> areia
data.table::setnames(ctb0004_layer, old = "Sand", new = "areia")
ctb0004_layer[, areia := areia * 10]
# Org. C -> carbono
data.table::setnames(ctb0004_layer, old = "Org. C", new = "carbono")
# CEC -> ctc
data.table::setnames(ctb0004_layer, old = "CEC", new = "ctc")
# pH em H_2O -> ph
data.table::setnames(ctb0004_layer, old = "pH em H_2O", new = "ph")
# Bulk density -> dsi
ctb0004_layer[, dsi := NA_real_]

# Merge events and layers
ctb0004 <- merge(ctb0004_event, ctb0004_layer, all = TRUE)
summary_soildata(ctb0004)
# Layers: 92
# Events: 40
# Georeferenced events: 40

# Replace the dataset in the soildata object
summary_soildata(soildata)
# Layers: 52545
# Events: 15222
# Georeferenced events: 12092

soildata <- soildata[dataset_id != "ctb0004"]
summary_soildata(soildata)
# Layers: 52473
# Events: 15186
# Georeferenced events: 12056

which_cols <- colnames(ctb0004) %in% colnames(soildata)
soildata <- rbind(soildata, ctb0004[, ..which_cols])
summary_soildata(soildata)
# Layers: 52565
# Events: 15226
# Georeferenced events: 12096

# ctb0006 ##########################################################################################

# ownCloud/febr-repo/publico/ctb0006/ctb0006-observacao.txt
file_path <- path.expand("~/ownCloud/febr-repo/publico/ctb0006/ctb0006-observacao.txt")
ctb0006_event <- data.table::fread(file_path, dec = ",")
colnames(ctb0006_event)

# Some events have wrong records in the column observacao_data 
ctb0006_event[observacao_data > 2014, observacao_data := 2014]

# Harmonize the coordinate reference system as registered in the coord_sistema column
# Start by splitting the dataset according to the coordinate reference system. Then, transform each
# subset to a spatial object with the respective coordinate reference system. Next, transform
# the coordinate reference system to WGS 84 (EPSG:4326). Finally, merge the subsets back into a
# single dataset and convert it to a data.table, including the coordinates in the data.table object.
# The coordinate reference system is reset to EPSG:4326.
ctb0006_event <- split(ctb0006_event, by = "coord_sistema")
ctb0006_event <- lapply(ctb0006_event, function(x) {
  x <- sf::st_as_sf(x, coords = c("coord_x", "coord_y"), crs = x$coord_sistema[1])
  x <- sf::st_transform(x, crs = 4326)
  dt <- data.table::as.data.table(x)
  xy <- sf::st_coordinates(x)
  x <- cbind(dt, coord_x = xy[, 1], coord_y = xy[, 2])
  x[, geometry := NULL]
  # Reset the coordinate reference system
  x$coord_sistema <- 4326
  return(x)
})
ctb0006_event <- data.table::rbindlist(ctb0006_event, fill = TRUE)
print(ctb0006_event)

# Prepare columns names
ctb0006_event[, dataset_id := "ctb0006"]
data.table::setnames(ctb0006_event, old = "observacao_data", new = "data_coleta_ano")
ctb0006_event[, coord_precisao := NA]
ctb0006_event[, coord_fonte := NA]
ctb0006_event[, amostra_area := NA]
data.table::setnames(ctb0006_event, old = "taxon_sibcs_2013", new = "taxon_sibcs")
data.table::setnames(ctb0006_event, old = "coord_sistema", new = "coord_datum_epsg")
ctb0006_event[, id := paste0(dataset_id, "-", observacao_id)]
print(ctb0006_event)

# ownCloud/febr-repo/publico/ctb0006/ctb0006-camada.txt
file_path <- path.expand("~/ownCloud/febr-repo/publico/ctb0006/ctb0006-camada.txt")
ctb0006_layer <- data.table::fread(file_path, na.strings = c("NA", "NaN", "-"), sep = "\t")
print(ctb0006_layer)

# Prepare columns names
colnames(soildata)
colnames(ctb0006_layer)
ctb0006_layer[, profund_sup := as.numeric(profund_sup)]
ctb0006_layer[, profund_inf := as.numeric(profund_inf)]
data.table::setnames(ctb0006_layer, old = "terrafina_xxx", new = "terrafina")
ctb0006_layer[, terrafina := as.numeric(terrafina)]
# argila_xxx_xxx -> argila
data.table::setnames(ctb0006_layer, old = "argila_xxx_xxx", new = "argila")
ctb0006_layer[, argila := as.numeric(argila)]
# silte_xxx_xxx -> silte
data.table::setnames(ctb0006_layer, old = "silte_xxx_xxx", new = "silte")
ctb0006_layer[, silte := as.numeric(silte)]
ctb0006_layer[, areia := as.numeric(areiagrossa2_xxx_xxx) + as.numeric(areiafina2_xxx_xxx)]
# carbono_xxx_xxx_xxx -> carbono
data.table::setnames(ctb0006_layer, old = "carbono_xxx_xxx_xxx", new = "carbono")
ctb0006_layer[, carbono := as.numeric(carbono)]
# ctc_soma_calc -> ctc
data.table::setnames(ctb0006_layer, old = "ctc_soma_calc", new = "ctc")
ctb0006_layer[, ctc := as.numeric(ctc)]
# ph_h2o_25_eletrodo -> ph
data.table::setnames(ctb0006_layer, old = "ph_h2o_25_eletrodo", new = "ph")
ctb0006_layer[, ph := as.numeric(ph)]
# dsi
ctb0006_layer[, dsi := NA_real_]
print(ctb0006_layer)

# Merge events and layers
ctb0006 <- merge(ctb0006_event, ctb0006_layer, all = TRUE)
summary_soildata(ctb0006)
# Layers: 826
# Events: 605
# Georeferenced events: 605

# Convert to sf and check if the data is correctly georeferenced
if (FALSE) {
  ctb0006_sf <- sf::st_as_sf(ctb0006, coords = c("coord_x", "coord_y"), crs = 4326)
  mapview::mapview(ctb0006_sf)
}

# Replace the dataset in the soildata object
summary_soildata(soildata)
# Layers: 52565
# Events: 15226
# Georeferenced events: 12096
soildata <- soildata[dataset_id != "ctb0006"]
summary_soildata(soildata)
# Layers: 52272
# Events: 15154
# Georeferenced events: 12024
which_cols <- colnames(ctb0006) %in% colnames(soildata)
soildata <- rbind(soildata, ctb0006[, ..which_cols])
summary_soildata(soildata)
# Layers: 53098
# Events: 15759
# Georeferenced events: 12629

# ctb0007 ##########################################################################################

# Google Sheet
# Spreadsheet ID:
gs <- "1yH3S0lFCCGFQ2JupPH_5KAoleBA-v5zoU7RRlfzvNyo"

# evento
gid <- 1628657862
ctb0007_event <- google_sheet(gs, gid)
print(ctb0007_event)
colnames(ctb0007_event)

# Check if there is more than one CRS
# There actually are no coordinates in this dataset
ctb0007_event[, .N, by = "Datum (coord)"]

# Prepare columns
colnames(soildata)
colnames(ctb0007_event)
ctb0007_event[, dataset_id := "ctb0007"]
# ID do evento -> observacao_id
data.table::setnames(ctb0007_event, old = "ID do evento", new = "observacao_id")
ctb0007_event[, id := paste0(dataset_id, "-", observacao_id)]
# Longitude -> coord_x
data.table::setnames(ctb0007_event, old = "Longitude", new = "coord_x")
# Latitude -> coord_y
data.table::setnames(ctb0007_event, old = "Latitude", new = "coord_y")
# Precisão (coord) [m] -> coord_precisao
data.table::setnames(ctb0007_event, old = "Precisão (coord) [m]", new = "coord_precisao")
# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0007_event, old = "Fonte (coord)", new = "coord_fonte")
# Estado (UF) -> estado_id
data.table::setnames(ctb0007_event, old = "Estado (UF)", new = "estado_id")
ctb0007_event[, estado_id := "RS"]
# Área amostrada [m^2] -> amostra_area
data.table::setnames(ctb0007_event, old = "Área amostrada [m^2]", new = "amostra_area")
# taxon_sibcs
ctb0007_event[, taxon_sibcs := NA_character_]
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0007_event, old = "Ano (coleta)", new = "data_coleta_ano")
# Datum (coord) -> coord_datum_epsg
data.table::setnames(ctb0007_event, old = "Datum (coord)", new = "coord_datum_epsg")
str(ctb0007_event, 1)

# camada
gid <- 771766248
ctb0007_layer <- google_sheet(gs, gid)
print(ctb0007_layer)
colnames(ctb0007_layer)

# Prepare columns
colnames(soildata)
colnames(ctb0007_layer)
# ID do evento -> observacao_id
data.table::setnames(ctb0007_layer, old = "ID do evento", new = "observacao_id")
# ID da camada -> camada_nome
data.table::setnames(ctb0007_layer, old = "ID da camada", new = "camada_nome")
ctb0007_layer[, camada_id := camada_nome]
# ID da amostra -> amostra_id
data.table::setnames(ctb0007_layer, old = "ID da amostra", new = "amostra_id")
# Profundidade inicial [cm] -> profund_sup
data.table::setnames(ctb0007_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
# Profundidade final [cm] -> profund_inf
data.table::setnames(ctb0007_layer, old = "Profundidade final [cm]", new = "profund_inf")
# terrafina
ctb0007_layer[, terrafina := 1000]
# Argila -> argila
data.table::setnames(ctb0007_layer, old = "Argila", new = "argila")
# Silte -> silte
data.table::setnames(ctb0007_layer, old = "Silte", new = "silte")
# Areia -> areia
data.table::setnames(ctb0007_layer, old = "Areia", new = "areia")
# MO -> carbono
data.table::setnames(ctb0007_layer, old = "MO", new = "carbono")
ctb0007_layer[, carbono := carbono * 0.58]
# CTC pH7 -> ctc
data.table::setnames(ctb0007_layer, old = "CTC pH7", new = "ctc")
# pH em H_2O -> ph
data.table::setnames(ctb0007_layer, old = "pH em H_2O", new = "ph")
# dsi
ctb0007_layer[, dsi := NA_real_]

# Merge events and layers
ctb0007 <- merge(ctb0007_event, ctb0007_layer, all = TRUE)
summary_soildata(ctb0007)
# Layers: 24
# Events: 12
# Georeferenced events: 0

# Replace the dataset in the soildata object
summary_soildata(soildata)
# Layers: 53098
# Events: 15759
# Georeferenced events: 12629
soildata <- soildata[dataset_id != "ctb0007"]
summary_soildata(soildata)
# Layers: 53074
# Events: 15747
# Georeferenced events: 12617
which_cols <- colnames(ctb0007) %in% colnames(soildata)
soildata <- rbind(soildata, ctb0007[, ..which_cols])
summary_soildata(soildata)
# Layers: 53098
# Events: 15759
# Georeferenced events: 12617

# ctb0008 ##########################################################################################
# wait list

# ctb0010 ##########################################################################################
# wait list

# ctb0011 ##########################################################################################

# Google Sheet
# Spreadsheet ID: 2PACX-1vTvGg1x5jvf_rLhGCLTu2-U-t1AYJnnawmhc3rL3-MXrk2S8Vpj5iYo4Fxl4f3qDOMTt9JmV1PyoTvm

# evento
# GID: 1628657862

# camada
# GID: 771766248

# ctb0012 ##########################################################################################
# wait list

# ctb0013 ##########################################################################################
# wait list

# ctb0014 ##########################################################################################
# wait list

# ctb0015 ##########################################################################################
# wait list

# ctb0016 ##########################################################################################
# wait list

# ctb0017 ##########################################################################################

# Google Sheet
# Spreadsheet ID: 2PACX-1vTZpuABZxTzofGnjbC1xW3IvtSY_zC13SI6ftvkk1Zu1_kmVm_FDWVJ6c4yP3zdGV5FP2_Y_PU6grtC

# evento
# GID: 1628657862



# camada
# GID: 771766248

# ctb0025 ##########################################################################################
# ownCloud/febr-repo/publico/ctb0025/ctb0025-observacao.txt
file_path <- path.expand("~/ownCloud/febr-repo/publico/ctb0025/ctb0025-observacao.txt")
ctb0025_event <- data.table::fread(file_path, dec = ",")
colnames(ctb0025_event)
print(ctb0025_event)

# Observations date
ctb0025_event[, observacao_data := as.Date(observacao_data, origin = "1900-01-01", format = "%Y-%m-%d")]
ctb0025_event[, data_coleta_ano := as.integer(format(observacao_data, "%Y"))]

# Check if there is more than one coord_datum_epsg
ctb0025_event[, .N, by = coord_datum_epsg]
# Transform the coordinate reference system to EPSG:4326
ctb0025_event <- sf::st_as_sf(ctb0025_event, coords = c("coord_longitude", "coord_latitude"), crs = 4674)
ctb0025_event <- sf::st_transform(ctb0025_event, crs = 4326)
if (FALSE) {
  mapview::mapview(ctb0025_event)
}
dt <- data.table::as.data.table(ctb0025_event)
xy <- sf::st_coordinates(ctb0025_event)
ctb0025_event <- cbind(dt, coord_x = xy[, 1], coord_y = xy[, 2])
ctb0025_event[, geometry := NULL]

# Prepare columns names
colnames(soildata)
colnames(ctb0025_event)
ctb0025_event[, dataset_id := "ctb0025"]
ctb0025_event[, id := paste0(dataset_id, "-", observacao_id)]
# estado_sigla -> estado_id
data.table::setnames(ctb0025_event, old = "estado_sigla", new = "estado_id")
# amostra_area
ctb0025_event[, amostra_area := NA]
# taxon_sibcs_2006 -> taxon_sibcs
data.table::setnames(ctb0025_event, old = "taxon_sibcs_2006", new = "taxon_sibcs")

# ownCloud/febr-repo/publico/ctb0025/ctb0025-camada.txt
file_path <- path.expand("~/ownCloud/febr-repo/publico/ctb0025/ctb0025-camada.txt")
ctb0025_layer <- data.table::fread(file_path, na.strings = c("NA", "NaN", "-"), sep = "\t")
print(ctb0025_layer)

# Prepare columns
colnames(soildata)
colnames(ctb0025_layer)
ctb0025_layer[, profund_sup := as.numeric(profund_sup)]
ctb0025_layer[, profund_inf := as.numeric(profund_inf)]
# terrafina_peneira -> terrafina
data.table::setnames(ctb0025_layer, old = "terrafina_peneira", new = "terrafina")
ctb0025_layer[, terrafina := as.numeric(terrafina)]
# argila_naoh_densimetro -> argila
data.table::setnames(ctb0025_layer, old = "argila_naoh_densimetro", new = "argila")
ctb0025_layer[, argila := as.numeric(argila)]
# silte_0002mm0050mm_calc -> silte
data.table::setnames(ctb0025_layer, old = "silte_0002mm0050mm_calc", new = "silte")
ctb0025_layer[, silte := as.numeric(silte)]
# areia
ctb0025_layer[, areia := as.numeric(areia_0200mm2000mm_calc) + as.numeric(areia_0050mm0200mm_peneira)]
# carbono_cromo_xxx_mohr -> carbono
data.table::setnames(ctb0025_layer, old = "carbono_cromo_xxx_mohr", new = "carbono")
ctb0025_layer[, carbono := as.numeric(carbono)]
# ctc_soma_calc -> ctc
data.table::setnames(ctb0025_layer, old = "ctc_soma_calc", new = "ctc")
ctb0025_layer[, ctc := as.numeric(ctc)]
# ph_h2o_25_eletrodo -> ph
data.table::setnames(ctb0025_layer, old = "ph_h2o_25_eletrodo", new = "ph")
ctb0025_layer[, ph := as.numeric(ph)]
# dsi
ctb0025_layer[, dsi := NA_real_]

# Merge events and layers
ctb0025 <- merge(ctb0025_event, ctb0025_layer, all = TRUE)
summary_soildata(ctb0025)
# Layers: 669
# Events: 237
# Georeferenced events: 237

# Replace the dataset in the soildata object
summary_soildata(soildata)
# Layers: 53078
# Events: 15755
# Georeferenced events: 12625

soildata <- soildata[dataset_id != "ctb0025"]
summary_soildata(soildata)
# Layers: 52483
# Events: 15592
# Georeferenced events: 12462

which_cols <- colnames(ctb0025) %in% colnames(soildata)
soildata <- rbind(soildata, ctb0025[, ..which_cols])
summary_soildata(soildata)
# Layers: 53152
# Events: 15829
# Georeferenced events: 12699


# Export cleaned data
data.table::fwrite(soildata, "data/13_soildata_soc.txt", sep = "\t")