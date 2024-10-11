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

# ctb0006
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

# Convert to sf and check if the data is correctly georeferenced
if (FALSE) {
  ctb0006_sf <- sf::st_as_sf(ctb0006, coords = c("coord_x", "coord_y"), crs = 4326)
  mapview::mapview(ctb0006_sf)
}

# Replace the dataset in the soildata object
soildata <- soildata[dataset_id != "ctb0006"]
which_cols <- colnames(ctb0006) %in% colnames(soildata)
soildata <- rbind(soildata, ctb0006[, ..which_cols])

# Export cleaned data
summary_soildata(soildata)
# Layers: 53078
# Events: 15755
# Georeferenced events: 12625
data.table::fwrite(soildata, "data/13_soildata_soc.txt", sep = "\t")

# ctb0004
# 2PACX-1vTH9FV04ZPJeHZ0fK7IypABKSGm9NoWZkBKGqjH_TjIdhBo8Er_Fy25IstZp2cb9_Ts8CQ_L4SC78TT
# evento (dados)
# camada (dados)

# ctb0017
# 2PACX-1vTZpuABZxTzofGnjbC1xW3IvtSY_zC13SI6ftvkk1Zu1_kmVm_FDWVJ6c4yP3zdGV5FP2_Y_PU6grtC
# evento
# camada

# ctb0025
# /home/alessandro/ownCloud/febr-repo/publico/ctb0025
# /home/alessandro/ownCloud/febr-repo/publico/ctb0025/ctb0025-observacao.txt
# /home/alessandro/ownCloud/febr-repo/publico/ctb0025/ctb0025-camada.txt


