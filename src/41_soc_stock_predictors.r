# title: SoilData - Soil Organic Carbon Stock
# subtitle: Prepare SOC stock predictors
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2024 CC-BY
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}

# Source helper functions
source("src/00_helper_functions.r")

# Read data processed in the previous script
soildata <- data.table::fread("data/40_soildata_soc.txt", sep = "\t")
summary_soildata(soildata)
# Layers: 16743
# Events: 10378
# Georeferenced events: 8311

# Carbon stocks per event
# Aggregation of carbon stocks in the top layer (0 to 30 cm) of each event
# Computation of covariates
soildata_soc_stock <- soildata[
  ,
  .(
    # Target variables
    soc_stock_kgm2 = sum(soc_stock_kgm2, na.rm = TRUE), # time
    endpoint = mean(endpoint), # event (1 = event occurred, 0 = event did not occur)
    # Profile wise predictor variables
    dataset_id = unique(dataset_id),
    year = as.integer(round(mean(data_coleta_ano, na.rm = TRUE))),
    coord_x = mean(coord_x, na.rm = TRUE),
    coord_y = mean(coord_y, na.rm = TRUE),
    br_state = unique(estado_id),
    max_depth = max(profund_inf),
    order = unique(ORDER),
    suborder = unique(SUBORDER),
    lulc = unique(lulc)
  ),
  by = id
]

# Set end point
# If a profile has a max_depth of 30 cm, then set end_point to 1
nrow(soildata_soc_stock[endpoint == 1, ]) # Result: 176 events
soildata_soc_stock[max_depth == 30, endpoint := 1]
nrow(soildata_soc_stock[endpoint == 1, ]) # Result: 6198 events
nrow(soildata_soc_stock[is.na(endpoint), ]) # Result: 0 events
soildata_soc_stock[is.na(endpoint), endpoint := 0]

# Write data to disk
summary_soildata(soildata_soc_stock)
# Layers: 10378
# Events: 10378
# Georeferenced events: 8311
data.table::fwrite(soildata_soc_stock, "data/41_soildata_soc.txt", sep = "\t")


# PREVIOUS /////////////////////////////////////////////////////////////////////////////////////////
# # Agregar estoque de carbono na camada superficial (0 até 30 cm) de cada evento
# colnames(febr_data)
# febr_data <- febr_data[
#   !is.na(carbono_estoque_kgm2) &
#     espessura > 0 &
#     !is.na(coord_x) &
#     !is.na(coord_y) &
#     !is.na(data_coleta_ano),
#   .(
#     cos_estoque_gm2 = as.integer(round(sum(carbono_estoque_kgm2, na.rm = TRUE) * 1000)),
#     data_coleta_ano = as.integer(round(mean(data_coleta_ano, na.rm = TRUE))),
#     coord_x = mean(coord_x, na.rm = TRUE),
#     coord_y = mean(coord_y, na.rm = TRUE),
#     espessura = sum(espessura),
#     FOREST = unique(FOREST),
#     NONFOREST = unique(NONFOREST),
#     PASTURE = unique(PASTURE),
#     AGRICULTURE = unique(AGRICULTURE),
#     FORESTRY = unique(FORESTRY),
#     NONVEGETATION = unique(NONVEGETATION)
#   ),
#   by = id
# ]
# nrow(febr_data) # Result: 8904 events/layers
# 
# if (FALSE) {
#   x11()
#   hist(febr_data[, cos_estoque_gm2] / 1000)
#   rug(febr_data[, cos_estoque_gm2] / 1000)
# }

# Filter out duplicated events
# Dataset ctb0829 used events from the RADAMBRASIL project. Those events are erroneously assigned
# the sampling year 2000. We will correct this by removing those events.
test_columns <- c("coord_x", "coord_y")
duplicates <- duplicated(febr_data[, ..test_columns])
# Identify duplicates with id starting with "ctb0829"
duplicates <- duplicates & grepl("ctb0829", febr_data$id)
sum(duplicates) # 34 duplicated events
febr_data <- febr_data[!duplicates, ]
nrow(febr_data) # 8870 events/layers

# Other datasets used events from the RADAMBRASIL project. Those events are erroneously assigned
# the sampling year. We will correct this by removing those events.
# ctb0702
febr_data <- febr_data[id != "ctb0702-P-86", ]
febr_data <- febr_data[id != "ctb0702-P-97", ]
# ctb0683
febr_data <- febr_data[id != "ctb0683-232", ]
nrow(febr_data) # 8867 events/layers

# Other events with equal coordinates will be jittered (add small noise)
test_columns <- c("coord_x", "coord_y")
duplicates <- duplicated(febr_data[, ..test_columns])
sum(duplicates) # 25 duplicated events
febr_data[duplicates, coord_x := coord_x + runif(.N, -0.0001, 0.0001)]
febr_data[duplicates, coord_y := coord_y + runif(.N, -0.0001, 0.0001)]
nrow(febr_data) # 8867 events

# Write data to disk
data.table::fwrite(febr_data, "mapbiomas-soc/data/06-febr-data.txt", sep = "\t", dec = ",")
data.table::fwrite(
  febr_data[, c("id", "cos_estoque_gm2", "data_coleta_ano", "coord_x", "coord_y")],
  paste0("mapbiomas-soc/res/tab/", format(Sys.time(), "%Y-%m-%d"), "-pontos-estoque-cos.csv")
)
