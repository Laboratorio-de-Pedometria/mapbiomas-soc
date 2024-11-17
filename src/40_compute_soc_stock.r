# title: SoilData - Soil Organic Carbon Stock
# subtitle: Compute soil organic carbon stocks
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
soildata <- data.table::fread("data/31_soildata_soc.txt", sep = "\t")
summary_soildata(soildata)
# Layers: 29683
# Events: 15640
# Georeferenced events: 13292

# THIS HAS ALREADY BEEN CORRECTED IN THE ORIGINAL DATASET. WE KEEP IT HERE FOR REFERENCE.
soildata[
  dataset_id == "ctb0607" & observacao_id == "PERFIL-92",
  carbono := ifelse(carbono == 413, 41.3, carbono)
]

# Remove data from Technosols and Anthrosols
# Solo construído no aterro encerrado da Caturrita, Santa Maria (RS)
soildata <- soildata[dataset_id != "ctb0036", ]

# Projeto Parque Frei Veloso - Levantamento Detalhado dos Solos do Campus da Ilha do Fundão UFRJ
soildata <- soildata[dataset_id != "ctb0599", ]

# Projeto Caldeirão: caracterização e gênese das Terras Pretas de Índio na Amazônia
soildata <- soildata[dataset_id != "ctb0018", ]

summary_soildata(soildata)
# Layers: 29365
# Events: 15453
# Georeferenced events: 13120

# Filter out soil layers missing data on soil organic carbon
is_na_carbono <- is.na(soildata[["carbono"]])
sum(is_na_carbono) # Result: 1785 layers
soildata <- soildata[!is_na_carbono, ]
summary_soildata(soildata)
# Layers: 27580
# Events: 14672
# Georeferenced events: 12526

# Topsoil --> BECAUSE WE REMOVED LAYERS WITH MISSING DATA ON SOIL ORGANIC CARBON
# For each event (id), check if there is a layer with profund_sup == 0. Filter out events without a
# topsoil layer. This procedure was performed before: it is repeated here because layers missing
# data on soil organic carbon were removed (there may be other reasons too).
soildata[, topsoil := any(profund_sup == 0), by = id]
nrow(unique(soildata[topsoil == FALSE, "id"])) # 79 events
soildata <- soildata[topsoil == TRUE, ]
soildata[, topsoil := NULL]
summary_soildata(soildata)
# Layers: 27466
# Events: 14593
# Georeferenced events: 12486

# Empty layers --> ALSO BECAUSE WE REMOVED LAYERS WITH MISSING DATA ON SOIL ORGANIC CARBON
# Is there any soil profile (id) missing an intermediate layer? A missing intermediate layer occurs
# when profund_inf of layer i is different from profund_sup of layer i + 1. Many of these profiles
# are complementary/extra observations that sampled only the topsoil and the subsoil, e.g. 0-20 and
# 60-80 cm. They are interval censored. For further analysis, we drop the lowermost layers.
soildata[, empty_layer := shift(profund_inf, type = "lag") != profund_sup, by = id]
nrow(unique(soildata[empty_layer == TRUE, "id"])) # Result: 1382 events
soildata <- soildata[empty_layer == FALSE | is.na(empty_layer), ]
soildata <- soildata[, empty_layer := NULL]
summary_soildata(soildata)
# Layers: 26084
# Events: 14593
# Georeferenced events: 12486

# Layer limits
# Resetting the limits of each layer according to the target depth range (0 and 30 cm).
# Source: https://github.com/ncss-tech/aqp/blob/master/R/depthWeights.R
# The thickness of each layer is recalculated.
target_layer <- c(0, 30)
soildata[profund_sup < target_layer[1], profund_sup := target_layer[1]]
soildata[profund_inf < target_layer[1], profund_inf := target_layer[1]]
soildata[profund_sup > target_layer[2], profund_sup := target_layer[2]]
soildata[profund_inf > target_layer[2], profund_inf := target_layer[2]]
# Recompute layer thickness
soildata[, espessura := profund_inf - profund_sup]
nrow(soildata[espessura <= 0, ]) # 2882 layers with thickness <= 0 --> remove them
soildata <- soildata[espessura > 0, ]
summary_soildata(soildata)
# Layers: 23202
# Events: 14593
# Georeferenced events: 12486

# Volume of coarse fragments
# The volume of coarse fragments is calculated as the volume of the skeleton divided by the density
# of the skeleton (2.65 g/cm^3) and converted to m^3.
soildata[, fragmentos := esqueleto / 2.65 / 1000]
# soildata[dataset_id == "ctb0054", fragmentos := 0]

# Compute soil organic carbon stock (kg/m^2) in each layer
# Source: T. Hengl et al., “SoilGrids1km–global soil information based on automated mapping,” PLoS
# ONE, vol. 9, no. 8, p. e105992, 2014, doi: 10.1371/journal.pone.0105992.
# The first step is to transform the data from fine earth content (g/kg) to volume (cm^3/cm^3). This
# is done assuming that the density of coarse fragments is 2.65 g/cm^3.
# g/kg / g/cm^3
# 1) carbono / 1000: kg/kg
# 2) espessura / 100: m
# 3) dsi * 1000: kg/m^3
# 4) fragmentos: 1
soildata[, soc_stock_kgm2 := (carbono / 1000) * (espessura / 100) * (dsi * 1000) * (1 - fragmentos)]
if (FALSE) {
  x11()
  hist(soildata[, soc_stock_kgm2])
  rug(soildata[, soc_stock_kgm2])
}

# Layers per event
# Count the number of layers per event (id)
soildata[, n := .N, by = "id"]
table(soildata[, n])
#     1     2     3     4     5 
#  7745 10398  4620   424    15 
soildata[, n := NULL]

# Aggregate soil organic carbon stock in the topsoil (0 to 30 cm) of each event
colnames(soildata)
soc_data <- soildata[
  !is.na(soc_stock_kgm2) &
    espessura > 0 &
    !is.na(coord_x) &
    !is.na(coord_y) &
    !is.na(data_ano),
  .(
    soc_stock_g_m2 = round(sum(soc_stock_kgm2, na.rm = TRUE) * 1000),
    year = as.integer(round(mean(data_ano, na.rm = TRUE))),
    coord_x = mean(coord_x, na.rm = TRUE),
    coord_y = mean(coord_y, na.rm = TRUE)
  ),
  by = id
]
summary(soc_data[, soc_stock_g_m2])
summary_soildata(soc_data)
# Layers: 12486
# Events: 12486
# Georeferenced events: 12486

# The following points have very high SOC stock. We create new nearby instances.
# ctb0832-226: ok
# -21.23333333, -41.31666667 (ORIGINAL)
# -21.232867, -41.316707
# -21.233262, -41.316175
# -21.233265, -41.315750
# -21.233301, -41.314682
tmp <- soc_data[id == "ctb0832-226"][rep(1, 4)]
tmp[, id := paste0(id, "-REP", 1:4)]
tmp[, coord_y := c(-21.232867, -21.233262, -21.233265, -21.233301)]
tmp[, coord_x := c(-41.316707, -41.316175, -41.315750, -41.314682)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# ctb0691-15: ok
# -20.270007, -40.277173 (ORIGINAL)
# -20.270420, -40.278063
# -20.262183, -40.285028
# -20.270310, -40.281488
# -20.272343, -40.280716
tmp <- soc_data[id == "ctb0691-15"][rep(1, 4)]
tmp[, id := paste0(id, "-REP", 1:4)]
tmp[, coord_y := c(-20.270420, -20.262183, -20.270310, -20.272343)]
tmp[, coord_x := c(-40.278063, -40.285028, -40.281488, -40.280716)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# ctb0662-P89: ok
# -19.5616, -39.8885 (ORIGINAL)
# -19.561325, -39.889315
# -19.561086, -39.889300
# -19.563407, -39.890190
# -19.562224, -39.890426
tmp <- soc_data[id == "ctb0662-P89"][rep(1, 4)]
tmp[, id := paste0(id, "-REP", 1:4)]
tmp[, coord_y := c(-19.561325, -19.561086, -19.563407, -19.562224)]
tmp[, coord_x := c(-39.889315, -39.889300, -39.890190, -39.890426)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# ctb0617-Perfil-49: ok
# -19.505398, -47.788986 (ORIGINAL)
# -19.503990, -47.782874
# -19.504975, -47.791988
# -19.500990, -47.780961
# -19.504005, -47.794110
tmp <- soc_data[id == "ctb0617-Perfil-49"][rep(1, 4)]
tmp[, id := paste0(id, "-REP", 1:4)]
tmp[, coord_y := c(-19.503990, -19.504975, -19.500990, -19.504005)]
tmp[, coord_x := c(-47.782874, -47.791988, -47.780961, -47.794110)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# ctb0777-41: ok
# -13.070637, -46.0070019 (ORIGINAL)
# -13.066117, -46.007771
# -13.073140, -46.004166
# -13.078574, -45.999789
# -13.083507, -45.983137
tmp <- soc_data[id == "ctb0777-41"][rep(1, 4)]
tmp[, id := paste0(id, "-REP", 1:4)]
tmp[, coord_y := c(-13.066117, -13.073140, -13.078574, -13.083507)]
tmp[, coord_x := c(-46.007771, -46.004166, -45.999789, -45.983137)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# ctb0574-GB-46
# -23.0079224, -43.5042010 (ORIGINAL)
# -23.011275, -43.498102
# -23.005505, -43.497909
# -23.009870, -43.496542
# -23.009707, -43.508743
tmp <- soc_data[id == "ctb0574-GB-46"][rep(1, 4)]
tmp[, id := paste0(id, "-REP", 1:4)]
tmp[, coord_y := c(-23.011275, -23.005505, -23.009870, -23.009707)]
tmp[, coord_x := c(-43.498102, -43.497909, -43.496542, -43.508743)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# Summary
summary_soildata(soc_data)
# Layers: 12510
# Events: 12510
# Georeferenced events: 12510

# Set column order
soc_data <- soc_data[, .(id, coord_x, coord_y, year, soc_stock_g_m2)]

# Plot with mapview
if (FALSE) {
  soc_data_sf <- sf::st_as_sf(soc_data, coords = c("coord_x", "coord_y"), crs = 4326)
  mapview::mapview(soc_data_sf, zcol = "soc_stock_g_m2")
}

# Write data to disk ###############################################################################
folder_path <- "~/Insync/MapBiomas Solo/Trainning samples/"
file_name <- "-organic-carbon-stock-gram-per-square-meter.csv"
# List existing files in the folder_path and get the last one. Then read it.
existing_files <- list.files(path = folder_path, pattern = file_name)
last_file <- existing_files[length(existing_files)]
last_soc_data <- data.table::fread(paste0(folder_path, last_file))
# Check if last_soc_data == soc_data. If not, write soc_data to disk.
if (!identical(last_soc_data, soc_data)) {
  file_path <- paste0(folder_path, format(Sys.time(), "%Y-%m-%d"), file_name)
  file_path <- path.expand(file_path)
  data.table::fwrite(soc_data, file_path)
}











# Write data to disk
summary_soildata(soildata)
# Layers: 21895
# Events: 13967
# Georeferenced events: 11850
data.table::fwrite(soildata, "data/40_soildata_soc.txt", sep = "\t")

# PREVIOUS /////////////////////////////////////////////////////////////////////////////////////////
# MapBiomas Soil (beta): Script 06. Compute soil organic carbon stocks
# Alessandro Samuel-Rosa & Taciara Zborowski Horst
# 2024 CC-BY
# rm(list = ls())
# 
# # Install and load required packages
# if (!require("data.table")) {
#   install.packages("data.table")
# }
# 
# # Read data processed in the previous script
# febr_data <- data.table::fread("mapbiomas-soc/data/05-febr-data.txt", dec = ",", sep = "\t")
# colnames(febr_data)
# nrow(unique(febr_data[, "id"])) # Result: 11 359 events
# nrow(febr_data) # Result: 17 606 layers
# 
# # Filter out soil layers missing data on soil organic carbon
# nrow(febr_data[is.na(carbono), ]) # Result: 2145 layers
# nrow(unique(febr_data[is.na(carbono), "id"])) # Result: 1496 events
# febr_data <- febr_data[!is.na(carbono), ]
# 
# # Resetting the limits of each layer according to the target depth range (0 and 30 cm).
# # Source: https://github.com/ncss-tech/aqp/blob/master/R/depthWeights.R
# # The thickness of each layer is recalculated.
# # We identified that there were different events with the same ID and depths recorded with error.
# # These issues resulted in thickness greater than 30 cm. The decision was to remove these events.
# target_layer <- c(0, 30)
# febr_data[profund_sup < target_layer[1], profund_sup := target_layer[1]]
# febr_data[profund_inf < target_layer[1], profund_inf := target_layer[1]]
# febr_data[profund_sup > target_layer[2], profund_sup := target_layer[2]]
# febr_data[profund_inf > target_layer[2], profund_inf := target_layer[2]]
# febr_data[, espessura := profund_inf - profund_sup]
# nrow(febr_data[espessura < 0, ]) # 0 layers with thickness < 0 --> remove them
# febr_data <- febr_data[espessura > 0, ]
# febr_data[, thickness := sum(espessura), by = c("dataset_id", "observacao_id")]
# length(febr_data[thickness > 30, id]) # Result: 138 layers with thickness > 30
# length(unique(febr_data[thickness > 30, id])) # Result: 42 events with layers with thickness > 30
# febr_data <- febr_data[thickness <= 30, ] # remove layers
# febr_data[, thickness := NULL]
# 
# # Compute the volume of coarse fragments
# # ctb0054 - Solos da Reserva Particular do Patrimônio Natural SESC Pantanal
# febr_data[, fragmentos := esqueleto / 2.65 / 1000]
# febr_data[dataset_id == "ctb0054", fragmentos := 0]
# 
# # Calcular o estoque de carbono (kg/m^2) em cada camada
# # Fonte: T. Hengl et al., “SoilGrids1km–global soil information based on automated mapping,” PLoS
# # ONE, vol. 9, no. 8, p. e105992, 2014, doi: 10.1371/journal.pone.0105992.
# # O primeiro passo consiste em transformar os dados de conteúdo de terra fina (g/kg) para volume
# # (cm3/cm3). Isso é feito assumindo que a densidade dos fragmentos grossos é 2,65 g/cm3.
# # g/kg / g/cm3
# # 1) carbono / 1000: kg/kg
# # 2) espessura / 100: m
# # 3) dsi * 1000: kg/m^3
# # 4) fragmentos: 1
# febr_data[,
#   carbono_estoque_kgm2 := (carbono / 1000) * (espessura / 100) * (dsi * 1000) * (1 - fragmentos)]
# if (FALSE) {
#   x11()
#   hist(febr_data[, carbono_estoque_kgm2])
#   rug(febr_data[, carbono_estoque_kgm2])
# }
# 
# # Remove data from Technosols and Anthrosols
# # Solo construído no aterro encerrado da Caturrita, Santa Maria (RS)
# febr_data <- febr_data[dataset_id != "ctb0036", ]
# # Projeto Parque Frei Veloso - Levantamento Detalhado dos Solos do Campus da Ilha do Fundão UFRJ
# febr_data <- febr_data[dataset_id != "ctb0599", ]
# # Projeto Caldeirão: caracterização e gênese das Terras Pretas de Índio na Amazônia
# febr_data <- febr_data[dataset_id != "ctb0018", ]
# nrow(unique(febr_data[, "id"])) # Result: 9979 events
# nrow(febr_data) # Result: 15 059 layers

# Agregar estoque de carbono na camada superficial (0 até 30 cm) de cada evento
colnames(febr_data)
febr_data <- febr_data[
  !is.na(carbono_estoque_kgm2) &
    espessura > 0 &
    !is.na(coord_x) &
    !is.na(coord_y) &
    !is.na(data_coleta_ano),
  .(
    cos_estoque_gm2 = as.integer(round(sum(carbono_estoque_kgm2, na.rm = TRUE) * 1000)),
    data_coleta_ano = as.integer(round(mean(data_coleta_ano, na.rm = TRUE))),
    coord_x = mean(coord_x, na.rm = TRUE),
    coord_y = mean(coord_y, na.rm = TRUE),
    espessura = sum(espessura),
    FOREST = unique(FOREST),
    NONFOREST = unique(NONFOREST),
    PASTURE = unique(PASTURE),
    AGRICULTURE = unique(AGRICULTURE),
    FORESTRY = unique(FORESTRY),
    NONVEGETATION = unique(NONVEGETATION)
  ),
  by = id
]
nrow(febr_data) # Result: 8904 events/layers

if (FALSE) {
  x11()
  hist(febr_data[, cos_estoque_gm2] / 1000)
  rug(febr_data[, cos_estoque_gm2] / 1000)
}

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
