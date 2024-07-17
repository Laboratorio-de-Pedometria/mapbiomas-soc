# title: SoilData - Soil Organic Carbon Stock
# subtitle: Compute soil organic carbon stocks
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2024 CC-BY
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}

# Read data processed in the previous script
soildata <- data.table::fread("data/30_soildata_soc.txt", sep = "\t")
nrow(unique(soildata[, "id"])) # Result: 11 751 events
nrow(soildata) # Result: 21 750 layers

# Filter out soil layers missing data on soil organic carbon
is_na_carbono <- is.na(soildata[["carbono"]])
sum(is_na_carbono) # Result: 2188 layers
soildata <- soildata[!is_na_carbono, ]
nrow(unique(soildata[, "id"])) # Result: 10 635 events
nrow(soildata) # Result: 19 562 layers

# Topsoil --> BECAUSE WE REMOVED LAYERS WITH MISSING DATA ON SOIL ORGANIC CARBON
# For each event (id), check if there is a layer with profund_sup == 0. Filter out events without a
# topsoil layer. This procedure was performed before: it is repeated here because layers missing
# data on soil organic carbon were removed (there may be other reasons too).
soildata[, topsoil := any(profund_sup == 0), by = id]
nrow(unique(soildata[topsoil == FALSE, "id"])) # 70 events
soildata <- soildata[topsoil == TRUE, ]
nrow(unique(soildata[, "id"])) # 10 565 events
nrow(soildata) # 19 461 layers
soildata[, topsoil := NULL]

# Empty layers --> ALSO BECAUSE WE REMOVED LAYERS WITH MISSING DATA ON SOIL ORGANIC CARBON
# Is there any soil profile (id) missing an intermediate layer? A missing intermediate layer occurs
# when profund_inf of layer i is different from profund_sup of layer i + 1. Many of these profiles
# are complementary/extra observations that sampled only the topsoil and the subsoil, e.g. 0-20 and
# 60-80 cm. They are interval censored. For further analysis, we drop the lowermost layers.
soildata[, empty_layer := shift(profund_inf, type = "lag") != profund_sup, by = id]
nrow(unique(soildata[empty_layer == TRUE, "id"])) # Result: 1376 events
soildata <- soildata[empty_layer == FALSE | is.na(empty_layer), ]
soildata <- soildata[, empty_layer := NULL]
nrow(unique(soildata[, "id"])) # Result: 10 565 events
nrow(soildata) # Result: 18 085 layers

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
nrow(soildata[espessura <= 0, ]) # 1102 layers with thickness <= 0 --> remove them
soildata <- soildata[espessura > 0, ]
nrow(unique(soildata[, "id"])) # Result: 10 565 events
nrow(soildata) # Result: 16 983 layers

# Volume of coarse fragments
# The volume of coarse fragments is calculated as the volume of the skeleton divided by the density
# of the skeleton (2.65 g/cm^3) and converted to m^3.
soildata[, fragmentos := esqueleto / 2.65 / 1000]
soildata[dataset_id == "ctb0054", fragmentos := 0]

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

# Remove data from Technosols and Anthrosols
# Solo construído no aterro encerrado da Caturrita, Santa Maria (RS)
soildata <- soildata[dataset_id != "ctb0036", ]
# Projeto Parque Frei Veloso - Levantamento Detalhado dos Solos do Campus da Ilha do Fundão UFRJ
soildata <- soildata[dataset_id != "ctb0599", ]
# Projeto Caldeirão: caracterização e gênese das Terras Pretas de Índio na Amazônia
soildata <- soildata[dataset_id != "ctb0018", ]
nrow(unique(soildata[, "id"])) # Result: 10 378 events
nrow(soildata) # Result: 16 742 layers

# Layers per event
# Count the number of layers per event (id)
soildata[, n := .N, by = "id"]
table(soildata[, n])
#    1    2    3    4    5 
# 5402 7392 3519  420   10 
soildata[, n := NULL]

# Write data to disk
colnames(soildata)
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
