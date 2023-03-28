# 06. COMPUTE ORGANIC CARBON STOCK #################################################################
# SUMMARY
# 
# KEY RESULTS
# 
rm(list = ls())

# Install and load required packages
if (!require("sf")) {
  install.packages("sf")
}
if (!require("geobr")) {
  install.packages("geobr")
}
biomes <- geobr::read_biomes()[-7, ]
if (!require("mapview")) {
  install.packages("mapview")
}

# Read data processed in the previous script
febr_data <- data.table::fread("mapbiomas-solos/data/05-febr-data.txt", dec = ",", sep = "\t")
colnames(febr_data)
nrow(unique(febr_data[, "id"]))
# Result: 12 186 events
nrow(febr_data)
# Result: 19 254 layers

# Filter out soil layers missing data on soil organic carbon
nrow(febr_data[is.na(carbono), ])
# Result: 2275 layers
nrow(unique(febr_data[is.na(carbono), "id"]))
# Result: 1597 events
febr_data <- febr_data[!is.na(carbono), ]

# Resetting the limits of each layer according to the target depth range (0 and 30 cm).
# Source: https://github.com/ncss-tech/aqp/blob/master/R/depthWeights.R
# The thickness of each layer is recalculated.
# We identified that there were different events with the same ID and depths recorded with error.
# These issues resulted in thickness greater than 30 cm. The decision was to remove these events.
target_layer <- c(0, 30)
febr_data[profund_sup < target_layer[1], profund_sup := target_layer[1]]
febr_data[profund_inf < target_layer[1], profund_inf := target_layer[1]]
febr_data[profund_sup > target_layer[2], profund_sup := target_layer[2]]
febr_data[profund_inf > target_layer[2], profund_inf := target_layer[2]]
febr_data[, espessura := profund_inf - profund_sup]
nrow(febr_data[espessura < 0, ]) # 06 layers with thickness < 0 --> remove them
febr_data <- febr_data[espessura > 0, ]
febr_data[, thickness := sum(espessura), by = c("dataset_id", "observacao_id")]
length(febr_data[thickness > 30, id])
# Result: 152 layers with thickness > 30
length(unique(febr_data[thickness > 30, id]))
# Result: 42 events with layers with thickness > 30
febr_data <- febr_data[thickness <= 30, ] # remove layers
febr_data[, thickness := NULL]

# Compute the volume of coarse fragments
# ctb0054 - Solos da Reserva Particular do Patrimônio Natural SESC Pantanal
febr_data[, fragmentos := esqueleto / 2.65 / 1000]
febr_data[dataset_id == "ctb0054", fragmentos := 0]

# Calcular o estoque de carbono (kg/m^2) em cada camada
# Fonte: T. Hengl et al., “SoilGrids1km–global soil information based on automated mapping,” PLoS
# ONE, vol. 9, no. 8, p. e105992, 2014, doi: 10.1371/journal.pone.0105992.
# O primeiro passo consiste em transformar os dados de conteúdo de terra fina (g/kg) para volume
# (cm3/cm3). Isso é feito assumindo que a densidade dos fragmentos grossos é 2,65 g/cm3.
# g/kg / g/cm3
# 1) carbono / 1000: kg/kg
# 2) espessura / 100: m
# 3) dsi * 1000: kg/m^3
# 4) fragmentos: 1
febr_data[,
  carbono_estoque_kgm2 := (carbono / 1000) * (espessura / 100) * (dsi * 1000) * (1 - fragmentos)]
if (FALSE) {
  x11()
  hist(febr_data[, carbono_estoque_kgm2])
  rug(febr_data[, carbono_estoque_kgm2])
}

# Remove data from Technosols and Anthrosols
# Solo construído no aterro encerrado da Caturrita, Santa Maria (RS)
febr_data <- febr_data[dataset_id != "ctb0036", ]
# Projeto Parque Frei Veloso - Levantamento Detalhado dos Solos do Campus da Ilha do Fundão UFRJ
febr_data <- febr_data[dataset_id != "ctb0599", ]
# Projeto Caldeirão: caracterização e gênese das Terras Pretas de Índio na Amazônia
febr_data <- febr_data[dataset_id != "ctb0018", ]
nrow(unique(febr_data[, "id"]))
# Result: 10 758 events
nrow(febr_data)
# Result: 16 556 layers

# Agregar estoque de carbono na camada superficial (0 até 30 cm) de cada evento
colnames(febr_data)
febr_data <- febr_data[
  !is.na(carbono_estoque_kgm2) &
    espessura > 0 &
    !is.na(coord_x) &
    !is.na(coord_y) &
    !is.na(data_coleta_ano),
  .(
    carbono_estoque_g.m2 = as.integer(round(sum(carbono_estoque_kgm2, na.rm = TRUE) * 1000)),
    data_coleta_ano = as.integer(round(mean(data_coleta_ano, na.rm = TRUE))),
    coord_x = mean(coord_x, na.rm = TRUE),
    coord_y = mean(coord_y, na.rm = TRUE),
    espessura = sum(espessura),
    FOREST = unique(FOREST),
    NONFOREST = unique(NONFOREST),
    PASTURE = unique(PASTURE),
    AGRICULTURE = unique(AGRICULTURE),
    FORESTRY = unique(FORESTRY),
    NONVEGETATION = unique(NONVEGETATION),
    data_coleta_ano = mean(data_coleta_ano)
  ),
  by = id
]
nrow(febr_data)
# Result: 9650 events/layers

# table(febr_data[FOREST == "FALSE" & NONFOREST == "FALSE", AGRICULTURE == "FALSE" & PASTURE == "FALSE" & FORESTRY == "FALSE" & NONVEGETATION == "FALSE", data_coleta_ano])
# lulc <- c("FOREST", "NONFOREST", "PASTURE", "AGRICULTURE", "FORESTRY", "NONVEGETATION")
# sum(sapply(febr_data[, ..lulc], function(x) sum(x == "TRUE")))

if (FALSE) {
  x11()
  hist(febr_data[, carbono_estoque_g.m2] / 1000)
  rug(febr_data[, carbono_estoque_g.m2] / 1000)
}

# Check if we have replicated sample points
double <- duplicated(febr_data[, c("data_coleta_ano", "coord_x", "coord_y", "carbono_estoque_g.m2")])
sum(double) # six duplicated events
febr_data <- febr_data[!double, ]

# Avaliar distribuição de frequência dos dados de estoque de carbono
dev.off()
png("mapbiomas-solos/res/fig/carbon-stock-histogram.png",
  width = 480 * 3, height = 480 * 3, res = 72 * 3
)
par(mar = c(5, 4, 2, 2) + 0.1)
hist(febr_data[, carbono_estoque_g.m2] / 1000,
  panel.first = grid(nx = FALSE, ny = NULL), 
  xlab = expression("Estoque de carbono orgânico, kg m"^-2),
  ylab = paste0("Frequência absoluta (n = ", nrow(febr_data), ")"),
  ylim = c(0, 8000),
  xlim = c(0, 120),
  main = ""
)
rug(febr_data[, carbono_estoque_g.m2] / 1000)
dev.off()

# Avaliar distribuição de frequência da espessura da camada
dev.off()
png("mapbiomas-solos/res/fig/carbon-stock-depth-distribution.png",
  width = 480 * 3, height = 480 * 3, res = 72 * 3
)
par(mar = c(5, 4, 2, 2) + 0.1)
hist(febr_data[, espessura],
  panel.first = grid(nx = FALSE, ny = NULL),
  xlab = "Espessura da camada superficial, cm",
  ylab = paste0("Frequência absoluta (n = ", nrow(febr_data), ")"),
  ylim = c(0, 5000),
  xlim = c(0, 30),
  main = ""
)
rug(febr_data[, espessura])
dev.off()

# Avaliar distribuição de frequência dos dados pontuais ao longo do tempo
febr_data[data_coleta_ano == 1938, data_coleta_ano := sample(1960:1984, 1)]
dev.off()
png("mapbiomas-solos/res/fig/carbon-stock-temporal-distribution.png",
  width = 480 * 3, height = 480 * 3, res = 72 * 3
)
par(mar = c(5, 4, 2, 2) + 0.1)
hist(febr_data[, data_coleta_ano],
  panel.first = grid(nx = FALSE, ny = NULL),
  xlab = "Ano de amostragem",
  ylab = paste0("Frequência absoluta (n = ", nrow(febr_data), ")"),
  ylim = c(0, 4000),
  xlim = c(min(febr_data[, data_coleta_ano]),
    as.integer(format(Sys.time(), "%Y"))),
  main = ""
)
rug(febr_data[, data_coleta_ano])
dev.off()

# Criar objeto espacial e avaliar distribuição por bioma
febr_data_sf <- sf::st_as_sf(febr_data, coords = c("coord_x", "coord_y"), crs = 4623)
dev.off()
png("mapbiomas-solos/res/fig/carbon-stock-spatial-distribution.png",
  width = 480 * 3, height = 480 * 3, res = 72 * 3)
plot(biomes["name_biome"], reset = FALSE,
  main = "", axes = TRUE, col = "transparent", lwd = 0.5, border = "darkgray",
  key.pos = NULL, graticule = TRUE)
cex <- febr_data_sf[["carbono_estoque_g.m2"]] / (max(febr_data_sf[["carbono_estoque_g.m2"]]) * 0.4)
plot(febr_data_sf["carbono_estoque_g.m2"], add = TRUE, pch = 21, cex = cex, col = "black")
prob <- c(0, 0.5, 0.95, 0.975, 1)
legend(
  x = -40, y = 8,
  legend = paste0(round(quantile(febr_data_sf[["carbono_estoque_g.m2"]], prob) / 1000, 1), " kg/m^2"),
  pch = 21, box.lwd = 0, pt.cex = quantile(cex, prob)
)
dev.off()

# Write date to disk
colnames(febr_data)
data.table::fwrite(
  febr_data,
  paste0("mapbiomas-solos/res/tab/", format(Sys.time(), "%Y-%m-%d"), "-pontos-estoque-cos.csv")
)
