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
if (!require("mapview")) {
  install.packages("mapview")
}

# Ler dados do disco
febr_data <- data.table::fread("mapbiomas-solos/data/05-febr-data.txt", dec = ",", sep = "\t")
colnames(febr_data)
nrow(unique(febr_data[, "id"]))
# FEBR: 11 256 events; PronaSolos: 11 946
nrow(febr_data)
# FEB: 19 314 layers; PronaSolos: 20 700

# Filter out soil layers missing data on soil organic carbon
nrow(febr_data[is.na(carbono), ])
# FEBR: ???8 235 layers; PronaSolos: 7767 layers
nrow(unique(febr_data[is.na(carbono), "id"]))
# FEBR: ???8 235 events; PronaSolos: 4477 layers
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
febr_data[, thickness := sum(espessura), by = c("dataset_id", "observacao_id")]
length(febr_data[thickness > 30, id])
# FEBR: ???663 layers; PronaSolos: 126 layers
length(unique(febr_data[thickness > 30, id]))
# FEB: ???149 events; PronaSolos: 42 events
febr_data <- febr_data[thickness <= 30, ]
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

# Agregar estoque de carbono na camada superficial (0 até 30 cm) de cada evento
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
    espessura = sum(espessura)
  ),
  by = id
]
nrow(febr_data)
# FEBR: 6398 events/layers; PronaSolos: 6723
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
  width = 480 * 3, height = 480 * 3, res = 72 * 3)
hist(febr_data[, carbono_estoque_g.m2] / 1000,
  panel.first = grid(),
  xlab = expression("Estoque de carbono orgânico, kg m"^-2),
  ylab = paste0("Frequência absoluta (n = ", nrow(febr_data), ")"),
  main = ""
)
rug(febr_data[, carbono_estoque_g.m2] / 1000)
dev.off()

# Avaliar distribuição de frequência da espessura da camada
dev.off()
png("mapbiomas-solos/res/fig/carbon-stock-depth-distribution.png",
  width = 480 * 3, height = 480 * 3, res = 72 * 3)
hist(febr_data[, espessura],
  panel.first = grid(),
  xlab = "Profundidade da camada, cm",
  ylab = paste0("Frequência absoluta (n = ", nrow(febr_data), ")"),
  main = ""
)
rug(febr_data[, espessura])
dev.off()

# Avaliar distribuição de frequência dos dados pontuais ao longo do tempo
dev.off()
png("mapbiomas-solos/res/fig/carbon-stock-temporal-distribution.png",
  width = 480 * 3, height = 480 * 3, res = 72 * 3)
hist(febr_data[, data_coleta_ano],
  panel.first = grid(),
  main = "",
  xlab = "Ano de amostragem",
  ylab = paste0("Frequência absoluta (n = ", nrow(febr_data), ")")
)
rug(febr_data[, data_coleta_ano])
dev.off()

# Criar objeto espacial e avaliar distribuição por bioma
biomes <- geobr::read_biomes()[-7, ]
febr_data_sf <- sf::st_as_sf(febr_data, coords = c("coord_x", "coord_y"), crs = 4623)
dev.off()
png("mapbiomas-solos/res/fig/carbon-stock-spatial-distribution.png",
  width = 480 * 3, height = 480 * 3, res = 72 * 3)
plot(biomes["name_biome"], reset = FALSE,
  main = "", axes = TRUE,
  key.pos = NULL, graticule = TRUE)
cex <- febr_data_sf[["carbono_estoque_g.m2"]] / (max(febr_data_sf[["carbono_estoque_g.m2"]]) * 0.3)
plot(febr_data_sf["carbono_estoque_g.m2"], add = TRUE, pch = 20, cex = cex)
dev.off()

# Escrever dados em disco
febr_data[, espessura := NULL]
data.table::fwrite(febr_data, "mapbiomas-solos/res/pontos-estoque.csv")
