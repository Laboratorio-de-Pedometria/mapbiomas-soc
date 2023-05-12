# 07. CREATE FIGURES
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("sf")) {
  install.packages("sf")
}
if (!require("geobr")) {
  install.packages("geobr")
}
biomes <- geobr::read_biomes()[-7, ]
biomes <- sf::st_transform(biomes, crs = 4326)
brazil <- geobr::read_country()
brazil <- sf::st_transform(brazil, crs = 4326)

# Read data processed in the previous script
febr_data <- data.table::fread("mapbiomas-solos/data/06-febr-data.txt", dec = ",", sep = "\t")
colnames(febr_data)
nrow(unique(febr_data[, "id"])) # Result: 9650 events
nrow(febr_data) # Result: 9650 layers

# Avaliar distribuição de frequência dos dados de estoque de carbono
dev.off()
png("mapbiomas-solos/res/fig/carbon-stock-histogram.png",
  width = 480 * 3, height = 480 * 3, res = 72 * 3
)
par(mar = c(5, 4, 2, 2) + 0.1)
y_max <- hist(febr_data[, cos_estoque_gm2] / 1000, plot = FALSE)$counts
y_max <- ceiling(max(y_max) / 1000) * 1000
hist(febr_data[, cos_estoque_gm2] / 1000,
  panel.first = grid(nx = FALSE, ny = NULL), 
  # xlab = expression("Estoque de carbono orgânico do solo, kg m"^-2),
  xlab = expression("Estoque de carbono orgânico do solo, t/ha"),
  ylab = paste0("Frequência absoluta (n = ", nrow(febr_data), ")"),
  ylim = c(0, y_max),
  xlim = c(0, 120),
  main = ""
)
rug(febr_data[, cos_estoque_gm2] / 1000)
dev.off()

# Avaliar distribuição de frequência da espessura da camada
dev.off()
png("mapbiomas-solos/res/fig/carbon-stock-depth-distribution.png",
  width = 480 * 3, height = 480 * 3, res = 72 * 3
)
par(mar = c(5, 4, 2, 2) + 0.1)
y_max <- hist(febr_data[, espessura], plot = FALSE)$counts
y_max <- ceiling(max(y_max) / 1000) * 1000
hist(febr_data[, espessura] / 100,
  panel.first = grid(nx = FALSE, ny = NULL),
  xlab = "Espessura da camada superficial, m",
  ylab = paste0("Frequência absoluta (n = ", nrow(febr_data), ")"),
  ylim = c(0, y_max),
  xlim = c(0, 30) / 100,
  main = ""
)
rug(febr_data[, espessura] / 100)
dev.off()

# Avaliar distribuição de frequência dos dados pontuais ao longo do tempo
dev.off()
png("mapbiomas-solos/res/fig/carbon-stock-temporal-distribution.png",
  width = 480 * 3, height = 480 * 3, res = 72 * 3
)
par(mar = c(5, 4, 2, 2) + 0.1)
y_max <- hist(febr_data[, data_coleta_ano], plot = FALSE)$counts
y_max <- ceiling(max(y_max) / 1000) * 1000
hist(febr_data[, data_coleta_ano],
  panel.first = grid(nx = FALSE, ny = NULL),
  xlab = "Ano de amostragem",
  ylab = paste0("Frequência absoluta (n = ", nrow(febr_data), ")"),
  ylim = c(0, y_max),
  xlim = c(min(febr_data[, data_coleta_ano]),
    as.integer(format(Sys.time(), "%Y"))),
  main = ""
)
rug(febr_data[, data_coleta_ano])
dev.off()

# Criar objeto espacial e avaliar distribuição por bioma
febr_data_sf <- sf::st_as_sf(febr_data, coords = c("coord_x", "coord_y"), crs = 4326)
nrow(febr_data_sf)
dev.off()
png("mapbiomas-solos/res/fig/carbon-stock-spatial-distribution.png",
  width = 480 * 3, height = 480 * 3, res = 72 * 3)
plot(biomes["name_biome"], reset = FALSE,
  main = "", axes = TRUE, col = "transparent", lwd = 0.5, border = "darkgray",
  key.pos = NULL, graticule = TRUE)
cex <- febr_data_sf[["cos_estoque_gm2"]] / (max(febr_data_sf[["cos_estoque_gm2"]]) * 0.4)
plot(febr_data_sf["cos_estoque_gm2"], add = TRUE, pch = 21, cex = cex, col = "black")
prob <- c(0, 0.5, 0.95, 0.975, 1)
quant <- round(quantile(febr_data_sf[["cos_estoque_gm2"]], prob) / 1000, 1)
leg_text <- paste(quant, "~ kg/m^2")
leg_text <- sapply(leg_text, function(x) parse(text = x))
legend(x = -40, y = 8, legend = leg_text, pch = 21, box.lwd = 0, pt.cex = quantile(cex, prob))
dev.off()

# Avaliar distribuição por bioma
biomes$name_biome <- gsub(" ", "-", biomes$name_biome)
biomes$name_biome <- gsub("ô", "o", biomes$name_biome)
biomes$name_biome <- gsub("â", "a", biomes$name_biome)
biomes$name_biome <- tolower(biomes$name_biome)
febr_data_sf[["biome"]] <- NA_character_
dev.off()
for (i in biomes$name_biome) {
  idx_bio <- which(biomes$name_biome == i)
  idx_sf <- sf::st_intersects(biomes[idx_bio, ], febr_data_sf)[[1]]
  febr_data_sf[["biome"]][idx_sf] <- i
  file_name <- paste0(
    "mapbiomas-solos/res/fig/carbon-stock-spatial-distribution-", i, ".png"
  )
  png(file_name, width = 480 * 3, height = 480 * 3, res = 72 * 3, bg = "transparent")
  plot(
    biomes[idx_bio, "name_biome"],
    reset = FALSE, main = "", axes = FALSE, col = "#eeece1", border = "gray69", key.pos = NULL
  )
  plot(
    febr_data_sf["cos_estoque_gm2"][idx_sf, ],
    add = TRUE, pch = 21, col = "firebrick", cex = 1.0
  )
  dev.off()
}

# Mapa da distribuição espacial e temporal
# < 1985 e depois quatro intervalos
# Remover linhas de grade

# Number of samples per biome
table(febr_data_sf[["biome"]])

# Areal density of points per biome
round(table(febr_data_sf[["biome"]]) / (as.numeric(sf::st_area(biomes)) / (1000 * 1000 * 1000)), 1)


round(table(febr_data_sf[["biome"]]) / (2022 - 1985))
