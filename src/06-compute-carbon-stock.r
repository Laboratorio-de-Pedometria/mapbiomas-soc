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

# Ler dados do disco
febr_data <- data.table::fread("mapbiomas-solos/data/05-febr-data.txt", dec = ",", sep = "\t")
colnames(febr_data)

# Configurar dados em função da profundidade
# Camadas superficiais de constituição orgânica (serrapilheira) são ignoradas
# febr_data[profund_sup > profund_inf, carbono := NA_real_]
# febr_data[profund_sup < 0, carbono := NA_real_]

# Redefinir limites de cada camada em função do intervalo de profundidade alvo (0 e 30 cm)
# Fonte: https://github.com/ncss-tech/aqp/blob/master/R/depthWeights.R
# A espessura de cada camada é recalculada
target_layer <- c(0, 30)
febr_data[profund_sup < target_layer[1], profund_sup := target_layer[1]]
febr_data[profund_inf < target_layer[1], profund_inf := target_layer[1]]
febr_data[profund_sup > target_layer[2], profund_sup := target_layer[2]]
febr_data[profund_inf > target_layer[2], profund_inf := target_layer[2]]
febr_data[, espessura := profund_inf - profund_sup]

hist(febr_data[["espessura"]])
rug(febr_data[["espessura"]])

# Corrigir conteúdo de carbono estimado usando combustão úmida
# Considerar todas as amostras do século XX
# febr_data[data_coleta_ano < 2000, carbono := carbono * 1.1]

# Corrigir dados de esqueleto de camada R
febr_data[camada_nome == "R", terrafina := 0]

# Compute the volume of coarse fragments
# ctb0054 - Solos da Reserva Particular do Patrimônio Natural SESC Pantanal
colnames(febr_data)
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
hist(febr_data[, carbono_estoque_kgm2])
rug(febr_data[, carbono_estoque_kgm2])

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
    coord_y = mean(coord_y, na.rm = TRUE)
  ),
  by = id]
hist(febr_data[, carbono_estoque_g.m2])
rug(febr_data[, carbono_estoque_g.m2])

# Carregar dados de conjuntos de dados ainda não disponíveis no FEBR
# files_stock <- list.files(
#   path = "/home/alessandro/ownCloud/febr-repo/processamento", pattern = "-estoque.csv$",
#   full.names = TRUE, recursive = TRUE)
# data_stocks <- list()
# for (i in seq_along(files_stock)) {
#   data_stocks[[i]] <- data.table::fread(
#     files_stock[i], sep = ",", dec = ".")
# }
# data_stocks <- do.call(rbind, data_stocks)
# data.table::setnames(data_stocks, old = c("evento_id_febr", "coord_longitude", "coord_latitude"),
#   new = c("id", "coord_x", "coord_y"))
# febr_data <- rbind(febr_data, data_stocks)

# Avaliar distribuição de frequência dos dados de estoque de carbono
dev.off()
png("mapbiomas-solos/res/fig/distribuicao-dos-estoques-de-carbono.png",
  width = 480 * 3, height = 480 * 3, res = 72 * 3)
hist(febr_data[, carbono_estoque_g.m2] / 1000,
  main = "Distribuição do estoque de carbono, kg/m^2",
  sub = paste("N = ", nrow(febr_data)),
  xlab = "Ano", ylab = "Frequência absoluta")
rug(febr_data[, carbono_estoque_g.m2] / 1000)
dev.off()

# Avaliar distribuição de frequência dos dados pontuais ao longo do tempo
dev.off()
png("mapbiomas-solos/res/fig/distribuicao-temporal-dos-dados.png",
  width = 480 * 3, height = 480 * 3, res = 72 * 3)
hist(febr_data[, data_coleta_ano],
  main = "Distribuição temporal dos dados",
  sub = paste("N = ", nrow(febr_data)),
  xlab = "Ano", ylab = "Frequência absoluta")
rug(febr_data[, data_coleta_ano])
dev.off()

# Criar objeto espacial e avaliar distribuição por bioma
brazil <- geobr::read_biomes()
febr_data_sf <- sf::st_as_sf(febr_data, coords = c("coord_x", "coord_y"), crs = 4623)
dev.off()
png("mapbiomas-solos/res/fig/distribuicao-espacial-dos-dados.png",
  width = 480 * 3, height = 480 * 3, res = 72 * 3)
plot(brazil["name_biome"], reset = FALSE,
  main = "Distribuição espacial dos dados",
  key.pos = NULL, graticule = TRUE)
cex <- febr_data_sf[["carbono_estoque_g.m2"]] / (max(febr_data_sf[["carbono_estoque_g.m2"]]) * 0.2)
plot(febr_data_sf["carbono_estoque_g.m2"], add = TRUE, pch = 20, cex = cex)
dev.off()

# Escrever dados em disco
data.table::fwrite(febr_data, "mapbiomas-solos/data/06-febr-data.txt", sep = "\t", dec = ",")
