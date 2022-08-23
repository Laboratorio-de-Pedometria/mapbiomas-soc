rm(list = ls())

# Install and load required packages
if (!require("sf")) {
  install.packages("sf")
}
if (!require("geobr")) {
  install.packages("geobr")
}

# Ler dados do disco
febr_data <- data.table::fread("mapbiomas-solos/data/02-febr-data.txt", dec = ",", sep = "\t")
colnames(febr_data)

# Configurar dados em função da profundidade
# Camadas superficiais de constituição orgânica (serrapilheira) são ignoradas
febr_data[profund_sup > profund_inf, carbono := NA_real_]
febr_data[profund_sup < 0, carbono := NA_real_]

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

# Calcular o estoque de carbono (kg/m^2) em cada camada
# Fonte: T. Hengl et al., “SoilGrids1km–global soil information based on automated mapping,” PLoS
# ONE, vol. 9, no. 8, p. e105992, 2014, doi: 10.1371/journal.pone.0105992.
# 1) carbono / 1000: kg/kg
# 2) espessura / 100: m
# 3) dsi * 1000: kg/m^3
# 4) terrafina / 1000: 1
febr_data[,
  carbono_estoque_kgm2 := (carbono / 1000) * (espessura / 100) * (dsi * 1000) * (terrafina / 1000)]
febr_data[, carbono_estoque_kgm2 := carbono_estoque_kgm2]
hist(febr_data[, carbono_estoque_kgm2])
rug(febr_data[, carbono_estoque_kgm2])

# Agregar estoque de carbono na camada superficial (0 até 30 cm) de cada evento
febr_data <- febr_data[
  !is.na(carbono_estoque_kgm2) &
  espessura > 0 &
  !is.na(coord_x) &
  !is.na(coord_y) &
  !is.na(data_coleta_ano),
  .(carbono_estoque_g.m2 = as.integer(round(sum(carbono_estoque_kgm2, na.rm = TRUE) * 1000)),
    data_coleta_ano = as.integer(round(mean(data_coleta_ano, na.rm = TRUE))),
    coord_x = mean(coord_x, na.rm = TRUE),
    coord_y = mean(coord_y, na.rm = TRUE)),
  by = id]
febr_data
hist(febr_data[, carbono_estoque_g.m2])
rug(febr_data[, carbono_estoque_g.m2])

# Carregar dados de conjuntos de dados ainda não disponíveis no FEBR
files_stock <- list.files(
  path = "/home/alessandro/ownCloud/febr-repo/processamento", pattern = "-estoque.csv$",
  full.names = TRUE, recursive = TRUE)
data_stocks <- list()
for (i in seq_along(files_stock)) {
  data_stocks[[i]] <- data.table::fread(
    files_stock[i], sep = ",", dec = ".")
}
data_stocks <- do.call(rbind, data_stocks)
data.table::setnames(data_stocks, old = c("evento_id_febr", "coord_longitude", "coord_latitude"),
  new = c("id", "coord_x", "coord_y"))
febr_data <- rbind(febr_data, data_stocks)

# Avaliar distribuição temporal
hist(febr_data[, data_coleta_ano], sub = paste("N = ", nrow(febr_data)))
rug(febr_data[, data_coleta_ano])

# Criar objeto espacial e avaliar distribuição por bioma
brazil <- geobr::read_biomes()
febr_data_sf <- sf::st_as_sf(febr_data, coords = c("coord_x", "coord_y"), crs = 4623)
plot(brazil["name_biome"], reset = FALSE)
plot(febr_data_sf["carbono_estoque_g.m2"], add = TRUE, pch = 20,
  cex = febr_data_sf[["carbono_estoque_g.m2"]] / (max(febr_data_sf[["carbono_estoque_g.m2"]]) * 0.2))

# Escrever dados de estoque de carbono no solo em disco
febr_data[, coord_x := round(as.numeric(coord_x), 8)]
febr_data[, coord_y := round(as.numeric(coord_y), 8)]
write.table(febr_data, file = paste0("mapbiomas-solos/res/pontos-estoque.csv"),
  row.names = FALSE, sep = ",", dec = ".")
