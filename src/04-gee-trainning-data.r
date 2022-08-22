rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}

# Ler planilha do Google Sheets contendo dados extraídos das covariáveis
# Não é preciso definir a aba pois há apenas uma delas na planilha
key <- "1dKLhlRT1Us4qpPuLV_1X07EiKEqRGJKnz8ztolOlFjE"
file <- paste0("http://docs.google.com/spreadsheets/d/", key, "/pub?output=tsv")
trainning <- data.table::fread(file, header = TRUE, na.strings = c("-", ""), sep = "\t", dec = ",")
head(trainning)
str(trainning)

# Calcular correlação entre estoque e idade da cobertura e uso da terra
landuse <- c(
  "formacaoFlorestal",
  "formacaoSavanica",
  "formacaoCampestre",
  "mosaicoAgriculturaPastagem",
  "outrasFormacoesNaoFlorestais",
  "pastagem",
  "silvicultura",
  "campoAlagado-areaPantanosa",
  "Lavouras"
)
dev.off()
png("mapbiomas-solos/res/gee-trainning-stock-time-landuse.png",
  width = 480 * 3, height = 480 * 3, res = 72 * 2)
par(mfrow = c(3, 3))
for (i in seq_along(landuse)) {
  vars <- c(landuse[i], "estoque")
  points <- trainning[, landuse[i], with = FALSE] > 0
  correlation <- round(
    cor(trainning[c(points), vars, with = FALSE], method = "spearman")[1, 2], 2)
  plot(trainning[c(points), c("year", "estoque")], main = landuse[i],
    xlim = c(1960, 2021), cex = 0.5, ylab = "Estoque (g/m^2)", xlab = "Ano")
  text(1960, max(trainning[c(points), estoque]) * 0.95,
    label = paste0("N = ", sum(points), "\n",
    "r = ", correlation), pos = 4)
}
dev.off()
# names(correlation) <- landuse
# idx_mosaico <- names(correlation) == "mosaicoAgriculturaPastagem"
# names(correlation)[idx_mosaico] <- "mosaico"
# correlation <- round(correlation, 2)
# correlation
# color <- ifelse(correlation < 0, "firebrick", "darkgreen")
# barplot(correlation, col = color)

# Identificar agrupamento de amostras de lavoura de 2011
# O agrupamento parece ser o responsável pela correlação positiva com a idade
# https://www.pedometria.org/febr/ctb0022
trainning[Lavouras > 0 & year == 2011, dataset_id]

# Identificar agrupamento de amostras de formação campestre de 2005
# Amostras localizadas nos Campos de Cima da Serra (RS)
# https://www.pedometria.org/febr/ctb0019
trainning[formacaoCampestre > 0 & year == 2005, dataset_id]

# Identificar amostras com elevado conteúdo de carbono localizadas em lavoura
trainning[Lavouras > 0 & estoque > 12000, c("dataset_id", "Lavouras", "estoque")]
