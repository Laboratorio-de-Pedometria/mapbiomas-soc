# title: SoilData - Soil Organic Carbon Stock
# subtitle: Prepare covariate figures
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2024 CC-BY
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}

# Read data processed in the previous script
soildata <- data.table::fread("data/21_soildata_soc.txt", sep = "\t", na.strings = c("NA", ""))
nrow(unique(soildata[, "id"])) # Result: 11 751 events
nrow(soildata) # Result: 21 750 layers

# Figure
# Distribution of events through land use/land cover classes
lulc_classes <- soildata[, lulc[1], by = id]
lulc_classes <- table(lulc_classes[, V1], useNA = "always")
dev.off()
png("res/fig/bulk-density-lulc-classes.png", width = 480 * 5, height = 480 * 3, res = 72 * 3)
barplot(lulc_classes,
  col = "gray", border = "gray",
  xlab = "Land use/land cover class",
  ylab = paste0("Absolute frequency (n = ", sum(lulc_classes), ")")
  )
grid(nx = FALSE, ny = NULL, col = "gray")
dev.off()

# Figure
# Geolocalized events missing data on SoilGrids and MapBiomas
na_sg <- nrow(soildata[is.na(clay_0_5cm) & !is.na(coord_x) & !is.na(coord_y), clay_0_5cm[1], by = .(id)])
print(na_sg) # 274 events
na_mb <- nrow(soildata[is.na(lulc) & !is.na(coord_x) & !is.na(coord_y) & data_coleta_ano >= 1985,
  lulc[1],
  by = .(id)
])
print(na_mb) # 4 events
brazil <- geobr::read_country()
biomas <- geobr::read_biomes()[-7, "name_biome"]
dev.off()
png("res/fig/bulk-density-gee-missing-data.png", width = 480 * 3, height = 480 * 3, res = 72 * 3)
plot(biomas,
  reset = FALSE, graticule = TRUE, axes = TRUE, ylab = "Longitude", xlab = "Latitude",
  main = "", key.pos = 1, key.length = 1
)
points(soildata[
  is.na(lulc) & !is.na(coord_x) & !is.na(coord_y) & data_coleta_ano >= 1985,
  c("coord_x", "coord_y")
], col = "blue")
points(soildata[is.na(clay_0_5cm) & !is.na(coord_x) & !is.na(coord_y), c("coord_x", "coord_y")],
  col = "red"
)
legend(
  x = -48, y = 6.5,
  legend = c(paste0("NA SoilGrids (n = ", na_sg, ")"), paste0("NA MapBiomas (n = ", na_mb, ")")),
  col = c("red", "blue"),
  box.lwd = 0, pch = 1
)
dev.off()
