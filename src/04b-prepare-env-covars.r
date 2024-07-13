# MapBiomas Soil (beta): Script 04b. Environmental covariates - get GEE data
# Alessandro Samuel-Rosa & Taciara Zborowski Horst
# 2024 CC-BY
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}

# Read data processed in the previous script
soildata <- data.table::fread("mapbiomas-soc/data/21_soildata_soc.txt", sep = "\t")
nrow(unique(soildata[, "id"])) # Result: 11 813 events
nrow(soildata) # Result: 21 891 layers

# Figure
# Distribution of events through land use/land cover classes
lulc_classes <- soildata[, lulc[1], by = id]
lulc_classes <- table(lulc_classes[, V1])
dev.off()
png(
  "mapbiomas-soc/res/fig/bulk-density-lulc-classes.png",
  width = 480 * 5, height = 480 * 3, res = 72 * 3
)
barplot(lulc_classes,
  col = "gray", border = "gray",
  xlab = "Land use/land cover class",
  ylab = paste0("Absolute frequency (n = ", sum(lulc_classes), ")")
  )
grid(nx = FALSE, ny = NULL, col = "gray")
dev.off()

# Figure
# Geolocalized events missing data on SoilGrids and MapBiomas
nrow(soildata[is.na(clay_0_5cm) & !is.na(coord_x) & !is.na(coord_y), clay_0_5cm[1], by = .(id)]) # 279 events
nrow(soildata[lulc == "unknown" & !is.na(coord_x) & !is.na(coord_y), lulc[1], by = .(id)]) # 4174 events

# Read data from geobr
brazil <- geobr::read_country()
biomas <- geobr::read_biomes()[-7, "name_biome"]

# //////////////////////////////////////////////////////////////////////////////////////////////////
# rm(list = ls())
# 
# # Install and load required packages
# if (!require("data.table")) {
#   install.packages("data.table")
# }
# if (!require("sf")) {
#   install.packages("sf")
# }
# if (!require("geobr")) {
#   install.packages("geobr")
# }
# 
# # Google Earth Engine
# if (!require("rgee")) {
#   install.packages("rgee", dependencies = TRUE)
#   library(rgee)
# }
# local_user <- Sys.getenv("USER")
# gee_user <- ifelse(grepl("alessandro", local_user), "alessandrosamuelrosa", NULL)
# rgee::ee_Initialize(user = gee_user)
# 
# # Read data from geobr
# brazil <- geobr::read_country()
# biomas <- geobr::read_biomes()[-7, "name_biome"]
# 
# # Missingness in attribute
# mia <-
#   function(x) {
#     is_na <- is.na(x)
#     if (is.numeric(x)) {
#       Xplus <- as.numeric(x)
#       Xplus[is_na] <- as.numeric(+Inf)
#       Xminus <- as.numeric(x)
#       Xminus[is_na] <- as.numeric(-Inf)
#       Xna <- rep("ISNOTNA", length(x))
#       Xna[is_na] <- as.character("ISNA")
#       out <- data.frame(Xplus = Xplus, Xminus = Xminus, Xna = Xna)
#     } else {
#       Xunknown <- as.character(x)
#       Xunknown[is_na] <- "UNKNOWN"
#       out <- data.frame(Xunknown = Xunknown)
#     }
#     return(out)
#   }
# 
# # Read data processed in the previous script
# febr_data <- data.table::fread("mapbiomas-soc/data/03-febr-data.txt", dec = ",", sep = "\t")
# nrow(unique(febr_data[, "id"])) # Result: 11 359 events
# nrow(febr_data) # Result: 17 606 layers
# 
# # Create spatial object
# # First filter out those samples without coordinates
# # Also keep a single sample per soil profile
# is_na_coordinates <- is.na(febr_data[, coord_x]) | is.na(febr_data[, coord_y])
# paste0(sum(is_na_coordinates), " events without coordinates") # 113 layers without coordinates
# sp_febr_data <- febr_data[!is_na_coordinates, ]
# nrow(unique(sp_febr_data[, "id"])) # Result: 11 312 events with coordinates
# nrow(sp_febr_data) # Result: 17 493 layers with coordinates
# first <- function(x) x[1, ]
# sf_febr_data <- 
#   sp_febr_data[, first(id),
#   by = c("dataset_id", "observacao_id", "coord_x", "coord_y", "data_coleta_ano")]
# sf_febr_data[, V1 := NULL]
# nrow(sf_febr_data) # Result: 11 312 events with coordinates
# sf_febr_data <- sf::st_as_sf(sf_febr_data, coords = c("coord_x", "coord_y"), crs = 4326)
# if (FALSE) {
#   x11()
#   plot(brazil, reset = FALSE)
#   plot(sf_febr_data, add = TRUE, col = "black", cex = 0.5)
# }
# 
# # Prepare for sampling on GEE
# n_max <- 5000 # Maximum number of events to be sampled at once
# n_points <- nrow(sf_febr_data)
# n_lags <- ceiling(n_points / n_max)
# lag_width <- ceiling(n_points / n_lags)
# lags <- rep(1:n_lags, each = lag_width)
# lags <- lags[1:n_points]
# 
# # Soil Grids 250m v2.0: bdod_mean (bulk density)
# bdod_mean <- list()
# for (i in 1:n_lags) {
#   bdod_mean[[i]] <- rgee::ee_extract(
#     x = rgee::ee$Image("projects/soilgrids-isric/bdod_mean"),
#     y = sf_febr_data[lags == i, ],
#     scale = 250
#   )
# }
# bdod_mean <- data.table::rbindlist(bdod_mean)
# bdod_mean[, c("bdod_30.60cm_mean", "bdod_60.100cm_mean", "bdod_100.200cm_mean") := NULL]
# nrow(bdod_mean) # Result: 12 139 events returned
# 
# # Soil Grids 250m v2.0: clay_mean (clay content)
# clay_mean <- list()
# for (i in 1:n_lags) {
#   clay_mean[[i]] <- rgee::ee_extract(
#     x = rgee::ee$Image("projects/soilgrids-isric/clay_mean"),
#     y = sf_febr_data[lags == i, ],
#     scale = 250
#   )
# }
# clay_mean <- data.table::rbindlist(clay_mean)
# clay_mean[, c("clay_30.60cm_mean", "clay_60.100cm_mean", "clay_100.200cm_mean") := NULL]
# nrow(clay_mean) # Result: 12 139 events returned
# 
# # Soil Grids 250m v2.0: sand_mean (sand content)
# sand_mean <- list()
# for (i in 1:n_lags) {
#   sand_mean[[i]] <- rgee::ee_extract(
#     x = ee$Image("projects/soilgrids-isric/sand_mean"),
#     y = sf_febr_data[lags == i, ],
#     scale = 250
#   )
# }
# sand_mean <- data.table::rbindlist(sand_mean)
# sand_mean[, c("sand_30.60cm_mean", "sand_60.100cm_mean", "sand_100.200cm_mean") := NULL]
# nrow(sand_mean) # Result: 12 139 events returned
# 
# # Soil Grids 250m v2.0: soc_mean (soil organic carbon)
# soc_mean <- list()
# for (i in 1:n_lags) {
#   soc_mean[[i]] <- rgee::ee_extract(
#     x = rgee::ee$Image("projects/soilgrids-isric/soc_mean"),
#     y = sf_febr_data[lags == i, ], scale = 250
#   )
# }
# soc_mean <- data.table::rbindlist(soc_mean)
# soc_mean[, c("soc_30.60cm_mean", "soc_60.100cm_mean", "soc_100.200cm_mean") := NULL]
# nrow(soc_mean) # Result: 12 139 events returned
# 
# # Soil Grids 250m v2.0: cfvo_mean (coarse fragments volume)
# cfvo_mean <- list()
# for (i in 1:n_lags) {
#   cfvo_mean[[i]] <- rgee::ee_extract(
#     x = rgee::ee$Image("projects/soilgrids-isric/cfvo_mean"),
#     y = sf_febr_data[lags == i, ], scale = 250
#   )
# }
# cfvo_mean <- data.table::rbindlist(cfvo_mean)
# cfvo_mean[, c("cfvo_30.60cm_mean", "cfvo_60.100cm_mean", "cfvo_100.200cm_mean") := NULL]
# nrow(cfvo_mean) # Result: 12 139 events returned
# 
# # Collate data sampled from SoilGrids
# SoilGrids <- merge(bdod_mean, clay_mean)
# SoilGrids <- merge(SoilGrids, sand_mean)
# SoilGrids <- merge(SoilGrids, soc_mean)
# SoilGrids <- merge(SoilGrids, cfvo_mean)
# colnames(SoilGrids) <- gsub("_mean", "", colnames(SoilGrids), fixed = TRUE)
# nrow(SoilGrids) # Result: 12 139 events returned
# 
# # Prepare to sample MapBiomas on GEE
# n_max <- 1000 # Maximum number of events to be sampled at once
# n_points <- nrow(sf_febr_data)
# n_lags <- ceiling(n_points / n_max)
# lag_width <- ceiling(n_points / n_lags)
# lags <- rep(1:n_lags, each = lag_width)
# lags <- lags[1:n_points]
# 
# # Sample MapBiomas LULC
# gee_path <- paste0(
#   "projects/mapbiomas-workspace/public/", # GEE asset path
#   "collection7_1/mapbiomas_collection71_integration_v1" # GEE asset name
# )
# mapbiomas <- list()
# for (i in 1:n_lags) {
#   mapbiomas[[i]] <- rgee::ee_extract(
#     x = ee$Image(gee_path),
#     y = sf_febr_data[lags == i, ],
#     scale = 30,
#     fun = rgee::ee$Reducer$first()
#   )
# }
# mapbiomas <- data.table::rbindlist(mapbiomas)
# nrow(mapbiomas) # Result: 12 139 events returned
# 
# # Get LULC class at the year of sampling (data_coleta_ano)
# # Each column in the MapBiomas dataset represents a year. The column name is the year of the
# # classification. The value is the class code. We need to extract the class code for the year of
# # sampling, which is stored in the data_coleta_ano column. 
# colnames(mapbiomas) <- gsub("classification_", "", colnames(mapbiomas))
# mapbiomas[, YEAR := data_coleta_ano]
# mapbiomas[YEAR < 1985, YEAR := 1985]
# lulc_idx <- match(mapbiomas[, YEAR], colnames(mapbiomas))
# lulc <- as.matrix(mapbiomas)
# lulc <- lulc[cbind(1:nrow(lulc), lulc_idx)]
# mapbiomas[, lulc := as.character(lulc)]
# mapbiomas[, YEAR := NULL]
# nrow(mapbiomas) # Result: 12 139 events returned
# 
# # Create bivariate covariates indicating natural land covers and agricultural land uses
# # mapbiomas-br-site.s3.amazonaws.com/downloads/_EN__C%C3%B3digos_da_legenda_Cole%C3%A7%C3%A3o_7.pdf
# table(mapbiomas[, lulc]) # A value 0 means "no data" (e.g., map holes and outside the map domain)
# sort(unique(mapbiomas[, lulc])) # Single digit codes such as " 1" and " 3" start with a space
# forest <- as.character(c(" 1", " 3", " 4", " 5", "49"))
# nonforest <- as.character(c(10, "11", "12", "32", "29", "50", 13))
# pasture <- c("15")
# agriculture <- c(14, 18, 19, "39", "20", "40", 62, "41", 36, "46", 47, "48", "21")
# forestry <- as.character(" 9")
# nonvegetation <- as.character(c(" 0", 22, "23", "24", "30", "25", 26, "33", "31", 27))
# mapbiomas[, FOREST := "FALSE"]
# mapbiomas[, NONFOREST := "FALSE"]
# mapbiomas[, PASTURE := "FALSE"]
# mapbiomas[, AGRICULTURE := "FALSE"]
# mapbiomas[, FORESTRY := "FALSE"]
# mapbiomas[, NONVEGETATION := "FALSE"]
# mapbiomas[lulc %in% forest, FOREST := "TRUE"]
# mapbiomas[lulc %in% nonforest, NONFOREST := "TRUE"]
# mapbiomas[lulc %in% pasture, PASTURE := "TRUE"]
# mapbiomas[lulc %in% agriculture, AGRICULTURE := "TRUE"]
# mapbiomas[lulc %in% forestry, FORESTRY := "TRUE"]
# mapbiomas[lulc %in% nonvegetation, NONVEGETATION := "TRUE"]
# mapbiomas[, as.character(1985:2021) := NULL]
# nrow(mapbiomas) # Results: 12 139 events returned
# 
# # Distribution of events through land use/land cover classes
# lulc_classes <- c("FOREST", "NONFOREST", "PASTURE", "AGRICULTURE", "FORESTRY", "NONVEGETATION")
# lulc_classes <- sort(lulc_classes)
# lulc_classes <- sapply(mapbiomas[, ..lulc_classes], function(x) { sum(x == "TRUE") })
# dev.off()
# png("mapbiomas-solo/res/fig/bulk-density-lulc-classes.png",
#   width = 480 * 5, height = 480 * 3, res = 72 * 3)
# barplot(lulc_classes,
#   # horiz = TRUE, las = 1,
#   col = "white", border = "white", axes = FALSE,
#   xlab = "Classe de cobertura ou uso da terra",
#   ylab = paste0("Frequência absoluta (n = ", sum(lulc_classes), ")")
#   )
# grid(nx = FALSE, ny = NULL)
# barplot(lulc_classes,
#   # horiz = TRUE, las = 1,
#   add = TRUE
# )
# dev.off()
# 
# # Merge data sampled from SoilGrids and MapBiomas
# sf_febr_data <- merge(sf_febr_data, SoilGrids)
# sf_febr_data <- merge(sf_febr_data, mapbiomas)
# nrow(sf_febr_data) # Result: 12 139 events returned
# 
# # Merge geolocalized events and layers
# sf_febr_data <- data.table::as.data.table(sf_febr_data)
# sf_febr_data[, geometry := NULL]
# sp_febr_data <- merge(sp_febr_data, sf_febr_data,
#   by = c("dataset_id", "observacao_id", "data_coleta_ano"))
# nrow(unique(sp_febr_data[, c("dataset_id", "observacao_id", "data_coleta_ano")])) # 12 139 events
# nrow(sp_febr_data) # Result: 19 141 layers
# 
# # Merge geolocalized and non-geolocalized events and layers
# febr_data <- data.table::rbindlist(list(sp_febr_data, febr_data[is_na_coordinates, ]),
#   use.names = TRUE, fill = TRUE)
# nrow(unique(febr_data[, "id"])) # Result: 12 186 events
# nrow(febr_data) # Result: 19 254 layers
# 
# Impute missing data using the MIA method
which_cols <- union(colnames(SoilGrids), colnames(mapbiomas))
which_cols <- which_cols[
    !which_cols %in% c("dataset_id", "observacao_id", "data_coleta_ano", "lulc")
]
which_cols <- match(which_cols, colnames(febr_data))
for (i in which_cols) {
  y <- mia(febr_data[[i]])
  colnames(y) <- gsub("X", toupper(colnames(febr_data)[i]), colnames(y))
  febr_data <- cbind(febr_data, y)
}
febr_data[, colnames(febr_data)[which_cols] := NULL]
colnames(febr_data) <- gsub("unknown", "", colnames(febr_data))

# SoilGrids and MapBiomas
# 340 geolocalized events are missing values for SoilGrids data
# 6 geolocalized events are missing values for MapBiomas data
n_na_soilgrids <- nrow(unique(febr_data[
  SOC_0.5CMna == "ISNA" & !is.na(coord_x),
  c("dataset_id", "observacao_id", "data_coleta_ano")
]))
print(n_na_soilgrids) # SoilGrids: 340 events are missing values
n_na_mapbiomas <- nrow(unique(febr_data[
  is.na(lulc) & !is.na(coord_x),
  c("dataset_id", "observacao_id", "data_coleta_ano")
]))
print(n_na_mapbiomas) # MapBiomas: 06 events are missing values
dev.off()
png("mapbiomas-solo/res/fig/bulk-density-mapbiomas-soilgrids-missing-data.png",
  width = 480 * 3, height = 480 * 3, res = 72 * 3)
plot(biomas, reset = FALSE, graticule = TRUE, axes = TRUE, ylab = "Longitude", xlab = "Latitude",
  main = "", key.pos = 1, key.length = 1)
points(febr_data[SOC_0.5CMna == "ISNA" & !is.na(coord_x), c("coord_x", "coord_y")], col = "red")
points(febr_data[is.na(lulc) & !is.na(coord_x), c("coord_x", "coord_y")], col = "blue")
legend(x = -45, y = 6.5,
  legend = c(paste0("NA SoilGrids (n = ", n_na_soilgrids, ")"),
            paste0("NA MapBiomas (n = ", n_na_mapbiomas, ")")),
  col = c("red", "blue"),
  box.lwd = 0, pch = 1)
dev.off()

# Restore past
# ids_keep <- unique(febr_data[, id])
# febr_data <- data.table::fread("mapbiomas-soc/data/04-febr-data.txt", dec = ",", sep = "\t")
# nrow(febr_data)
# nrow(unique(febr_data[, "id"]))
# febr_data <- febr_data[id %in% ids_keep, ]
# nrow(febr_data) # Result: 17 606 layers
# nrow(unique(febr_data[, "id"])) # Result: 11 359 events

# Write data to disk
nrow(unique(febr_data[, "id"])) # Result: 11 359 events
nrow(febr_data) # Result: 17 606 layers
febr_data[, lulc := NULL]
data.table::fwrite(febr_data, "mapbiomas-soc/data/04-febr-data.txt", sep = "\t", dec = ",")
