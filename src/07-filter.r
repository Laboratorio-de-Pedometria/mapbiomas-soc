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
if (!require("rgee")) {
  install.packages("rgee", dependencies = TRUE)
}
rgee::ee_Initialize()

# Read data from disk
febr_data <- data.table::fread("mapbiomas-solos/data/06-febr-data.txt", dec = ",", sep = "\t")
colnames(febr_data)

# Set date = 1985 for samples collected before 1995
febr_data[data_coleta_ano < 1985, data_coleta_ano := 1985]

# Check if we have replicated sample points
dupl <- dist(febr_data[, c("data_coleta_ano", "coord_x", "coord_y", "carbono_estoque_g.m2")])
dupl <- as.matrix(dupl)
diag(dupl) <- NA_real_
dupl_idx <- which(dupl == 0, arr.ind = TRUE)
dupl_idx <- dupl_idx[seq(1, nrow(dupl_idx), by = 2), ]
febr_data <- febr_data[!dupl_idx[, 1], ]

# Create spatial object
febr_data <- sf::st_as_sf(febr_data, coords = c("coord_x", "coord_y"), crs = 4326)
brazil <- geobr::read_country()
x11()
plot(brazil, reset = FALSE)
plot(febr_data, add = TRUE, col = "black", cex = 0.5)

# Prepare for sampling on GEE
n_points <- nrow(febr_data)
n_lags <- ceiling(n_points / 1000)
lag_width <- ceiling(n_points / n_lags)
lags <- rep(1:n_lags, each = lag_width)
lags <- lags[1:n_points]

# MapBiomas Land Cover Land Use
gee_path <- "projects/mapbiomas-workspace/public/collection7/mapbiomas_collection70_integration_v2"
mapbiomas <- list()
for (i in 1:n_lags) {
  mapbiomas[[i]] <- rgee::ee_extract(
    x = ee$Image(gee_path),
    y = febr_data[lags == i, ],
    scale = 30,
    fun = rgee::ee$Reducer$first()
  )
}
mapbiomas <- data.table::rbindlist(mapbiomas)
colnames(mapbiomas) <- gsub("classification_", "", colnames(mapbiomas))
lulc_idx <- match(mapbiomas[, data_coleta_ano], colnames(mapbiomas))
lulc <- as.matrix(mapbiomas)
lulc <- lulc[cbind(1:nrow(lulc), lulc_idx)]
febr_data[["lulc"]] <- lulc
febr_data[["natural"]] <- lulc %in% c(1, 3, 4, 5, 10, 49, 11, 12, 32, 29, 50, 13, 9)

x11()
hist(febr_data[["carbono_estoque_g.m2"]][which(febr_data$natural)])
rug(febr_data[["carbono_estoque_g.m2"]][which(febr_data$natural)])

cex <- febr_data[["carbono_estoque_g.m2"]] / (max(febr_data[["carbono_estoque_g.m2"]]) * 0.2)
plot(brazil, reset = FALSE, main = "Natural LULC at the Time of Sampling", sub = "According to MapBiomas")
plot(febr_data[which(febr_data$natural), "carbono_estoque_g.m2"],
  add = TRUE, pch = 20,
  cex = cex[which(febr_data$natural)]
)
sum(!febr_data$natural)

plot(brazil,
  reset = FALSE,
  main = "NOT-Natural LULC at the Time of Sampling",
  sub = "According to MapBiomas"
)
plot(febr_data[which(!febr_data$natural), "carbono_estoque_g.m2"],
  add = TRUE, pch = 20,
  cex = cex[which(!febr_data$natural)]
)

febr_data[!febr_data$natural & febr_data$carbono_estoque_g.m2 > 100000, ]

sort(cex[which(!febr_data$natural)])

# Escrever dados de estoque de carbono no solo em disco
febr_data[, coord_x := round(as.numeric(coord_x), 8)]
febr_data[, coord_y := round(as.numeric(coord_y), 8)]
write.table(febr_data,
  file = paste0("mapbiomas-solos/res/pontos-estoque.csv"),
  row.names = FALSE, sep = ",", dec = "."
)
