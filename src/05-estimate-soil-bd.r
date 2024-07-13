# title: SoilData - Soil Organic Carbon Stock
# subtitle: Estimate soil bulk density
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2024 CC-BY
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("ranger")) {
  install.packages("ranger")
}

# Read data from disk
soildata <- data.table::fread("mapbiomas-soc/data/21_soildata_soc.txt", sep = "\t")
str(soildata)
nrow(unique(soildata[, "id"])) # Result: 11 813 events
nrow(soildata) # Result: 21 890 layers

# Identify layers missing soil bulk density data
nrow(soildata[is.na(dsi), ]) # Result: 19 047 layers
nrow(unique(soildata[is.na(dsi), "id"])) # Result: 10 481 events







# PREVIOUS /////////////////////////////////////////////////////////////////////////////////////////
# MapBiomas Soil (beta): Script 05. Estimate soil bulk density
# Alessandro Samuel-Rosa & Taciara Zborowski Horst
# 2024 CC-BY
# The bulk density is a key soil property to compute SOC stocks. However, data on this soil property
# is missing for various soil layers. We deal with this issue by training a random forest regression
# model to estimate the bulk density of soil samples that are missing data on such variable. We use
# soil and environmental covariates as predictor variables.
# 
# KEY RESULTS
# 
# rm(list = ls())
# # Install and load required packages
# if (!require("ranger")) {
#   install.packages("ranger")
# }
# if (!require("caret")) {
#   install.packages("caret")
# }
# 
# # Ler dados do disco
# febr_data <- data.table::fread("mapbiomas-soc/data/04-febr-data.txt",
#   dec = ",", sep = "\t",
#   stringsAsFactors = TRUE
# )
# str(febr_data)
# nrow(unique(febr_data[, "id"])) # Result: 11 359 events
# nrow(febr_data) # Result: 17 606 layers
# colnames(febr_data)
# 
# # Identify layers missing soil bulk density data
# # We noticed that very high values (> 2.3 g/cm^3) were recorded for a few layers. There
# # also were two B horizons and one A horizon with too low density values (< 0.5). Checking their
# # source soil surveys, we identified that these data were erroneous. The data was then deleted.
# nrow(febr_data[dsi > 2.3, ]) # Result: 7 layers
# febr_data[dsi > 2.3, dsi := NA_real_]
# febr_data[dsi < 0.25, dsi := NA_real_]
# febr_data[dsi < 0.5 & grepl("B", camada_nome), dsi := NA_real_]
# febr_data[
#   dataset_id == "ctb0654" & observacao_id == "11-V-RCC" & camada_nome == "A",
#   dsi := NA_real_
# ]
# dsi_isna <- is.na(febr_data[["dsi"]])
# sum(!dsi_isna)
# sum(dsi_isna) # Result: 2787 and 14 819

# Figure 1: Distribution of soil bulk density data
dev.off()
png("mapbiomas-soc/res/fig/bulk-density-training-data.png",
  width = 480 * 3, height = 480 * 3, res = 72 * 3
)
par(mar = c(5, 4, 2, 2) + 0.1)
hist(febr_data[["dsi"]],
  panel.first = grid(nx = FALSE, ny = NULL), 
  xlab = expression("Densidade do solo, g cm"^-3),
  ylab = paste0("Frequência absoluta (n = ", sum(!dsi_isna), ")"),
  ylim = c(0, 1000),
  xlim = c(0, 2.5),
  main = "")
rug(febr_data[["dsi"]])
dev.off()

# Estimate random forest model
colnames(febr_data)
covars <- colnames(febr_data)
idx <- which(covars == "ORDER")
covars <- covars[idx:length(covars)]
dsi_formula <- as.formula(paste0("dsi ~ ", paste0(covars, collapse = " + ")))

t0 <- proc.time()
set.seed(1984)
dsi_model <- ranger::ranger(
  formula = dsi_formula,
  data = febr_data[!dsi_isna, ],
  num.trees = ceiling(nrow(febr_data[!dsi_isna, ]) * 0.25),
  importance = "impurity"
)
proc.time() - t0

# Compute regression model statistics
errosStatistics <-
  function(observed, predicted) {
    error <- predicted - observed
    residual <- mean(observed) - observed
    me <- mean(error)
    mae <- mean(abs(error))
    mse <- mean(error^2)
    rmse <- sqrt(mse)
    nse <- 1 - mse/mean(residual^2)
    slope <- coef(lm(observed ~ predicted))[2]
    return(data.frame(me, mae, mse, rmse, nse, slope))
}
print(dsi_model)

# Write model parameters to disk
write.table(capture.output(print(dsi_model))[6:15],
    file = "mapbiomas-soc/res/tab/bulk-density-model-parameters.txt", sep = "\t",
    row.names = FALSE
)

# Variable importance
dev.off()
png("mapbiomas-soc/res/fig/bulk-density-variable-importance.png",
  width = 480 * 3, height = 480 * 4, res = 72 * 3)
par(mar = c(4, 6, 1, 1) + 0.1)
barplot(sort(dsi_model$variable.importance) / max(dsi_model$variable.importance),
  horiz = TRUE, las = 1, col = "white", border = "white", axes = FALSE,
  xlab = "Importância relativa", cex.names = 0.5)
grid(nx = NULL, ny = FALSE)
barplot(sort(dsi_model$variable.importance) / max(dsi_model$variable.importance),
  horiz = TRUE, las = 1, add = TRUE, cex.names = 0.5)
dev.off()

# Fitted versus observed
dev.off()
png("mapbiomas-soc/res/fig/bulk-density-observed-versus-oob.png",
  width = 480 * 3, height = 480 * 3, res = 72 * 3
)
par(mar = c(4, 4.5, 2, 2) + 0.1)
plot(y = febr_data[!dsi_isna, dsi], x = dsi_model$predictions, xlim = c(0, 2.5), ylim = c(0, 2.5), 
  panel.first = grid(),
  ylab = expression("Densidade do solo observada, g cm"^-3),
  xlab = expression("Densidade do solo predita (OOB), g cm"^-3)
)
abline(0, 1)
dev.off()

# k-fold cross-validation with k = 10
t0 <- proc.time()
loocv_dsi_model <- caret::train(
  form = dsi_formula,
  method = "ranger",
  num.trees = dsi_model$num.trees,
  trControl = caret::trainControl(method = "cv", number = 10, savePredictions = TRUE),
  tuneGrid = data.frame(
    mtry = dsi_model$mtry,
    min.node.size = dsi_model$min.node.size,
    splitrule = dsi_model$splitrule),
  data = febr_data[!dsi_isna, ]
)
proc.time() - t0
print(loocv_dsi_model)
dsi_model_stats <- rbind(
  round(errosStatistics(febr_data[!dsi_isna, dsi], dsi_model$predictions), 4),
  round(errosStatistics(loocv_dsi_model$pred$obs, loocv_dsi_model$pred$pred), 4)
)
rownames(dsi_model_stats) <- c("out-of-bag", "10-fold cv")
print(dsi_model_stats)

# Write model statistics to disk
write.table(dsi_model_stats,
    file = "mapbiomas-soc/res/tab/bulk-density-model-statistics.txt", sep = "\t"
)

dev.off()
png("mapbiomas-soc/res/fig/bulk-density-observed-versus-10cv.png",
  width = 480 * 3, height = 480 * 3, res = 72 * 3
)
par(mar = c(4, 4.5, 2, 2) + 0.1)
plot(y = loocv_dsi_model$pred$obs, x = loocv_dsi_model$pred$pred, xlim = c(0, 2.5),
  ylim = c(0, 2.5),
  panel.first = grid(),
  ylab = expression("Densidade do solo observada, g cm"^-3),
  xlab = expression("Densidade do solo predita (CV), g cm"^-3)
)
abline(0, 1)
dev.off()

# Predict soil bulk density
tmp <- predict(dsi_model, data = febr_data[dsi_isna, ])
febr_data[dsi_isna, dsi := round(tmp$predictions, 2)]
nrow(unique(febr_data[, "id"])) # Result: 11 359
nrow(febr_data) # Result: 17 606

# Write data to disk
data.table::fwrite(febr_data, "mapbiomas-soc/data/05-febr-data.txt", sep = "\t", dec = ",")
