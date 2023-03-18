# 05. ESTIMATE SOIL BULK DENSITY ###################################################################
# SUMMARY
# The bulk density is a key soil property to compute SOC stocks. However, data on this soil property
# is missing for various soil layers. We deal with this issue by training a random forest regression
# model to estimate the bulk density of soil samples that are missing data on such variable. We use
# soil and environmental covariates as predictor variables.
# 
# KEY RESULTS
# 
rm(list = ls())

# Install and load required packages
if (!require("ranger")) {
  install.packages("ranger")
}
if (!require("caret")) {
  install.packages("caret")
}

# Ler dados do disco
febr_data <- data.table::fread("mapbiomas-solos/data/04-febr-data.txt", dec = ",", sep = "\t",
  stringsAsFactors = TRUE)
nrow(unique(febr_data[, c("dataset_id", "observacao_id")])) # 15 129
nrow(febr_data) # 52 566
colnames(febr_data)

# We work only with data from the first 30 cm and deeper layers that start at or before 30 cm.
# We also ignore organic layers (negative depth) in mineral soils.
# * four layers with negative depth
# * 26 351 layers with superior depth equal to or larger than 30 cm
# * 17 743 layers missing SOC and bulk density data
nrow(febr_data[profund_sup < 0, ])
nrow(febr_data[profund_sup >= 30, ])
nrow(febr_data[is.na(dsi) & is.na(carbono), ])
febr_data <- febr_data[profund_sup >= 0 & profund_sup < 30, ]
febr_data <- febr_data[!is.na(dsi) | !is.na(carbono), ]
nrow(unique(febr_data[, c("dataset_id", "observacao_id")])) # 9669
nrow(febr_data) # 16 596

# Identify layers missing soil bulk density data
# We noticed that very high values (> 2.3 g/cm^3) were recorded for a few layers (n = 12). There
# also were two B horizons and one A horizon with too low density values (< 0.5). Checking their
# source soil surveys, we identified that these data were erroneous. The data was then deleted.
# There are 3582 layers with data on soil bulk density. Predictions need to be made for 13 014
# layers.
nrow(febr_data[dsi > 2.3, ])
febr_data[dsi > 2.3, dsi := NA_real_]
febr_data[dsi < 0.5 & grepl("B", camada_nome), dsi := NA_real_]
febr_data[dataset_id == "ctb0654" & observacao_id == "11-V-RCC" & camada_nome == "A",
  dsi := NA_real_]
dsi_isna <- is.na(febr_data[["dsi"]])
sum(!dsi_isna); sum(dsi_isna)
dev.off()
png("mapbiomas-solos/res/fig/bulk-density-training-data.png",
  width = 480 * 3, height = 480 * 3, res = 72 * 3)
hist(febr_data[["dsi"]],
  panel.first = grid(), 
  xlab = expression("Densidade do solo, g cm"^-3),
  ylab = paste0("Frequência absoluta (n = ", sum(!dsi_isna), ")"),
  main = "")
rug(febr_data[["dsi"]])
dev.off()

# Estimate random forest model
colnames(febr_data)
febr_data[, GREATGROUP := NULL]
febr_data[, SUBGROUP := NULL]
covars <- colnames(febr_data)
idx <- which(covars == "ORDER")
covars <- covars[idx:length(covars)]
dsi_formula <- as.formula(paste0("dsi ~ ", paste0(covars, collapse = " + ")))

t0 <- proc.time()
dsi_model <- ranger::ranger(
  formula = dsi_formula,
  data = febr_data[!dsi_isna, ],
  num.trees = 500,
  importance = "impurity"
)
proc.time() - t0

# Compute model statistics
print(dsi_model)

# Variable importance
dev.off()
png("mapbiomas-solos/res/fig/bulk-density-variable-importance.png",
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
png("mapbiomas-solos/res/fig/bulk-density-fitted-versus-observed.png",
  width = 480 * 3, height = 480 * 3, res = 72 * 3)
plot(y = febr_data[!dsi_isna, dsi], x = dsi_model$predictions, xlim = c(0, 3), ylim = c(0, 3), 
  panel.first = grid(),
  ylab = expression("Densidade do solo observada, g cm"^-3),
  xlab = expression("Densidade do solo ajustada, g cm"^-3)
)
abline(0, 1)
dev.off()

# OOB RMSE
round(sqrt(dsi_model$prediction.error), 2)

validationMetrics <-
  function(data) {
    error <- data$pred - data$obs
    residual <- mean(data$obs) - data$obs
    me <- mean(error)
    mae <- mean(abs(error))
    mse <- mean(error^2)
    rmse <- sqrt(mse)
    nse <- round(1 - mse/mean(residual^2), 2)
    slope <- coef(lm(data$obs ~ data$pred))[2]
    return(data.frame(me, mae, rmse, nse, slope))
}

t0 <- proc.time()
loocv_dsi_model <- caret::train(
  form = dsi_formula,
  method = "ranger",
  trControl = caret::trainControl(method = "cv", number = 20, savePredictions = TRUE),
  tuneGrid = data.frame(mtry = 8, min.node.size = 5, splitrule = "variance"),
  data = febr_data[!dsi_isna, ]
)
proc.time() - t0
print(loocv_dsi_model)
round(validationMetrics(loocv_dsi_model$pred), 2)

plot(loocv_dsi_model$pred[, 2] ~ loocv_dsi_model$pred[, 1])


# Predict soil bulk density
tmp <- predict(dsi_model, data = febr_data[dsi_isna, ])
febr_data[dsi_isna, dsi := round(tmp$predictions, 2)]

# Escrever dados em disco
data.table::fwrite(
  febr_data[!is.na(carbono) | !is.na(dsi), ],
  "mapbiomas-solos/data/05-febr-data.txt", sep = "\t", dec = ",")

# Estimate recursive partitioning regression model #################################################
# dsi_formula <- dsi ~ argila + silte + carbono + log1p(carbono) + I(argila * carbono) | profund + terrafina + areia
# t0 <- proc.time()
# dsi_model <- partykit::glmtree(formula = dsi_formula, data = febr_data[dsi_notna_idx, ],
#   family = gaussian(link = "identity"), alpha = 0.10, prune = "AIC",
#   verbose = TRUE, restart = TRUE, cores = 3)
# proc.time() - t0

# Model statistics
# print(dsi_model)
# plot(dsi_model, panel.first = {
#   grid()
# })
# plot(residuals(dsi_model), panel.first = {
#     grid()
#     abline(h = 0, col = "red", lty = "dashed")
#   },
#   ylab = expression("Densidade residual, g cm"^-3), xlab = "Índice da amostra"
# )
# summary(dsi_model)
# round(sqrt(mean(residuals(dsi_model)^2)), 2)
# round(1 - (sum(residuals(dsi_model)^2) / sum((dsi_model$data[["dsi"]] - mean(dsi_model$data[["dsi"]]))^2)), 2)

# dev.off()
# png("ptf-density/res/fig/mob-fit-left-branch.png", width = 480 * 2, height = 480 * 2, res = 72)
# plot(dsi_model[[2]])
# dev.off()

# dev.off()
# png("ptf-density/res/fig/mob-fit-right-branch.png", width = 480 * 2, height = 480 * 2, res = 72)
# plot(dsi_model[[23]])
# dev.off()

# Predict soil density
# febr_data[!dsi_notna_idx, "dsi"] <- predict(dsi_model, newdata = febr_data[!dsi_notna_idx, ])
# dsi_minmax <- range(febr_data[dsi_notna_idx, dsi])
# outsider_idx <-
#   febr_data[!dsi_notna_idx, dsi] < dsi_minmax[1] | febr_data[!dsi_notna_idx, dsi] > dsi_minmax[2]
# febr_data[!dsi_notna_idx, "dsi"][outsider_idx] <- NA_real_

# Plot resulting data
# hist(febr_data[["dsi"]])

# Write data on disk
# data.table::fwrite(febr_data, "mapbiomas-solos/data/febr-data.txt", sep = "\t", dec = ",")
