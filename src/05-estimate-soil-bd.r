rm(list = ls())

# Install and load required packages
if (!require("ranger")) {
  install.packages("ranger")
}

# Ler dados do disco
febr_data <- data.table::fread("mapbiomas-solos/data/04-febr-data.txt", dec = ",", sep = "\t")
colnames(febr_data)

# We work with data from the first 30 cm and ignore organic layers
febr_data <- febr_data[profund_sup >= 0 & profund_sup < 30, ]
nrow(febr_data)

# Compute carbon related covariates
# febr_data[, log_carbono := log1p(carbono)]
# febr_data[, argila_carbono := argila * carbono]
# summary(febr_data[, c("log_carbono", "argila_carbono")])

# Identify layers not missing soil density data
dsi_isna <- is.na(febr_data[["dsi"]])
sum(!dsi_isna); sum(dsi_isna)
x11()
hist(febr_data[!dsi_isna, dsi],
  xlab = expression("Densidade, g cm"^-3), ylab = "Frequência", main = "")
rug(febr_data[!dsi_isna, dsi])

# Exploratory data analysis
# r_vars <- c(
#   "argila", "areia", "carbono", "log_carbono", "argila_carbono", "silte", "profund", "terrafina")
# r_matrix <- cor(febr_data[dsi_notna_idx, ..r_vars], use = "complete", method = "spearman")
# pedometrics::plotCor(round(r_matrix, 2))

# Set categorical variables as factors
# febr_data[, estado_id := as.factor(estado_id)]
# state_table <- sort(table(febr_data[dsi_notna_idx, estado_id]), decreasing = TRUE)
# barplot(state_table, xlab = "Unidade da federação", ylab = "Frequência")

# Imputar valores faltantes
# colnames(febr_data)
# febr_data[, areia := randomForest::na.roughfix(areia)]
# febr_data[, argila := randomForest::na.roughfix(argila)]
# febr_data[, carbono := randomForest::na.roughfix(carbono)]
# febr_data[, argila_carbono := randomForest::na.roughfix(argila_carbono)]
# febr_data[, profund := randomForest::na.roughfix(profund)]
# febr_data[, sibcs_ordem := randomForest::na.roughfix(sibcs_ordem)]
# febr_data[, sibcs_subordem := randomForest::na.roughfix(sibcs_subordem)]
# febr_data[, coord_x := randomForest::na.roughfix(coord_x)]
# febr_data[, coord_y := randomForest::na.roughfix(coord_y)]

# Estimate random forest model
colnames(febr_data)
febr_data[, psd := NULL]
febr_data[, GREATGROUP := NULL]
febr_data[, SUBGROUP := NULL]
covars <- colnames(febr_data)
idx <- which(covars == "ORDER")
covars <- covars[idx:length(covars)]
dsi_formula <- paste0("dsi ~ ", paste0(covars, collapse = " + "))

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
x11()
par(mar = c(5, 10, 4, 2) + 0.1)
barplot(sort(dsi_model$variable.importance), horiz = TRUE, las = 1)

# Predicted versus observed
x11()
plot(y = febr_data[!dsi_isna, dsi], x = dsi_model$predictions, xlim = c(0, 3), ylim = c(0, 3), 
  panel.first = grid()
)
abline(0, 1)

# OOB RMSE
round(sqrt(dsi_model$prediction.error), 2)

# dsi_model_imp <- randomForest::importance(dsi_model)
# dsi_model_imp <- dsi_model_imp[order(dsi_model_imp[, 1]), ]
# par(mar = c(4, 7, 4, 4))
# barplot(dsi_model_imp[, 1], horiz = TRUE, las = 1)

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
