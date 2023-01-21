rm(list = ls())

# Install and load required packages
# if (!require("partykit")) {
#   install.packages("partykit")
# }
if (!require("randomForest")) {
  install.packages("randomForest")
}
if (!require("pedometrics")) {
  install.packages("pedometrics")
}

# Ler dados do disco
febr_data <- data.table::fread("mapbiomas-solos/data/01-febr-data.txt", dec = ",", sep = "\t")
colnames(febr_data)

# Calculate sampling depth and layer thickness
febr_data[, espessura := profund_inf - profund_sup]
febr_data[, profund := (profund_sup + profund_inf) / 2]
summary(febr_data[, c("espessura", "profund")])

# Compute carbon related covariates
febr_data[, log_carbono := log1p(carbono)]
febr_data[, argila_carbono := argila * carbono]
summary(febr_data[, c("log_carbono", "argila_carbono")])

# Compute soil class
sibcs <- strsplit(febr_data[["taxon_sibcs"]], " ")
idx_notaxon <- sapply(sibcs, length) == 0
sibcs[idx_notaxon] <- list(c(NA_character_, NA_character_))
idx_singletaxon <- sapply(sibcs, length) == 1
sibcs[idx_singletaxon] <- lapply(sibcs[idx_singletaxon], function(x) c(x, NA_character_))
sibcs <- do.call(rbind, sibcs)
febr_data[, sibcs_ordem := as.factor(sibcs[, 1])]
febr_data[, sibcs_subordem := as.factor(sibcs[, 2])]
febr_data[, taxon_sibcs := as.factor(taxon_sibcs)]
table(febr_data[["sibcs_ordem"]])
table(febr_data[["sibcs_subordem"]])
table(febr_data[["taxon_sibcs"]])

# Identify layers missing soil density data
dsi_notna_idx <- !is.na(febr_data[["dsi"]])
hist(febr_data[dsi_notna_idx, dsi],
  xlab = expression("Densidade, g cm"^-3), ylab = "Frequência", main = "")
rug(febr_data[dsi_notna_idx, dsi])

# Exploratory data analysis
r_vars <- c(
  "argila", "areia", "carbono", "log_carbono", "argila_carbono", "silte", "profund", "terrafina")
r_matrix <- cor(febr_data[dsi_notna_idx, ..r_vars], use = "complete", method = "spearman")
pedometrics::plotCor(round(r_matrix, 2))

# Set categorical variables as factors
febr_data[, estado_id := as.factor(estado_id)]
state_table <- sort(table(febr_data[dsi_notna_idx, estado_id]), decreasing = TRUE)
barplot(state_table, xlab = "Unidade da federação", ylab = "Frequência")

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
dsi_formula <- dsi ~
  terrafina +
  # silte +
  areia + argila +
  # ctc + ph +
  carbono + argila_carbono +
  # log_carbono +
  profund +
  estado_id
  # sibcs_ordem +
  # sibcs_subordem +
  # taxon_sibcs +
  # coord_x + coord_y
t0 <- proc.time()
dsi_model <- randomForest::randomForest(
  formula = dsi_formula,
  data = febr_data[dsi_notna_idx, ],
  na.action = randomForest::na.roughfix,
  ntree = 500,
  importance = TRUE
)
proc.time() - t0

# Compute model statistics
print(dsi_model)
plot(dsi_model, panel.first = {
  grid()
})
dsi_model_imp <- randomForest::importance(dsi_model)
dsi_model_imp <- dsi_model_imp[order(dsi_model_imp[, 1]), ]
par(mar = c(4, 7, 4, 4))
barplot(dsi_model_imp[, 1], horiz = TRUE, las = 1)

# Predict soil bulk density
febr_data[!dsi_notna_idx, "dsi"] <- predict(
  dsi_model, newdata = febr_data[!dsi_notna_idx, ])
febr_data[, dsi := round(dsi, 2)]
hist(febr_data[["dsi"]])
rug(febr_data[["dsi"]])

# Escrever dados em disco
data.table::fwrite(
  febr_data[!is.na(carbono) | !is.na(dsi), ],
  "mapbiomas-solos/data/02-febr-data.txt", sep = "\t", dec = ",")

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
