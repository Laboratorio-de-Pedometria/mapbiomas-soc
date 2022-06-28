# Install and load required packages
if (!require("febr")) {
  install.packages("febr")
}
if (!require("sf")) {
  install.packages("sf")
}
# if (!require("partykit")) {
#   install.packages("partykit")
# }
if (!require("randomForest")) {
  install.packages("randomForest")
}

# Download data from FEBR
febr_data <- data.table::fread("~/ownCloud/febr-repo/publico/febr-superconjunto.txt", dec = ",")
colnames(febr_data)

# Calculate sampling depth and layer thickness
febr_data[, espessura := profund_inf - profund_sup]
febr_data[, profund := (profund_sup + profund_inf) / 2]

# Compute carbon related covariates
febr_data[, log_carbono := log1p(carbono)]
febr_data[, argila_carbono := argila * carbono]

# Compute soil class
sibcs <- do.call(rbind, strsplit(febr_data[["taxon_sibcs"]], " "))
febr_data[, sibcs_ordem := as.factor(sibcs[, 1])]
febr_data[, sibcs_subordem := as.factor(sibcs[, 2])]
table(febr_data[["sibcs_ordem"]])
table(febr_data[["sibcs_subordem"]])

# Identify layers missing soil density data
dsi_notna_idx <- !is.na(febr_data[["dsi"]])
hist(febr_data[dsi_notna_idx, dsi],
  xlab = expression("Densidade, g cm"^-3), ylab = "Frequência", main = "")

# Exploratory data analysis
r_vars <- c(
  "argila", "areia", "carbono", "log_carbono", "argila_carbono", "silte", "profund", "terrafina")
r_matrix <- cor(febr_data[dsi_notna_idx, ..r_vars], use = "complete", method = "spearman")
pedometrics::plotCor(round(r_matrix, 2))

# Set categorical variables as factors
febr_data[, estado_id := as.factor(estado_id)]
state_table <- sort(table(febr_data[dsi_notna_idx, estado_id]), decreasing = TRUE)
barplot(state_table, xlab = "Unidade da federação", ylab = "Frequência")

# Estimate random forest model
dsi_formula <- dsi ~
  terrafina + areia + silte + argila +
  carbono + log_carbono + argila_carbono +
  profund +
  estado_id +
  sibcs_ordem + sibcs_subordem
t0 <- proc.time()
dsi_model <- randomForest::randomForest(
  formula = dsi_formula,
  data = febr_data[dsi_notna_idx, ],
  na.action = randomForest::na.roughfix,
  ntree = 300,
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
hist(febr_data[["dsi"]])

# Write data on disk
data.table::fwrite(
  febr_data, "mapbiomas-solos/data/febr-data.txt", sep = "\t", dec = ",")

# Estimate recursive partitioning regression model #################################################
# dsi_formula <- dsi ~ argila + silte + carbono + log1p(carbono) + I(argila * carbono) | profund + terrafina + areia
# t0 <- proc.time()
# dsi_model <- partykit::glmtree(formula = dsi_formula, data = febr_data[dsi_notna_idx, ],
#   family = gaussian(link = "identity"), alpha = 0.10, prune = "AIC",
#   verbose = TRUE, restart = TRUE, cores = 3)
# proc.time() - t0

# Model statistics
print(dsi_model)
plot(dsi_model, panel.first = {
  grid()
})
# plot(residuals(dsi_model), panel.first = {
#     grid()
#     abline(h = 0, col = "red", lty = "dashed")
#   },
#   ylab = expression("Densidade residual, g cm"^-3), xlab = "Índice da amostra"
# )
summary(dsi_model)
round(sqrt(mean(residuals(dsi_model)^2)), 2)
round(1 - (sum(residuals(dsi_model)^2) / sum((dsi_model$data[["dsi"]] - mean(dsi_model$data[["dsi"]]))^2)), 2)

# dev.off()
# png("ptf-density/res/fig/mob-fit-left-branch.png", width = 480 * 2, height = 480 * 2, res = 72)
# plot(dsi_model[[2]])
# dev.off()

# dev.off()
# png("ptf-density/res/fig/mob-fit-right-branch.png", width = 480 * 2, height = 480 * 2, res = 72)
# plot(dsi_model[[23]])
# dev.off()

# Predict soil density
febr_data[!dsi_notna_idx, "dsi"] <- predict(dsi_model, newdata = febr_data[!dsi_notna_idx, ])
dsi_minmax <- range(febr_data[dsi_notna_idx, dsi])
outsider_idx <-
  febr_data[!dsi_notna_idx, dsi] < dsi_minmax[1] | febr_data[!dsi_notna_idx, dsi] > dsi_minmax[2]
febr_data[!dsi_notna_idx, "dsi"][outsider_idx] <- NA_real_

# Plot resulting data
hist(febr_data[["dsi"]])

# Write data on disk
data.table::fwrite(febr_data, "mapbiomas-solos/data/febr-data.txt", sep = "\t", dec = ",")
