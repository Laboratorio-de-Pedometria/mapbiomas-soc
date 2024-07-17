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

# Source helper functions
source("src/00_helper_functions.r")

# Read data from disk
soildata <- data.table::fread("data/21_soildata_soc.txt", sep = "\t", na.strings = c("NA", ""))
summary_soildata(soildata)
# Layers: 21750
# Events: 11751
# Georeferenced events: 9452

# Identify layers missing soil bulk density data
is_na_dsi <- is.na(soildata[["dsi"]])
nrow(soildata[is.na(dsi), ]) # Result: 19 036 layers
nrow(unique(soildata[is.na(dsi), "id"])) # Result: 10 476 events

# Endpoint (MUST BE CORRECTED IN PREVIOUS SCRIPT)
soildata[is.na(endpoint), endpoint := 0]

# Set covariates
colnames(soildata)
not_covars <- c(
  "dsi",
  "observacao_id", "coord_x", "coord_y", "coord_precisao", "coord_fonte", "amostra_area",
   "amostra_id", "camada_nome",
  "data_coleta_ano", "id", "coord_datum_epsg", "taxon_sibcs", "esqueleto"
)
covars_names <- colnames(soildata)[!colnames(soildata) %in% not_covars]
print(covars_names)

# Missing value imputation
# Use the missingness-in-attributes (MIA) approach with +/- Inf, with the indicator for missingness
# (mask) to impute missing values in the covariates
covariates <- imputation(soildata[, ..covars_names],
  method = "mia", na.replacement = list(cont = Inf, cat = "unknown"), na.indicator = TRUE
)
covariates <- data.table::as.data.table(covariates)
print(covariates)

# Prepare grid of hyperparameters
# num.trees, mtry, min.node.size and max.depth
num_trees <- c(100, 200, 400, 800)
mtry <- c(2, 4, 8, 16)
min_node_size <- c(1, 2, 4, 8)
max_depth <- c(10, 20, 30, 40)
hyperparameters <- expand.grid(num_trees, mtry, min_node_size, max_depth)
colnames(hyperparameters) <- c("num_trees", "mtry", "min_node_size", "max_depth")

# Fit ranger model testing different hyperparameters
t0 <- Sys.time()
hyper_results <- data.table()
for (i in 1:nrow(hyperparameters)) {
  print(hyperparameters[i, ])
  set.seed(1984)
  model <- ranger::ranger(
    y = soildata[!is_na_dsi, dsi],
    x = covariates[!is_na_dsi, ],
    num.trees = hyperparameters$num_trees[i],
    mtry = hyperparameters$mtry[i],
    min.node.size = hyperparameters$min_node_size[i],
    max.depth = hyperparameters$max_depth[i],
    replace = TRUE,
    verbose = TRUE
  )
  observed <- soildata[!is_na_dsi, dsi]
  predicted <- model$predictions
  error <- observed - predicted
  residual <- mean(observed) - observed
  me <- mean(error)
  mae <- mean(abs(error))
  mse <- mean(error^2)
  rmse <- sqrt(mse)
  nse <- 1 - mse / mean(residual^2)
  slope <- coef(lm(observed ~ predicted))[2]
  hyper_results <- rbind(hyper_results, data.table(
    num_trees = hyperparameters$num_trees[i],
    mtry = hyperparameters$mtry[i],
    min_node_size = hyperparameters$min_node_size[i],
    max_depth = hyperparameters$max_depth[i],
    me = me,
    mae = mae,
    rmse = rmse,
    nse = nse,
    slope = slope
  ))
}
Sys.time() - t0

# Export the results to a TXT file
data.table::fwrite(hyper_results, "res/tab/bulk_density_hyperparameter_tunning.txt", sep = "\t")
if (FALSE) {
  # Read the results from disk
  hyper_results <- data.table::fread("res/tab/bulk_density_hyperparameter_tunning.txt", sep = "\t")
}

# Assess results
# What is the Spearman correlation between hyperparameters and model performance metrics?
correlation <- round(cor(hyper_results, method = "spearman"), 2)
data.table::fwrite(correlation, "res/tab/bulk_density_hyperparameter_correlation.txt", sep = "\t")
print(correlation[1:4, 5:9])

# Sort the results by RMSE
hyper_results <- hyper_results[order(rmse)]

# Select the best hyperparameters
# Among smallest `rmse`, select the hyperparameters with the smallest `num_trees`.
# Then select the hyperparameters with the largest `nse`.
# Then select the hyperparameters with the smallest `max_depth`.
# Then select the hyperparameters with the smallest `mtry`.
# Then select the hyperparameters with the largest `min_node_size`.
digits <- 2
hyper_best <- round(hyper_results, digits)
hyper_best <- hyper_best[rmse == min(rmse), ]
hyper_best <- hyper_best[nse == max(nse), ]
hyper_best <- hyper_best[num_trees == min(num_trees), ]
hyper_best <- hyper_best[max_depth == min(max_depth), ]
hyper_best <- hyper_best[mtry == min(mtry), ]
hyper_best <- hyper_best[min_node_size == max(min_node_size), ]
print(hyper_best)

# Fit the best model
t0 <- Sys.time()
set.seed(2001)
dsi_model <- ranger::ranger(
  y = soildata[!is_na_dsi, dsi],
  x = covariates[!is_na_dsi, ],
  num.trees = hyper_best$num_trees,
  mtry = hyper_best$mtry,
  min.node.size = hyper_best$min_node_size,
  max.depth = hyper_best$max_depth,
  importance = "impurity",
  replace = TRUE,
  verbose = TRUE
)
Sys.time() - t0
print(dsi_model)

# Compute regression model statistics
dsi_model_stats <- error_statistics(soildata[!is_na_dsi, dsi], dsi_model$predictions)
# Write model statistics to disk
data.table::fwrite(dsi_model_stats, "res/tab/bulk_density_model_statistics.txt", sep = "\t")
print(round(dsi_model_stats, 2))

# Write model parameters to disk
write.table(capture.output(print(dsi_model))[6:15],
  file = "res/tab/bulk_density_model_parameters.txt", sep = "\t", row.names = FALSE
)

# Check absolute error
abs_error_tolerance <- 1
soildata[!is_na_dsi, abs_error := abs(soildata[!is_na_dsi, dsi] - dsi_model$predictions)]
if (any(soildata[!is_na_dsi, abs_error] >= abs_error_tolerance)) {
  print(soildata[
    abs_error >= abs_error_tolerance,
    .(id, camada_id, camada_nome, dsi, dsi_upper, dsi_lower, abs_error)
  ])
} else {
  print(paste0("All absolute errors are below ", abs_error_tolerance, " g/dm^3."))
}

# Figure: Variable importance
# Plot only those with relative importance >= 0.01
variable_importance_threshold <- 0.01
dsi_model_variable <- sort(dsi_model$variable.importance)
dsi_model_variable <- round(dsi_model_variable / max(dsi_model_variable), 2)
png("res/fig/bulk_density_variable_importance.png", width = 480 * 3, height = 480 * 4, res = 72 * 3)
par(mar = c(4, 6, 1, 1) + 0.1)
barplot(dsi_model_variable[dsi_model_variable >= variable_importance_threshold],
  horiz = TRUE, las = 1,
  col = "gray", border = "gray",
  xlab = "Relative importance >= 0.01", cex.names = 0.5
)
grid(nx = NULL, ny = FALSE, col = "gray")
dev.off()
dsi_model_variable[dsi_model_variable < variable_importance_threshold]


# Figure: Plot fitted versus observed values
# Set color of points as a function of the absolute error, that is, abs(y - x). The absolute error
# ranges from 0 to 1.
color_breaks <- seq(0, 1, by = 0.2)
color_class <- cut(soildata[!is_na_dsi, abs_error], breaks = color_breaks, include.lowest = TRUE)
color_palette <- RColorBrewer::brewer.pal(length(color_breaks) - 1, "Purples")
color_palette <- color_palette[as.numeric(color_class)]
png("res/fig/bulk_density_observed_versus_oob.png", width = 480 * 3, height = 480 * 3, res = 72 * 3)
par(mar = c(4, 4.5, 2, 2) + 0.1)
plot(
  y = soildata[!is_na_dsi, dsi], x = dsi_model$predictions, xlim = c(0, 2.5), ylim = c(0, 2.5),
  panel.first = grid(),
  pch = 21, bg = color_palette,
  ylab = expression("Observed soil bulk density, g cm"^-3),
  xlab = expression("Fitted bulk soil density (OOB), g cm"^-3)
)
abline(0, 1)
dev.off()

# Predict soil bulk density
dsi_digits <- 2
tmp <- predict(dsi_model, data = covariates[is_na_dsi, ])
soildata[is_na_dsi, dsi := round(tmp$predictions, dsi_digits)]
nrow(unique(soildata[, "id"])) # Result: 11 794
nrow(soildata) # Result: 21 847

# Figure. Distribution of soil bulk density data
png("res/fig/bulk_density_histogram.png", width = 480 * 3, height = 480 * 3, res = 72 * 3)
par(mar = c(5, 4, 2, 2) + 0.1)
hist(soildata[, dsi],
  xlab = expression("Soil bulk density, g cm"^-3),
  ylab = paste0("Absolute frequency (n = ", length(soildata[, dsi]), ")"),
  main = "", col = "gray", border = "gray"
)
grid(nx = FALSE, ny = NULL, col = "gray")
rug(soildata[!is_na_dsi, dsi])
# Legend: bars contain all data points, while the rug plot shows only the non-missing values used
# for model training
legend("topright",
  legend = c("All data (columns)", "Training data (rug)"), fill = c("gray", "black"),
  border = "white", box.lwd = "white" 
)
dev.off()

# Write data to disk
colnames(soildata)
soildata[, abs_error := NULL]
nrow(soildata[is_na_dsi, ]) # 19 036 layers
nrow(unique(soildata[, "id"])) # 11 751 events
data.table::fwrite(soildata, "data/30_soildata_soc.txt", sep = "\t")
