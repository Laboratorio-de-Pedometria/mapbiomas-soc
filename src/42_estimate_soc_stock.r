# title: SoilData - Soil Organic Carbon Stock
# subtitle: Estimate SOC stock
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2024 CC-BY
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("randomForestSRC")) {
  install.packages("randomForestSRC")
  library(randomForestSRC)
}

# Source helper functions
source("src/00_helper_functions.r")

# Read data processed in the previous script
soildata <- data.table::fread("data/41_soildata_soc.txt", sep = "\t")
summary_soildata(soildata)
# Layers: 10378
# Events: 10378
# Georeferenced events: 8311

# Missingness
# Select all columns of soildata, except id, soc_stock_kgm2, and endpoint
# randomForestSRC::rfsrc does not handle the +/- Inf MIA approach, so that we use +/- 99999 instead
colnames(soildata)
soildata_mia <- imputation(soildata[, !c("id", "soc_stock_kgm2", "endpoint"), with = FALSE],
  method = "mia", na.indicator = TRUE, na.replacement = list(cat = "???", cont = 99999)
)
# Identify character columns. Then set them as factors, except id
char_cols <- sapply(soildata_mia, is.character)
soildata_mia[, char_cols] <- lapply(soildata_mia[, char_cols], as.factor)
head(soildata_mia)
str(soildata_mia)

# Combine the imputed data with the original data
soildata_mia <- cbind(soildata[, c("id", "soc_stock_kgm2", "endpoint")], soildata_mia)
colnames(soildata_mia)

# Model training
# ?randomForestSRC::rfsrc
# Set maximum number of trees
# num_trees <- ceiling(nrow(soildata_mia) * 0.25) # Result: 1509
set.seed(1984)
soc_stock_survival <- randomForestSRC::rfsrc(
  Surv(soc_stock_kgm2, endpoint) ~ ., soildata_mia[, !"id"],
  ntree = 200, # Number of trees to grow
  nodesize = 15,
  nsplit = 10, # Non-negative integer specifying number of random splits for splitting a variable
  importance = "none", # Can be recovered later using vimp() or predict()
  block.size = 1, # Number of trees in a block used to compute the cumulative error rate
  do.trace = 1
  # Vector of values specifying the time points to be used for survival to constrain ensemble
  # calculations
  # ntime = seq_len(ceiling(max(soildata_mia[, soc_stock_kgm2]))),
  # save.memory = TRUE
)
