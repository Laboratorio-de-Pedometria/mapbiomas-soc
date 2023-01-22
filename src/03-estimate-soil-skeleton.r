rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("ranger")) {
  install.packages("ranger")
}

# Ler dados do disco
febr_data <- data.table::fread("mapbiomas-solos/data/02-febr-data.txt", dec = ",", sep = "\t")
colnames(febr_data)

# Calculate layer thickness
febr_data[, espessura := profund_inf - profund_sup]

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

# Set categorical variables as factors
febr_data[, estado_id := as.factor(estado_id)]
state_table <- sort(table(febr_data[, estado_id]), decreasing = TRUE)
barplot(state_table, xlab = "Unidade da federação", ylab = "Frequência")

# Soil skeleton
# Em algumas amostras de solos, os dados da concentração de terra fina e esqueleto estão invertidos.
# Isso é bastante comum nos casos em que a concentração do esqueleto é maior do que 800.
# The default solution used here is to swap the values between the two variables. A better solution
# must be implemented and it involves going back to the source of the data.
febr_data[, esqueleto := 1000 - terrafina]
febr_data[esqueleto > 800, terrafina := esqueleto]
febr_data[esqueleto > 800, esqueleto := 1000 - terrafina]

# PREPARE PREDICTOR VARIABLES
# Stony soil
febr_data[, STONESOL := 0]
febr_data[sibcs_ordem == "NEOSSOLO", STONESOL := 1]
febr_data[sibcs_ordem == "PLINTOSSOLO", STONESOL := 1]
febr_data[sibcs_subordem == "FLUVICO", STONESOL := 1]
febr_data[sibcs_subordem == "LITOLICO", STONESOL := 1]
febr_data[sibcs_subordem == "REGOLITICO", STONESOL := 1]
febr_data[sibcs_subordem == "QUARTZARENICO", STONESOL := 0]
febr_data[sibcs_subordem == "HAPLICO", STONESOL := 0]

# camada_nome
febr_data[, camada_nome := as.character(camada_nome)]
febr_data[is.na(camada_nome) | camada_nome == "" & profund_sup == 0, camada_nome := "A"]
febr_data[is.na(camada_nome) | camada_nome == "" & profund_sup != 0, camada_nome := NA_character_]
febr_data[, camada_nome := gsub("^X", "", camada_nome, ignore.case = FALSE, perl = TRUE)]
febr_data[, camada_nome := gsub("^I", "", camada_nome, ignore.case = FALSE, perl = TRUE)]
febr_data[, camada_nome := gsub("^V", "", camada_nome, ignore.case = FALSE, perl = TRUE)]
febr_data[, camada_nome := gsub("^III", "", camada_nome, ignore.case = FALSE, perl = TRUE)]
febr_data[, camada_nome := gsub("^II", "", camada_nome, ignore.case = FALSE, perl = TRUE)]
febr_data[, camada_nome := gsub("^11", "", camada_nome, ignore.case = FALSE, perl = TRUE)]
febr_data[, camada_nome := gsub("^ll", "", camada_nome, ignore.case = FALSE, perl = TRUE)]
febr_data[, camada_nome := gsub("^ii", "", camada_nome, ignore.case = FALSE, perl = TRUE)]
febr_data[, camada_nome := gsub("^I", "", camada_nome, ignore.case = FALSE, perl = TRUE)]
febr_data[, camada_nome := gsub("^X", "", camada_nome, ignore.case = FALSE, perl = TRUE)]
febr_data[, camada_nome := gsub("Ap1", "Ap", camada_nome, ignore.case = FALSE)]
febr_data[, camada_nome := gsub("p1", "pl", camada_nome, ignore.case = FALSE)]
febr_data[, camada_nome := gsub("1", "", camada_nome, ignore.case = FALSE)]
febr_data[, camada_nome := gsub("2", "", camada_nome, ignore.case = FALSE)]
febr_data[, camada_nome := gsub("3", "", camada_nome, ignore.case = FALSE)]
febr_data[, camada_nome := gsub("4", "", camada_nome, ignore.case = FALSE)]
febr_data[, camada_nome := gsub("pl", "f", camada_nome, ignore.case = FALSE)]
febr_data[, camada_nome := gsub("cn", "c", camada_nome, ignore.case = FALSE)]
febr_data[, camada_nome := gsub("Çg", "Cg", camada_nome, ignore.case = FALSE)]
febr_data[, camada_nome := gsub("0", "O", camada_nome, ignore.case = FALSE)]
febr_data[grepl("Tra", camada_nome, ignore.case = TRUE), camada_nome := " "]
febr_data[grepl("Sombrico", camada_nome, ignore.case = TRUE), camada_nome := " "]
febr_data[grepl("SUPERF", camada_nome, ignore.case = TRUE), camada_nome := " "]
febr_data[grepl("mudar", camada_nome, ignore.case = TRUE), camada_nome := " "]
febr_data[grepl("cam", camada_nome, ignore.case = TRUE), camada_nome := " "]
febr_data[grepl("Secçã", camada_nome, ignore.case = TRUE), camada_nome := " "]
febr_data[grepl("AREIA", camada_nome, ignore.case = TRUE), camada_nome := "SAND"]
febr_data[grepl("+", camada_nome, ignore.case = TRUE, fixed = TRUE), camada_nome := ""]

# Concretions, nodules, rock fragments, rock-like pedogenic layers, and human artifacts
febr_data[, STONES := 0]
febr_data[grepl("c", camada_nome, ignore.case = FALSE), STONES := 1]
febr_data[grepl("F", camada_nome, ignore.case = FALSE), STONES := 1]
febr_data[grepl("^R$", camada_nome, ignore.case = FALSE, perl = TRUE), esqueleto := 1000]
febr_data[grepl("R", camada_nome, ignore.case = TRUE), STONES := 1]
febr_data[grepl("u", camada_nome, ignore.case = FALSE), STONES := 1]

# Layer depth
febr_data[, DEPTH := profund_sup + (espessura / 2)]
summary(febr_data[, DEPTH])
febr_data[is.na(DEPTH) & esqueleto == 1000, DEPTH := 2000]
febr_data[is.na(DEPTH), DEPTH := -999]
summary(febr_data[, DEPTH])

# Clay
summary(febr_data[, argila])
febr_data[, CLAY := argila]
febr_data[is.na(CLAY) & esqueleto == 1000, CLAY := 0]
febr_data[is.na(CLAY), CLAY := -999]
summary(febr_data[, CLAY])

# Sand
summary(febr_data[, areia])
febr_data[, SAND := areia]
febr_data[is.na(SAND) & esqueleto == 1000, SAND := 0]
febr_data[is.na(SAND), SAND := -999]
summary(febr_data[, SAND])

# Silt
summary(febr_data[, silte])
febr_data[, SILT := silte]
febr_data[is.na(SILT) & esqueleto == 1000, SILT := 0]
febr_data[is.na(SILT), SILT := -999]
summary(febr_data[, SILT])

# Carbon
summary(febr_data[, carbono])
febr_data[, CARBON := carbono]
febr_data[is.na(CARBON) & esqueleto == 1000, CARBON := 0]
febr_data[is.na(CARBON) & grepl("O", camada_nome, ignore.case = TRUE), CARBON := 500]
febr_data[is.na(CARBON) & grepl("^R", camada_nome), CARBON := 0]
febr_data[is.na(CARBON) & grepl("^F", camada_nome), CARBON := 0]
febr_data[
  is.na(CARBON) & grepl("E", camada_nome),
  CARBON := median(febr_data[grepl("E", camada_nome), CARBON], na.rm = TRUE)
]
febr_data[
  is.na(CARBON) & grepl("BC", camada_nome, ignore.case = TRUE),
  CARBON := median(febr_data[grepl("BC", camada_nome, ignore.case = TRUE), CARBON], na.rm = TRUE)
]
febr_data[
  is.na(CARBON) & grepl("Cr", camada_nome, ignore.case = TRUE),
  CARBON := median(febr_data[grepl("Cr", camada_nome, ignore.case = TRUE), CARBON], na.rm = TRUE)
]
febr_data[
  is.na(CARBON) & grepl("f", camada_nome, ignore.case = TRUE),
  CARBON := median(febr_data[grepl("f", camada_nome, ignore.case = FALSE), CARBON], na.rm = TRUE)
]
febr_data[
  is.na(CARBON) & grepl("g", camada_nome, ignore.case = TRUE),
  CARBON := median(febr_data[grepl("g", camada_nome, ignore.case = TRUE), CARBON], na.rm = TRUE)
]
febr_data[
  is.na(CARBON) & grepl("Ap", camada_nome, ignore.case = TRUE),
  CARBON := median(febr_data[grepl("Ap", camada_nome, ignore.case = TRUE), CARBON], na.rm = TRUE)
]
febr_data[
  is.na(CARBON) & grepl("Bt", camada_nome, ignore.case = TRUE),
  CARBON := median(febr_data[grepl("Bt", camada_nome, ignore.case = TRUE), CARBON], na.rm = TRUE)
]
febr_data[
  is.na(CARBON) & grepl("Bw", camada_nome, ignore.case = TRUE),
  CARBON := median(febr_data[grepl("Bw", camada_nome, ignore.case = TRUE), CARBON], na.rm = TRUE)
]
febr_data[
  is.na(CARBON) & grepl("Bh", camada_nome, ignore.case = TRUE),
  CARBON := median(febr_data[grepl("Bh", camada_nome, ignore.case = TRUE), CARBON], na.rm = TRUE)
]
febr_data[is.na(CARBON) & grepl("H", camada_nome, ignore.case = TRUE), CARBON := 500]
febr_data[
  is.na(CARBON) & grepl("Bs", camada_nome, ignore.case = TRUE),
  CARBON := median(febr_data[grepl("Bs", camada_nome, ignore.case = TRUE), CARBON], na.rm = TRUE)
]
febr_data[
  is.na(CARBON) & grepl("Bn", camada_nome, ignore.case = TRUE),
  CARBON := median(febr_data[grepl("Bn", camada_nome, ignore.case = TRUE), CARBON], na.rm = TRUE)
]
febr_data[
  is.na(CARBON) & grepl("Bi", camada_nome, ignore.case = TRUE),
  CARBON := median(febr_data[grepl("Bi", camada_nome, ignore.case = TRUE), CARBON], na.rm = TRUE)
]
febr_data[
  is.na(CARBON) & grepl("AC", camada_nome, ignore.case = TRUE),
  CARBON := median(febr_data[grepl("AC", camada_nome, ignore.case = TRUE), CARBON], na.rm = TRUE)
]
febr_data[
  is.na(CARBON) & grepl("V", camada_nome, ignore.case = TRUE),
  CARBON := median(febr_data[grepl("V", camada_nome, ignore.case = TRUE), CARBON], na.rm = TRUE)
]
febr_data[
  is.na(CARBON) & grepl("m", camada_nome, ignore.case = TRUE),
  CARBON := median(febr_data[grepl("m", camada_nome, ignore.case = TRUE), CARBON], na.rm = TRUE)
]
febr_data[
  is.na(CARBON) & grepl("u", camada_nome, ignore.case = TRUE),
  CARBON := median(febr_data[grepl("u", camada_nome, ignore.case = TRUE), CARBON], na.rm = TRUE)
]
febr_data[
  is.na(CARBON) & grepl("n", camada_nome, ignore.case = TRUE),
  CARBON := median(febr_data[grepl("n", camada_nome, ignore.case = TRUE), CARBON], na.rm = TRUE)
]
febr_data[is.na(CARBON), CARBON := -999]


hist(febr_data[grepl("C", camada_nome, ignore.case = TRUE), CARBON])
sort(unique(febr_data[is.na(CARBON), camada_nome]))
colnames(febr_data)  

unique(febr_data[STONES == 1, camada_nome])
febr_data[grepl("^R$", camada_nome, ignore.case = FALSE, perl = TRUE), c("camada_nome", "esqueleto")]
unique(febr_data[STONES == 1, camada_nome])
table(febr_data[, camada_nome])
boxplot(esqueleto ~ STONES, febr_data)

# MODEL
is_na_skeleton <- is.na(febr_data[, esqueleto])
model_formula <- esqueleto ~ STONESOL + STONES + DEPTH + CLAY + SILT + SAND + CARBON
fit <- ranger::ranger(model_formula, febr_data[!is_na_skeleton, ],
    importance = "impurity_corrected")
print(fit)
barplot(ranger::importance(fit) / max(ranger::importance(fit)))





# estado_id
febr_data[is.na(estado_id) | estado_id == "", estado_id := NA_character_]
febr_data[, estado_id := as.factor(as.character(estado_id))]

# HZA
febr_data[, HZA := "NOT_HZA"]
febr_data[grepl("A", camada_nome, ignore.case = TRUE), HZA := "HZA"]
febr_data[is.na(camada_nome), HZA := "NA"]

# HZE
febr_data[, HZE := "NOT_HZE"]
febr_data[grepl("E", camada_nome, ignore.case = TRUE), HZE := "HZE"]
febr_data[is.na(camada_nome), HZE := "NA"]

# HZB
febr_data[, HZB := "NOT_HZB"]
febr_data[grepl("B", camada_nome, ignore.case = TRUE), HZB := "HZB"]
febr_data[is.na(camada_nome), HZB := "NA"]

# LATOSSOLICO
febr_data[, LATOSSOLICO := "NOT_LATOSSOLICO"]
febr_data[grepl("w", camada_nome, ignore.case = TRUE), LATOSSOLICO := "LATOSSOLICO"]
febr_data[is.na(camada_nome), LATOSSOLICO := "NA"]

# TEXTURAL
febr_data[, TEXTURAL := "NOT_TEXTURAL"]
febr_data[grepl("t", camada_nome, ignore.case = TRUE), TEXTURAL := "TEXTURAL"]
febr_data[is.na(camada_nome), TEXTURAL := "NA"]



colnames(febr_data)

table(febr_data[, camada_nome])
