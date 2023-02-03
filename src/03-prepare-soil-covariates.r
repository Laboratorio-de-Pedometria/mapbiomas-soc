rm(list = ls())
x11()
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
# Soil classes
sibcs <- strsplit(febr_data[["taxon_sibcs"]], " ")
idx_notaxon <- sapply(sibcs, length) == 0
sibcs[idx_notaxon] <- list(c(NA_character_, NA_character_))
idx_singletaxon <- sapply(sibcs, length) == 1
sibcs[idx_singletaxon] <- lapply(sibcs[idx_singletaxon], function(x) c(x, NA_character_))
sibcs <- do.call(rbind, sibcs)
febr_data[, ORDER := sibcs[, 1]]
febr_data[, SUBORDER := sibcs[, 2]]
febr_data[is.na(ORDER), ORDER := "UNKNOWN"]
febr_data[is.na(SUBORDER), SUBORDER := "UNKNOWN"]
febr_data[, ORDER := as.factor(ORDER)]
febr_data[, SUBORDER := as.factor(SUBORDER)]

# Soil classes known for having a skeleton
febr_data[, STONESOL := "UNKNOWN"]
febr_data[ORDER != "UNKNOWN" | SUBORDER != "UNKNOWN", STONESOL := "FALSE"]
febr_data[ORDER == "NEOSSOLO", STONESOL := "TRUE"]
febr_data[ORDER == "PLINTOSSOLO", STONESOL := "TRUE"]
febr_data[SUBORDER == "FLUVICO", STONESOL := "TRUE"]
febr_data[SUBORDER == "LITOLICO", STONESOL := "TRUE"]
febr_data[SUBORDER == "REGOLITICO", STONESOL := "TRUE"]
febr_data[SUBORDER == "QUARTZARENICO", STONESOL := "FALSE"]
febr_data[SUBORDER == "HAPLICO", STONESOL := "FALSE"]
febr_data[, STONESOL := as.factor(STONESOL)]
summary(febr_data[, STONESOL])

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
febr_data[, camada_nome := gsub("+", "", camada_nome, ignore.case = FALSE)]
febr_data[grepl("Tra", camada_nome, ignore.case = TRUE), camada_nome := "UNKNOWN"]
febr_data[grepl("Sombrico", camada_nome, ignore.case = TRUE), camada_nome := "UNKNOWN"]
febr_data[grepl("SUPERF", camada_nome, ignore.case = TRUE), camada_nome := "UNKNOWN"]
febr_data[grepl("mudar", camada_nome, ignore.case = TRUE), camada_nome := "UNKNOWN"]
febr_data[grepl("cam", camada_nome, ignore.case = TRUE), camada_nome := "UNKNOWN"]
febr_data[grepl("Secçã", camada_nome, ignore.case = TRUE), camada_nome := "UNKNOWN"]
febr_data[grepl("AREIA", camada_nome, ignore.case = TRUE), camada_nome := "SAND"]

# Concretions, nodules, rock fragments, rock-like pedogenic layers, and human artifacts
febr_data[, STONES := "UNKNOWN"]
febr_data[camada_nome != "UNKNOWN", STONES := "FALSE"]
febr_data[grepl("c", camada_nome, ignore.case = FALSE), STONES := "TRUE"]
febr_data[grepl("F", camada_nome, ignore.case = FALSE), STONES := "TRUE"]
febr_data[grepl("^R$", camada_nome, ignore.case = FALSE, perl = TRUE), esqueleto := 1000]
febr_data[grepl("R", camada_nome, ignore.case = TRUE), STONES := "TRUE"]
febr_data[grepl("u", camada_nome, ignore.case = FALSE), STONES := "TRUE"]
febr_data[, STONES := as.factor(STONES)]

# A horizon
febr_data[, AHRZN := "UNKNOWN"]
febr_data[camada_nome != "UNKNOWN", AHRZN := "FALSE"]
febr_data[grepl("^A", camada_nome, ignore.case = FALSE), AHRZN := "TRUE"]
febr_data[, AHRZN := as.factor(AHRZN)]
summary(febr_data[, AHRZN])

# B horizon
febr_data[, BHRZN := "UNKNOWN"]
febr_data[camada_nome != "UNKNOWN", BHRZN := "FALSE"]
febr_data[grepl("^B", camada_nome, ignore.case = FALSE), BHRZN := "TRUE"]
febr_data[, BHRZN := as.factor(BHRZN)]
summary(febr_data[, BHRZN])

# Layer thickness and average depth
febr_data[, espessura := profund_inf - profund_sup]
febr_data[, DEPTH := profund_sup + (espessura / 2)]
febr_data[is.na(DEPTH) & esqueleto == 1000, DEPTH := 2000]
febr_data[, DEPTHplus := DEPTH]
febr_data[, DEPTHminus := DEPTH]
febr_data[, DEPTHna := "NOTNA"]
febr_data[is.na(DEPTH), DEPTHplus := +Inf]
febr_data[is.na(DEPTH), DEPTHminus := -Inf]
febr_data[is.na(DEPTH), DEPTHna := "NA"]
febr_data[, CLAYna := as.factor(DEPTHna)]
febr_data[is.na(DEPTH), c("DEPTH", "DEPTHplus", "DEPTHminus", "DEPTHna")]

# Clay content
febr_data[, CLAY := as.numeric(argila)]
febr_data[is.na(CLAY) & esqueleto == 1000, CLAY := 0]
febr_data[, CLAYplus := CLAY]
febr_data[, CLAYminus := CLAY]
febr_data[, CLAYna := "NOTNA"]
febr_data[is.na(CLAY), CLAYplus := +Inf]
febr_data[is.na(CLAY), CLAYminus := -Inf]
febr_data[is.na(CLAY), CLAYna := "NA"]
febr_data[, CLAYna := as.factor(CLAYna)]
febr_data[is.na(CLAY), c("CLAY", "CLAYplus", "CLAYminus", "CLAYna")]

# Sand content
febr_data[, SAND := as.numeric(areia)]
febr_data[is.na(SAND) & esqueleto == 1000, SAND := 0]
febr_data[, SANDplus := SAND]
febr_data[, SANDminus := SAND]
febr_data[, SANDna := "NOTNA"]
febr_data[is.na(SAND), SANDplus := +Inf]
febr_data[is.na(SAND), SANDminus := -Inf]
febr_data[is.na(SAND), SANDna := "NA"]
febr_data[, SANDna := as.factor(SANDna)]
febr_data[is.na(SAND), c("SAND", "SANDplus", "SANDminus", "SANDna")]

# Silt content
febr_data[, SILT := as.numeric(silte)]
febr_data[is.na(SILT) & esqueleto == 1000, SILT := 0]
febr_data[, SILTplus := SILT]
febr_data[, SILTminus := SILT]
febr_data[, SILTna := "NOTNA"]
febr_data[is.na(SILT), SILTplus := +Inf]
febr_data[is.na(SILT), SILTminus := -Inf]
febr_data[is.na(SILT), SILTna := "NA"]
febr_data[, SILTna := as.factor(SILTna)]
febr_data[is.na(SILT), c("SILT", "SILTplus", "SILTminus", "SILTna")]

# Carbon content
febr_data[, CARBON := as.numeric(carbono)]
febr_data[is.na(CARBON) & esqueleto == 1000, CARBON := 0]
febr_data[is.na(CARBON) & grepl("O", camada_nome, ignore.case = TRUE), CARBON := 100]
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
febr_data[is.na(CARBON) & grepl("H", camada_nome, ignore.case = TRUE), CARBON := 100]
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
  is.na(CARBON) & grepl("n", camada_nome, ignore.case = TRUE) & camada_nome != "UNKNOWN",
  CARBON := median(febr_data[grepl("n", camada_nome, ignore.case = TRUE), CARBON], na.rm = TRUE)
]
febr_data[, CARBONplus := CARBON]
febr_data[, CARBONminus := CARBON]
febr_data[, CARBONna := "NOTNA"]
febr_data[is.na(CARBON), CARBONplus := +Inf]
febr_data[is.na(CARBON), CARBONminus := -Inf]
febr_data[is.na(CARBON), CARBONna := "NA"]
febr_data[, CARBONna := as.factor(CARBONna)]
febr_data[is.na(CARBON), c("CARBON", "CARBONplus", "CARBONminus", "CARBONna")]

# Whole soil bulk density
febr_data[, DENSITY := as.numeric(dsi)]
febr_data[, DENSITYplus := DENSITY]
febr_data[, DENSITYminus := DENSITY]
febr_data[, DENSITYna := "NOTNA"]
febr_data[is.na(DENSITY), DENSITYplus := +Inf]
febr_data[is.na(DENSITY), DENSITYminus := -Inf]
febr_data[is.na(DENSITY), DENSITYna := "NA"]
febr_data[, DENSITYna := as.factor(DENSITYna)]
febr_data[is.na(DENSITY), c("DENSITY", "DENSITYplus", "DENSITYminus", "DENSITYna")]

# Cation exchange capacity
febr_data[, CEC := as.numeric(ctc)]
febr_data[is.na(CEC) & esqueleto == 1000, CEC := 0]
febr_data[, CECplus := CEC]
febr_data[, CECminus := CEC]
febr_data[, CECna := "NOTNA"]
febr_data[is.na(CEC), CECplus := +Inf]
febr_data[is.na(CEC), CECminus := -Inf]
febr_data[is.na(CEC), CECna := "NA"]
febr_data[, CECna := as.factor(CECna)]
febr_data[is.na(CEC), c("CEC", "CECplus", "CECminus", "CECna")]

# pH
febr_data[, PH := as.numeric(ph)]
febr_data[is.na(PH) & esqueleto == 1000, PH := 0]
febr_data[, PHplus := PH]
febr_data[, PHminus := PH]
febr_data[, PHna := "NOTNA"]
febr_data[is.na(PH), PHplus := +Inf]
febr_data[is.na(PH), PHminus := -Inf]
febr_data[is.na(PH), PHna := "NA"]
febr_data[, PHna := as.factor(PHna)]
febr_data[is.na(PH), c("PH", "PHplus", "PHminus", "PHna")]

# Geographic coordinates - Longitude
febr_data[, LONG := as.numeric(coord_x)]
febr_data[, LONGplus := LONG]
febr_data[, LONGminus := LONG]
febr_data[, LONGna := "NOTNA"]
febr_data[is.na(LONG), LONGplus := +Inf]
febr_data[is.na(LONG), LONGminus := -Inf]
febr_data[is.na(LONG), LONGna := "NA"]
febr_data[, LONGna := as.factor(LONGna)]
febr_data[is.na(LONG), c("LONG", "LONGplus", "LONGminus", "LONGna")]

# Geographic coordinates - Latitude
febr_data[, LAT := as.numeric(coord_y)]
febr_data[, LATplus := LAT]
febr_data[, LATminus := LAT]
febr_data[, LATna := "NOTNA"]
febr_data[is.na(LAT), LATplus := +Inf]
febr_data[is.na(LAT), LATminus := -Inf]
febr_data[is.na(LAT), LATna := "NA"]
febr_data[, LATna := as.factor(LATna)]
febr_data[is.na(LAT), c("LAT", "LATplus", "LATminus", "LATna")]

# Write data to disk
data.table::fwrite(febr_data, "mapbiomas-solos/data/03-febr-data.txt", sep = "\t", dec = ",")

# hist(febr_data[grepl("C", camada_nome, ignore.case = TRUE), CARBON])
# sort(unique(febr_data[is.na(CARBON), camada_nome]))
# colnames(febr_data)  
# unique(febr_data[STONES == 1, camada_nome])
# febr_data[grepl("^R$", camada_nome, ignore.case = FALSE, perl = TRUE), c("camada_nome", "esqueleto")]
# unique(febr_data[STONES == 1, camada_nome])
# table(febr_data[, camada_nome])
# boxplot(esqueleto ~ STONES, febr_data)

# is_na_skeleton <- is.na(febr_data[, esqueleto])
# is_rock <- febr_data[, esqueleto] == 1000
# model_formula <- esqueleto ~ STONESOL + STONES + AHRZN + BHRZN +
#   DEPTHplus + DEPTHminus + DEPTHna +
#   CLAYplus + CLAYminus + CLAYna +
#   SILTplus + SILTminus + SILTna +
#   SANDplus + SANDminus + SANDna +
#   CARBONplus + CARBONminus + CARBONna +
#   DENSITYplus + DENSITYminus + DENSITYna +
#   CECplus + CECminus + CECna +
#   PHplus + PHminus + PHna +
#   LONGplus + LONGminus + LONGna +
#   LATplus + LATminus + LATna
# # MODEL
# fit_ranger <- ranger::ranger(model_formula, febr_data[!is_na_skeleton & !is_rock, ],
#   importance = "impurity", num.trees = 500
# )
# print(fit_ranger)
# fit_importance <- ranger::importance(fit_ranger)
# fit_importance <- sort(fit_importance / max(fit_importance))
# par(mar = c(5, 7, 4, 2) + 0.1)
# barplot(fit_importance, horiz = TRUE, las = 1, col = "white", border = "white", axes = FALSE)
# grid(nx = NULL, ny = FALSE)
# barplot(fit_importance, horiz = TRUE, las = 1, add = TRUE)
# plot(
#   x = fit_ranger$predictions, y = febr_data[!is_na_skeleton & !is_rock, esqueleto],
#   panel.first = grid(),
#   ylab = "Observed", xlab = "Predicted"
# )
# abline(a = 0, b = 1)
