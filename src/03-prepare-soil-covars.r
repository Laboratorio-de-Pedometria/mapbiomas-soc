# MapBiomas Soil (beta): Script 03. Prepare soil covariates
# Alessandro Samuel-Rosa & Taciara Zborowski Horst
# 2023 CC-BY
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}

# Read data processed in the previous script
febr_data <- data.table::fread("mapbiomas-solos/data/02-febr-data.txt", dec = ",", sep = "\t")
nrow(unique(febr_data[, "id"])) # 12 729 events
nrow(febr_data) # 44 158 layers
colnames(febr_data)

# Correct layer depth and name
febr_data[dataset_id == "ctb0829" & observacao_id == "P92", profund_inf := 8]
febr_data[
  dataset_id == "ctb0636" & observacao_id == "Perfil-03" & profund_sup == 0,
  camada_nome := "A1"
]

# Filter out soil layers with thickness > 50 cm
# Many of these layers are below 30 cm depth or result form typing errors: a common recommendation
# of soil description and sampling manuals is to use a maximum layers thickness of 50 cm
febr_data[, thickness := profund_inf - profund_sup]
nrow(febr_data[thickness > 50, ]) # 3729 layers with thickness > 50 cm
febr_data <- febr_data[thickness <= 50, ]
febr_data[, thickness := NULL]
nrow(unique(febr_data[, "id"])) # 12 455 events
nrow(febr_data) # 40 072 layers

# Filter out soil layers starting below 30 cm depth
# We work only with data from the first 30 cm and deeper layers that start at or before 30 cm.
# We also ignore organic layers in mineral soils: these layers generally are indicated with negative
# depth 
nrow(febr_data[profund_sup < 0, ]) # 04 layers with profund_sup < 0
nrow(febr_data[profund_sup >= 30, ]) # 20 814 layers with profund_sup >= 30
febr_data <- febr_data[profund_sup >= 0 & profund_sup < 30, ]
nrow(unique(febr_data[, "id"])) # Result: 12 186 events
nrow(febr_data) # Result: 19 254 layers

# Soil skeleton
# In some soil samples, the fine earth and skeleton concentration data are inverted. This is quite
# common in cases where the skeleton concentration is greater than 800. The solution used here is
# to swap the values between the two variables. A better solution must be implemented and it
# involves going back to the source of the data.
febr_data[, esqueleto := 1000 - terrafina]
febr_data[
  esqueleto > 800 & camada_nome %in% c("A", "A1", "B", "Bt", "B21", "B22", "Bw", "AB", "Ap"),
  terrafina := esqueleto]
febr_data[
  esqueleto > 800 & camada_nome %in% c("A", "A1", "B", "Bt", "B21", "B22", "Bw", "AB", "Ap"),
  esqueleto := 1000 - terrafina]

# PREPARE PREDICTOR VARIABLES
# Soil classification: ORDER, SUBORDER, GREATGROUP, SUBGROUP (multivariate)
febr_data[taxon_sibcs == "", taxon_sibcs := "NA NA NA NA"]
sibcs <- strsplit(febr_data[["taxon_sibcs"]], " ")
sibcs <- lapply(sibcs, function(x) {
  len <- length(x)
  if (len > 4) {
    x <- x[1:4]
  } else if (len < 4) {
    x <- c(x, rep("NA", 4 - len))
  }
  return(x)
})
sibcs <- data.table::as.data.table(do.call(rbind, sibcs))
febr_data[, ORDER := sibcs[, 1]]
febr_data[, SUBORDER := sibcs[, 2]]
febr_data[, GREATGROUP := sibcs[, 3]]
febr_data[, SUBGROUP := sibcs[, 4]]
febr_data[is.na(ORDER), ORDER := "UNKNOWN"]
febr_data[is.na(SUBORDER), SUBORDER := "UNKNOWN"]
febr_data[is.na(GREATGROUP), GREATGROUP := "UNKNOWN"]
febr_data[is.na(SUBGROUP), SUBGROUP := "UNKNOWN"]
febr_data[, ORDER := as.factor(ORDER)]
febr_data[, SUBORDER := as.factor(SUBORDER)]
febr_data[, GREATGROUP := as.factor(GREATGROUP)]
febr_data[, SUBGROUP := as.factor(SUBGROUP)]

# Soil classes known for having a skeleton (STONESOL) (bivariate)
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
# Newer dataset do not use the field "camada_nome", which has been replaced by the field
# "camada_id" in the recent updates of the FEBR. So, we check if "camada_nome" is missing and,
# if it is, we fill it up with whatever data is stored in "camada_id".
febr_data[is.na(camada_nome), camada_nome := camada_id]
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
febr_data[, camada_nome := gsub(" ", "", camada_nome, ignore.case = FALSE)]
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
febr_data[grepl("Crosta", camada_nome, ignore.case = TRUE), camada_nome := "UNKNOWN"]
febr_data[grepl("AREIA", camada_nome, ignore.case = TRUE), camada_nome := "SAND"]
febr_data[grepl("Leito", camada_nome, ignore.case = TRUE), camada_nome := "SAND"]
unique(febr_data[, camada_nome])

# ORGANIC: Organic layers (bivariate)
febr_data[, ORGANIC := "UNKNOWN"]
febr_data[carbono < 80, ORGANIC := "FALSE"]
febr_data[carbono >= 80, ORGANIC := "TRUE"]
febr_data[grepl("o", camada_nome), ORGANIC := "TRUE"]
febr_data[, ORGANIC := as.factor(ORGANIC)]
summary(febr_data[, ORGANIC])

# STONES: Soil layers known for having concretions, nodules, rock fragments, rock-like pedogenic
# layers, and human artifacts (bivariate)
febr_data[, STONES := "UNKNOWN"]
febr_data[camada_nome != "UNKNOWN", STONES := "FALSE"]
febr_data[grepl("c", camada_nome, ignore.case = FALSE), STONES := "TRUE"]
febr_data[grepl("F", camada_nome, ignore.case = FALSE), STONES := "TRUE"]
febr_data[grepl("^R$", camada_nome, ignore.case = FALSE, perl = TRUE), esqueleto := 1000]
febr_data[grepl("R", camada_nome, ignore.case = TRUE), STONES := "TRUE"]
febr_data[grepl("u", camada_nome, ignore.case = FALSE), STONES := "TRUE"]
febr_data[, STONES := as.factor(STONES)]

# AHRZN: A horizon (bivariate)
febr_data[, AHRZN := "UNKNOWN"]
febr_data[camada_nome != "UNKNOWN", AHRZN := "FALSE"]
febr_data[grepl("^A", camada_nome, ignore.case = FALSE), AHRZN := "TRUE"]
febr_data[, AHRZN := as.factor(AHRZN)]
summary(febr_data[, AHRZN])

# BHRZN: B horizon (bivariate)
febr_data[, BHRZN := "UNKNOWN"]
febr_data[camada_nome != "UNKNOWN", BHRZN := "FALSE"]
febr_data[grepl("^B", camada_nome, ignore.case = FALSE), BHRZN := "TRUE"]
febr_data[, BHRZN := as.factor(BHRZN)]
summary(febr_data[, BHRZN])

# Particle size distribution
# Start by checking if all three fractions are present and, if so, check if their sum is 100%
# of 1000 g/kg -- the later is the standard! If sum(psd) != 1000, adjust all three values.
febr_data[, psd := argila + silte + areia]
febr_data[psd != 1000, argila := round(argila / psd * 1000)]
febr_data[psd != 1000, silte := round(silte / psd * 1000)]
febr_data[psd != 1000, areia := round(areia / psd * 1000)]
febr_data[, psd := NULL]

# CLAY: Clay content (continuous)
febr_data[, CLAY := as.numeric(argila)]
febr_data[is.na(CLAY) & esqueleto == 1000, CLAY := 0]
febr_data[, CLAYplus := CLAY]
febr_data[, CLAYminus := CLAY]
febr_data[, CLAYna := "ISNOTNA"]
febr_data[is.na(CLAY), CLAYplus := +Inf]
febr_data[is.na(CLAY), CLAYminus := -Inf]
febr_data[is.na(CLAY), CLAYna := "ISNA"]
febr_data[, CLAYna := as.factor(CLAYna)]
febr_data[is.na(CLAY), c("CLAY", "CLAYplus", "CLAYminus", "CLAYna")]
febr_data[, CLAY := NULL]

# SAND: Sand content (continuous)
febr_data[, SAND := as.numeric(areia)]
febr_data[is.na(SAND) & esqueleto == 1000, SAND := 0]
febr_data[, SANDplus := SAND]
febr_data[, SANDminus := SAND]
febr_data[, SANDna := "ISNOTNA"]
febr_data[is.na(SAND), SANDplus := +Inf]
febr_data[is.na(SAND), SANDminus := -Inf]
febr_data[is.na(SAND), SANDna := "ISNA"]
febr_data[, SANDna := as.factor(SANDna)]
febr_data[is.na(SAND), c("SAND", "SANDplus", "SANDminus", "SANDna")]
febr_data[, SAND := NULL]

# SILT: Silt content (continuous)
febr_data[, SILT := as.numeric(silte)]
febr_data[is.na(SILT) & esqueleto == 1000, SILT := 0]
febr_data[, SILTplus := SILT]
febr_data[, SILTminus := SILT]
febr_data[, SILTna := "ISNOTNA"]
febr_data[is.na(SILT), SILTplus := +Inf]
febr_data[is.na(SILT), SILTminus := -Inf]
febr_data[is.na(SILT), SILTna := "ISNA"]
febr_data[, SILTna := as.factor(SILTna)]
febr_data[is.na(SILT), c("SILT", "SILTplus", "SILTminus", "SILTna")]
febr_data[, SILT := NULL]

# CARBON: Carbon content (continuous; layer-type average + MIA)
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
febr_data[, CARBONna := "ISNOTNA"]
febr_data[is.na(CARBON), CARBONplus := +Inf]
febr_data[is.na(CARBON), CARBONminus := -Inf]
febr_data[is.na(CARBON), CARBONna := "ISNA"]
febr_data[, CARBONna := as.factor(CARBONna)]
febr_data[is.na(CARBON), c("CARBON", "CARBONplus", "CARBONminus", "CARBONna")]
febr_data[, CARBON := NULL]

# CEC: Cation exchange capacity (continuous; MIA)
febr_data[, CEC := as.numeric(ctc)]
febr_data[is.na(CEC) & esqueleto == 1000, CEC := 0]
febr_data[, CECplus := CEC]
febr_data[, CECminus := CEC]
febr_data[, CECna := "ISNOTNA"]
febr_data[is.na(CEC), CECplus := +Inf]
febr_data[is.na(CEC), CECminus := -Inf]
febr_data[is.na(CEC), CECna := "ISNA"]
febr_data[, CECna := as.factor(CECna)]
febr_data[is.na(CEC), c("CEC", "CECplus", "CECminus", "CECna")]
febr_data[, CEC := NULL]

# PH: pH (continuous; MIA)
febr_data[, PH := as.numeric(ph)]
febr_data[is.na(PH) & esqueleto == 1000, PH := 0]
febr_data[, PHplus := PH]
febr_data[, PHminus := PH]
febr_data[, PHna := "ISNOTNA"]
febr_data[is.na(PH), PHplus := +Inf]
febr_data[is.na(PH), PHminus := -Inf]
febr_data[is.na(PH), PHna := "ISNA"]
febr_data[, PHna := as.factor(PHna)]
febr_data[is.na(PH), c("PH", "PHplus", "PHminus", "PHna")]
febr_data[, PH := NULL]

# LONG: Geographic coordinates - Longitude (continuous; MIA)
febr_data[, LONG := as.numeric(coord_x)]
febr_data[, LONGplus := LONG]
febr_data[, LONGminus := LONG]
febr_data[, LONGna := "ISNOTNA"]
febr_data[is.na(LONG), LONGplus := +Inf]
febr_data[is.na(LONG), LONGminus := -Inf]
febr_data[is.na(LONG), LONGna := "ISNA"]
febr_data[, LONGna := as.factor(LONGna)]
febr_data[is.na(LONG), c("LONG", "LONGplus", "LONGminus", "LONGna")]
febr_data[, LONG := NULL]

# LAT: Geographic coordinates - Latitude (continuous; MIA)
febr_data[, LAT := as.numeric(coord_y)]
febr_data[, LATplus := LAT]
febr_data[, LATminus := LAT]
febr_data[, LATna := "ISNOTNA"]
febr_data[is.na(LAT), LATplus := +Inf]
febr_data[is.na(LAT), LATminus := -Inf]
febr_data[is.na(LAT), LATna := "ISNA"]
febr_data[, LATna := as.factor(LATna)]
febr_data[is.na(LAT), c("LAT", "LATplus", "LATminus", "LATna")]
febr_data[, LAT := NULL]

# Write data to disk
data.table::fwrite(febr_data, "mapbiomas-solos/data/03-febr-data.txt", sep = "\t", dec = ",")
