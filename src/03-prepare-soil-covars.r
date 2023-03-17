# 03. PREPARE SOIL COVARIATES ######################################################################
# SUMMARY
# Soil covariates are predictor variables created using data on soil properties (clay content, soil
# classification and so on). These covariates will be necessary later on to train a random forest
# regression model. That regression model will be used to estimate the bulk density of soil samples
# that are missing data on such variable. The bulk density is a key soil property to compute SOC
# stocks.
# Soil covariates prepared in this script include categorical and continuous variables. A key
# feature of the process of creating soil covariates is the handling of missing data. For
# categorical variables, a new category named UNKNOWN is created to replace the NAs. For continuous
# variables, three new variables are created to replace the original:
# 1. A continuous variable with the NAs replaced with +Inf,
# 2. A continuous variable with the NAs replaced with -Inf, and
# 3. A categorical variable with two levels, ISNA and ISNOTNA, indicating if the data for a
#    particular sample was missing or not.
# This approach of handling missing data is based on the idea of incorporating the missingness in
# attributes (MIA, [1]). Our implementation differs from the original MIA in that MIA was designed
# to operate inside the decision/regression trees, that is, in each partition node. We are not aware
# of the impacts that this difference has on the prediction results.
# The exception to the process described above is the data on the concentration of soil organic
# carbon (SOC). For this soil property, we first performed a data imputation step using the average
# (mean) concentration of SOC of particular types of soil horizons and layers (organic layers, Ap,
# Bt, E and so on). The quasi-MIA approach was applied after the group imputation step.
# We also create bivariate categorical variables to record the presence/absence of particular
# features in the soil such as concretions and nodules.
# We noticed that, in some soil samples coming from the latest (2021) FEBR snapshot, the fine earth
# and skeleton concentration data are inverted. This is quite common in cases where the skeleton
# concentration is greater than 800 g/kg. The solution used here is to swap the values between the
# two variables for soil horizons such as A, Ap, Bt, and Bw.
# [1] B. E. T. H. Twala, M. C. Jones, and D. J. Hand, “Good methods for coping with missing data
#    in decision trees,” Pattern Recognition Letters, vol. 29, no. 7, pp. 950–956, May 2008, doi:
#    10.1016/j.patrec.2008.01.010. 
# KEY RESULTS
# The following soil covariates were created:
# * ORDER, SUBORDER, GREATGROUP, and SUBGROUP: Soil classification (multivariate; MIA)
# * STONESOL: Soil classes known for having a skeleton (bivariate)
# * STONES: Soil layers known for having concretions, nodules, rock fragments, rock-like pedogenic
#   layers, and human artifacts (bivariate)
# * AHRZN: A horizon (bivariate)
# * BHRZN: B horizon (bivariate)
# * DEPTH: Layer average depth (continuous; MIA)
# * CLAY: Clay content (continuous; MIA)
# * SAND: Sand content (continuous; MIA)
# * SILT: Silt content (continuous; MIA)
# * CARBON: Carbon content (continuous; group imputation + MIA)
# * CEC: Cation exchange capacity (continuous; MIA)
# * PH: pH (continuous; MIA)
# * LONG: Geographic coordinates - Longitude (continuous; MIA)
# * LAT: Geographic coordinates - Latitude (continuous; MIA)
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}

# Ler dados do disco
febr_data <- data.table::fread("mapbiomas-solos/data/02-febr-data.txt", dec = ",", sep = "\t")
nrow(unique(febr_data[, "id"])) # 15 141
nrow(febr_data) # 52 696
colnames(febr_data)

# Set categorical variables as factors
febr_data[, estado_id := as.factor(estado_id)]
state_table <- sort(table(febr_data[, estado_id]), decreasing = TRUE)
barplot(state_table, xlab = "Unidade da federação", ylab = "Frequência")

# Soil skeleton
# In some soil samples, the fine earth and skeleton concentration data are inverted. This is quite
# common in cases where the skeleton concentration is greater than 800. The solution used here is
# to swap the values between the two variables. A better solution must be implemented and it
# involves going back to the source of the data.
febr_data[, esqueleto := 1000 - terrafina]
febr_data[
  esqueleto > 800 & camada_nome %in% c("A", "A1", "B", "Bt". "B21", "B22", "Bw", "AB", "Ap"),
  terrafina := esqueleto]
febr_data[
  esqueleto > 800 & camada_nome %in% c("A", "A1", "B", "Bt". "B21", "B22", "Bw", "AB", "Ap"),
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
febr_data[grepl("AREIA", camada_nome, ignore.case = TRUE), camada_nome := "SAND"]
febr_data[grepl("Leito", camada_nome, ignore.case = TRUE), camada_nome := "SAND"]
unique(febr_data[, camada_nome])

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

# DEPTH: Layer average depth (continuous)
febr_data[, espessura := profund_inf - profund_sup]
febr_data[, DEPTH := profund_sup + (espessura / 2)]
febr_data[is.na(DEPTH) & esqueleto == 1000, DEPTH := 2000]
febr_data[, DEPTHplus := DEPTH]
febr_data[, DEPTHminus := DEPTH]
febr_data[, DEPTHna := "NOTNA"]
febr_data[is.na(DEPTH), DEPTHplus := +Inf]
febr_data[is.na(DEPTH), DEPTHminus := -Inf]
febr_data[is.na(DEPTH), DEPTHna := "NA"]
febr_data[, DEPTHna := as.factor(DEPTHna)]
febr_data[is.na(DEPTH), c("DEPTH", "DEPTHplus", "DEPTHminus", "DEPTHna")]
febr_data[, DEPTH := NULL]
febr_data[, espessura := NULL]

# Particle size distribution
# Start by checking if all three fractions are present and, if so, check if their sum is 100%
# of 1000 g/kg -- the later is the standard!
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
febr_data[, CLAYna := "NOTNA"]
febr_data[is.na(CLAY), CLAYplus := +Inf]
febr_data[is.na(CLAY), CLAYminus := -Inf]
febr_data[is.na(CLAY), CLAYna := "NA"]
febr_data[, CLAYna := as.factor(CLAYna)]
febr_data[is.na(CLAY), c("CLAY", "CLAYplus", "CLAYminus", "CLAYna")]
febr_data[, CLAY := NULL]

# SAND: Sand content (continuous)
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
febr_data[, SAND := NULL]

# SILT: Silt content (continuous)
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
febr_data[, CARBONna := "NOTNA"]
febr_data[is.na(CARBON), CARBONplus := +Inf]
febr_data[is.na(CARBON), CARBONminus := -Inf]
febr_data[is.na(CARBON), CARBONna := "NA"]
febr_data[, CARBONna := as.factor(CARBONna)]
febr_data[is.na(CARBON), c("CARBON", "CARBONplus", "CARBONminus", "CARBONna")]
febr_data[, CARBON := NULL]

# CEC: Cation exchange capacity (continuous; MIA)
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
febr_data[, CEC := NULL]

# PH: pH (continuous; MIA)
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
febr_data[, PH := NULL]

# LONG: Geographic coordinates - Longitude (continuous; MIA)
febr_data[, LONG := as.numeric(coord_x)]
febr_data[, LONGplus := LONG]
febr_data[, LONGminus := LONG]
febr_data[, LONGna := "NOTNA"]
febr_data[is.na(LONG), LONGplus := +Inf]
febr_data[is.na(LONG), LONGminus := -Inf]
febr_data[is.na(LONG), LONGna := "NA"]
febr_data[, LONGna := as.factor(LONGna)]
febr_data[is.na(LONG), c("LONG", "LONGplus", "LONGminus", "LONGna")]
febr_data[, LONG := NULL]

# LAT: Geographic coordinates - Latitude (continuous; MIA)
febr_data[, LAT := as.numeric(coord_y)]
febr_data[, LATplus := LAT]
febr_data[, LATminus := LAT]
febr_data[, LATna := "NOTNA"]
febr_data[is.na(LAT), LATplus := +Inf]
febr_data[is.na(LAT), LATminus := -Inf]
febr_data[is.na(LAT), LATna := "NA"]
febr_data[, LATna := as.factor(LATna)]
febr_data[is.na(LAT), c("LAT", "LATplus", "LATminus", "LATna")]
febr_data[, LAT := NULL]

# Write data to disk
data.table::fwrite(febr_data, "mapbiomas-solos/data/03-febr-data.txt", sep = "\t", dec = ",")
