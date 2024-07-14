# title: SoilData - Soil Organic Carbon Stock
# subtitle: Clean data
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2024 CC-BY
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}

# Read SoilData data processed in the previous script
soildata <- data.table::fread("data/12_soildata_soc.txt", sep = "\t")
nrow(soildata) # 52 545 layers
nrow(unique(soildata[, "id"])) # 15 222 events

# Clean datasets
# ctb0001
soildata <- soildata[dataset_id != "ctb0001", ]
# ctb0654 (duplicate of ctb0608)
soildata <- soildata[dataset_id != "ctb0654", ]
# ctb0800 (many duplicates of ctb0702)
soildata <- soildata[dataset_id != "ctb0800", ]
nrow(unique(soildata[, "id"])) # 14 962 events
nrow(soildata) # 51 800 layers

# Clean layers

# Organic topsoil
# Some topsoil layers are organic layers that do not have a measurement of carbon. This can occur
# when the soil surface is moved to the top of organic layers.
# We start by identifying layers with profund_sup == 0, carbono == NA, and H or O in camada_nome.
# Then we correct the layer limits accordingly. Finally, we filter out layers with profund_sup < 0,
# i.e. the organic topsoil layers (litter)..
soildata[
  ,
  organic := any(profund_sup == 0 & is.na(carbono) &
    grepl("H|O|0", camada_nome, ignore.case = TRUE)),
  by = id
]
nrow(unique(soildata[organic == TRUE, "id"])) # 173 events
soildata[organic == TRUE, profund_sup := profund_sup - min(profund_inf), by = id]
soildata[organic == TRUE, profund_inf := profund_inf - min(profund_inf), by = id]
# Filter out layers with profund_sup < 0
soildata <- soildata[profund_sup >= 0, ]
soildata[, organic := NULL]
nrow(unique(soildata[, "id"])) # 14 075 events
nrow(soildata) # 50 487 layers

# Layer limits
# Some layers have equal values for profund_sup and profund_inf, generally the lowermost layer.
# This occurs because soil sampling and description ended at the top of the layer, producing a
# censoring effect. We add a fixed depth (plus_depth) to the lowermost layer when camada_nome has
# an R, D, or C. Otherwise, we keep the layer limits as they are.
nrow(soildata[profund_sup == profund_inf]) # 221 layers
plus_depth <- 20
soildata[
  profund_sup == profund_inf & grepl("R|D|C", camada_nome),
  profund_inf := profund_inf + plus_depth
]
nrow(soildata[profund_sup == profund_inf]) # 69 layers
# ATTENTION: SOME LAYERS HAVE THE DEPTH LIMITS EQUAL TO ZERO! THIS IS A PROBLEM THAT NEEDS TO BE
# SOLVED IN THE FUTURE.
soildata[profund_sup == profund_inf & profund_sup == 0, .(id, camada_nome, profund_sup, profund_inf)]

# Layer id
# Sort each event (id) by layer depth (profund_sup and profund_inf)
# Update the columns camada_id
soildata <- soildata[order(id, profund_sup, profund_inf)]
soildata[, camada_id := 1:.N, by = id]

# Remove repeated layers
# Some layers are repeated in the same event (id). These layers have equal values for camada_nome,
# profund_sup, and profund_inf. We create a new variable called repeated to identify these layers.
# Then, we filter out these layers.
soildata[,
  repeated := duplicated(camada_nome) & duplicated(profund_sup) & duplicated(profund_inf),
  by = id
]
print(soildata[repeated == TRUE, .(id, camada_nome, profund_sup, profund_inf, carbono)])
# ATTENTION: REPEATED LAYERS IN DATA FROM THE NATIONAL FOREST INVENTORY HAVE DIFFERENT SOIL
# PROPERTY VALUES. THIS IS A PROBLEM THAT NEEDS TO BE SOLVED IN THE FUTURE!
print(soildata[id == "ctb0055-PR_4", .(id, camada_nome, profund_sup, profund_inf, carbono)])
soildata <- soildata[repeated == FALSE, ]
soildata[, repeated := NULL]
nrow(unique(soildata[, "id"])) # 14 075 events
nrow(soildata) # 49 772 layers

# Soil end point (<= 30 cm)
# The variable 'endpoint' identifies if the soil profile sampling and description went all the way
# till its end, i.e. down to the bedrock (R).
# Identify layers that have the following:
# 1) camada_nome containing the letter R and
# 2) profund_sup starting before or at 30 cm and
# 3) carbono == NA
# 4) argila == NA
soildata[
  ,
  r_endpoint := grepl("R", camada_nome, ignore.case = TRUE) & profund_sup <= 30 &
    is.na(carbono) & is.na(argila)
]
soildata[r_endpoint == TRUE, .N] # 97 layers
soildata[, endpoint := ifelse(any(r_endpoint == TRUE), 1, NA_integer_), by = id]
soildata[, r_endpoint := NULL]
# If dataset_id == ctb0003 and profund_inf < 20, then endpoint := 1
soildata[
  dataset_id == "ctb0003" & profund_inf < 20,
  endpoint := 1
]
sum(soildata[["endpoint"]], na.rm = TRUE) # 352 events with endpoint <= 30 cm
print(soildata[endpoint == 1, .(id, camada_nome, profund_sup, profund_inf, carbono)])

# Maximum layer depth
# Filter out soil layers starting below a maximum depth of 30 cm
# We work only with data from the first 30 cm and deeper layers that start at or before 30 cm.
max_depth <- 30
nrow(soildata[profund_sup >= max_depth, ]) # 27 590 layers with profund_sup >= 30
soildata <- soildata[profund_sup >= 0 & profund_sup <= max_depth, ]
nrow(unique(soildata[, "id"])) # 13 846 events
nrow(soildata) # 26 011 layers

# Adjacent layers
# For each event (id), profund_inf of layer i should be equal to profund_sup of layer i + 1.
# For records with abs(diff) %in% 1:10, set profund_inf = profund_inf + (diff * -1)
# Filter out records for which abs(diff) > 10
soildata[, diff := profund_inf - data.table::shift(profund_sup, type = "lead"), by = id]
nrow(soildata[abs(diff) %in% 1:10, ]) # 1 951 layers
soildata[abs(diff) %in% 1:10, profund_inf := profund_inf + (diff * -1)]
soildata[, diff10 := any(diff > 10), id]
nrow(soildata[diff10 == TRUE, ]) # 336 layers
soildata <- soildata[diff10 == FALSE | is.na(diff10), ]
nrow(unique(soildata[, "id"])) # 13 736 events
nrow(soildata) # 25 675 layers
soildata[, diff := NULL]
soildata[, diff10 := NULL]

# Filter out layers with profund_sup == profund_inf
soildata <- soildata[profund_sup < profund_inf, ]
nrow(unique(soildata[, "id"])) # 13 707 events
nrow(soildata) # 25 610 layers

# Thickness
# Compute layer thickness
soildata[, espessura := profund_inf - profund_sup]

# Maximum layer thickness
# Filter out soil layers with thickness > 50 cm
# Many of these layers are below 30 cm depth or result form typing errors: a common recommendation
# of soil description and sampling manuals is to use a maximum layers thickness of 50 cm
max_thickness <- 50
nrow(soildata[espessura > max_thickness, ]) # 507 layers
soildata <- soildata[espessura <= max_thickness, ]
nrow(unique(soildata[, "id"])) # 13 615 events
nrow(soildata) # 24 103 layers

# Update layer id
# Sort each event (id) by layer depth (profund_sup and profund_inf)
soildata <- soildata[order(id, profund_sup, profund_inf)]
soildata[, camada_id := 1:.N, by = id]

# Topsoil
# For each event (id), check if there is a layer with profund_sup == 0. Missing a surface layer is
# common in reconnaissance soil surveys, where only the diagnostic subsurface horizons are 
# described. It can also occur in studies that use data from various sources and have a focus on
# subsurface horizons. 
# Filter out whole events without a topsoil layer.
soildata[, topsoil := any(profund_sup == 0), by = id]
nrow(unique(soildata[topsoil != TRUE, "id"])) # 571 events
soildata <- soildata[topsoil == TRUE, ]
nrow(unique(soildata[, "id"])) # 13 044 events
nrow(soildata) # 24 142 layers
soildata[, topsoil := NULL]

# Fine earth
# Correct samples with terrafina = 0 g/kg
# It is assumed that these are samples with missing data and that, when missing, the value of fine
# earth is 1000 g/kg.
nrow(soildata[terrafina == 0, ]) # 8 samples with terrafina == 0
soildata[terrafina == 0, terrafina := 1000]

# Soil skeleton
# In some soil samples, the fine earth and skeleton concentration data are inverted. This is quite
# common in cases where the skeleton concentration is greater than 800. The solution used here is
# to swap the values between the two variables. A better solution must be implemented and it
# involves going back to the source of the data. The, filter out samples with skeleton == 1000.
soildata[, esqueleto := 1000 - terrafina]
soildata[
  esqueleto > 800 & camada_nome %in% c("A", "A1", "B", "Bt", "B21", "B22", "Bw", "AB", "Ap"),
  terrafina := esqueleto
]
soildata[
  esqueleto > 800 & camada_nome %in% c("A", "A1", "B", "Bt", "B21", "B22", "Bw", "AB", "Ap"),
  esqueleto := 1000 - terrafina
]
nrow(soildata[esqueleto == 1000, ]) # 0 layers with esqueleto == 1000
soildata <- soildata[esqueleto < 1000, ]

# Clean camada_nome
soildata[, camada_nome := as.character(camada_nome)]
soildata[is.na(camada_nome) | camada_nome == "" & profund_sup == 0, camada_nome := "A"]
soildata[is.na(camada_nome) | camada_nome == "" & profund_sup != 0, camada_nome := NA_character_]
soildata[, camada_nome := gsub("^X", "", camada_nome, ignore.case = FALSE, perl = TRUE)]
soildata[, camada_nome := gsub("^I", "", camada_nome, ignore.case = FALSE, perl = TRUE)]
soildata[, camada_nome := gsub("^V", "", camada_nome, ignore.case = FALSE, perl = TRUE)]
soildata[, camada_nome := gsub("^III", "", camada_nome, ignore.case = FALSE, perl = TRUE)]
soildata[, camada_nome := gsub("^II", "", camada_nome, ignore.case = FALSE, perl = TRUE)]
soildata[, camada_nome := gsub("^11", "", camada_nome, ignore.case = FALSE, perl = TRUE)]
soildata[, camada_nome := gsub("^ll", "", camada_nome, ignore.case = FALSE, perl = TRUE)]
soildata[, camada_nome := gsub("^ii", "", camada_nome, ignore.case = FALSE, perl = TRUE)]
soildata[, camada_nome := gsub("^I", "", camada_nome, ignore.case = FALSE, perl = TRUE)]
soildata[, camada_nome := gsub("^X", "", camada_nome, ignore.case = FALSE, perl = TRUE)]
soildata[, camada_nome := gsub("Ap1", "Ap", camada_nome, ignore.case = FALSE)]
soildata[, camada_nome := gsub("p1", "pl", camada_nome, ignore.case = FALSE)]
soildata[, camada_nome := gsub(" ", "", camada_nome, ignore.case = FALSE)]
soildata[, camada_nome := gsub("1", "", camada_nome, ignore.case = FALSE)]
soildata[, camada_nome := gsub("2", "", camada_nome, ignore.case = FALSE)]
soildata[, camada_nome := gsub("3", "", camada_nome, ignore.case = FALSE)]
soildata[, camada_nome := gsub("4", "", camada_nome, ignore.case = FALSE)]
soildata[, camada_nome := gsub("pl", "f", camada_nome, ignore.case = FALSE)]
soildata[, camada_nome := gsub("cn", "c", camada_nome, ignore.case = FALSE)]
soildata[, camada_nome := gsub("Çg", "Cg", camada_nome, ignore.case = FALSE)]
soildata[, camada_nome := gsub("0", "O", camada_nome, ignore.case = FALSE)]
soildata[, camada_nome := gsub("+", "", camada_nome, ignore.case = FALSE)]
soildata[grepl("Tra", camada_nome, ignore.case = TRUE), camada_nome := "???"]
soildata[grepl("Sombrico", camada_nome, ignore.case = TRUE), camada_nome := "???"]
soildata[grepl("SUPERF", camada_nome, ignore.case = TRUE), camada_nome := "???"]
soildata[grepl("mudar", camada_nome, ignore.case = TRUE), camada_nome := "???"]
soildata[grepl("cam", camada_nome, ignore.case = TRUE), camada_nome := "???"]
soildata[grepl("Secçã", camada_nome, ignore.case = TRUE), camada_nome := "???"]
soildata[grepl("Crosta", camada_nome, ignore.case = TRUE), camada_nome := "???"]
soildata[grepl("NULL", camada_nome, ignore.case = TRUE), camada_nome := "???"]
soildata[grepl(",OOE+O", camada_nome, fixed = TRUE), camada_nome := "???"]
soildata[grepl("C.SUPE.", camada_nome, fixed = TRUE), camada_nome := "???"]
soildata[grepl("A9", camada_nome), camada_nome := "A"]
soildata[grepl("D", camada_nome), camada_nome := "R"]
soildata[is.na(camada_nome), camada_nome := "???"]
soildata[camada_nome == "", camada_nome := "???"]
soildata[grepl("AREIA", camada_nome, ignore.case = TRUE), camada_nome := "SAND"]
soildata[grepl("Leito", camada_nome, ignore.case = TRUE), camada_nome := "SAND"]
sort(unique(soildata[, camada_nome]))

# R layers
# Filter out layers with camada_nome == "R", creating a new variable called soil. 
# Filter out layers with camada_nome == "R", creating a new variable called soil. This variable is
# used to identify layers with soil (1) and rock (0). Then we set the bulk density of these rock
# layers to +Inf.
nrow(soildata[camada_nome == "R", ]) # 66 R layers
# soildata <- soildata[camada_nome != "R", ]
# soildata[, soil := 1]
# soildata[camada_nome == "R", soil := 0]

# # Soil bulk density data
# # We noticed that very high values (> 2.3 g/cm^3) were recorded for a few layers. There also were
# # A and B horizons with too low density values (< 0.5). These values are odd and the data was
# # deleted.
# nrow(soildata[dsi > 2.3 | dsi < 0.5, ]) # 51 layers
# soildata[dsi > 2.3 | dsi < 0.5, .(id, camada_nome, dsi)]
# soildata <- soildata[dsi <= 2.3 & dsi >= 0.5, ]
# nrow(unique(soildata[, "id"])) # 13 101 events
# nrow(soildata) # 24 252 layers

# # Soil carbon data
# # Filter out records missing both 'carbono' and 'dsi'
# soildata[is.na(carbono) & is.na(dsi), .N] # 2193 layers
# soildata[is.na(carbono) & is.na(dsi), .(id, camada_nome, carbono, dsi)]
# soildata <- soildata[!is.na(carbono) | !is.na(dsi), ]
# nrow(unique(soildata[, "id"])) # 13 101 events
# nrow(soildata) # 21 687 layers

# Particle size distribution
# Start by checking if all three fractions are present and, if so, check if their sum is 100%
# of 1000 g/kg -- the later is the standard! If sum(psd) != 1000, adjust all three values.
soildata[, psd := argila + silte + areia]
soildata[psd != 1000, argila := round(argila / psd * 1000)]
soildata[psd != 1000, silte := round(silte / psd * 1000)]
soildata[psd != 1000, areia := round(areia / psd * 1000)]
soildata[, psd := NULL]

# Correct bulk density values
soildata[id == "ctb0562-Perfil-13" & camada_id == 2, dsi := 0.86]
soildata[id == "ctb0562-Perfil-14" & camada_id == 1, dsi := 1.09]
soildata[id == "ctb0562-Perfil-14" & camada_id == 2, dsi := 0.9]
soildata[id == "ctb0608-15-V-RCC" & camada_id == 3, dsi := 1.94]
soildata[id == "ctb0631-Perfil-17" & camada_id == 3, dsi := 1.10]
soildata[id == "ctb0700-15" & camada_id == 1, dsi := 1.6]
soildata[id == "ctb0700-15" & camada_id == 2, dsi := 1.49]
soildata[id == "ctb0771-26" & camada_id == 1, dsi := 1.32]
soildata[id == "ctb0771-26" & camada_id == 2, dsi := 1.37]
soildata[id == "ctb0777-1" & camada_id == 1, dsi := 1.35]
soildata[id == "ctb0777-1" & camada_id == 2, dsi := 1.29]
soildata[id == "ctb0787-1" & camada_id == 2, dsi := 1.35]
soildata[id == "ctb0787-4" & camada_id == 1, dsi := 1.35]
soildata[id == "ctb0787-4" & camada_id == 2, dsi := 1.27]
soildata[id == "ctb0811-2" & camada_id == 3, dsi := 1.64]
soildata[id == "ctb0702-P-46" & camada_id == 1, dsi := 1.08] # check documentation
soildata[id == "ctb0572-Perfil-063" & camada_id == 2, dsi := 0.84]
soildata[id == "ctb0605-P-06" & camada_id == 2, dsi := 1.32]

# Clean events

# Identify duplicated events
# Duplicated events have equal spatial and temporal coordinates.
# Make sure to analise events with complete spatial and temporal coordinates.
soildata_events <- soildata[!is.na(coord_x) & !is.na(coord_y) & !is.na(data_coleta_ano), id[1],
  by = c("dataset_id", "observacao_id", "coord_x", "coord_y", "data_coleta_ano")
]
nrow(soildata_events) # 9653 events
test_columns <- c("coord_x", "coord_y", "data_coleta_ano")
duplo <- duplicated(soildata_events[, ..test_columns])
sum(duplo) # 274 duplicated events

# Jitter the coordinates of the duplicated events belonging to the following datasets:
# ctb0004, ctb0010, ctb0033, ctb0054, ctb0832, ctb0800, ctb0631, ctb0585
ctb <- c("ctb0004", "ctb0010", "ctb0033", "ctb0054", "ctb0832", "ctb0800", "ctb0631", "ctb0585")
soildata_events[duplo & dataset_id %in% ctb, coord_x := coord_x + runif(.N, -0.00001, 0.00001)]
duplo <- duplicated(soildata_events[, ..test_columns])
sum(duplo) # 201 duplicated events

# Remove remaining duplicated events
duplo <- soildata_events[duplo, V1]
soildata <- soildata[!id %in% duplo, ] # remove duplicated events
nrow(unique(soildata[, "id"])) # 11 751 events
nrow(soildata) # 21 750 layers
nrow(unique(soildata[!is.na(coord_x) & !is.na(coord_y), "id"])) # 9452 events

# Export cleaned data
colnames(soildata)
data.table::fwrite(soildata, "data/13_soildata_soc.txt", sep = "\t")
