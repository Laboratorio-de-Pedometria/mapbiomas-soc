# MapBiomas Soil (beta): Script 03. Prepare soil covariates

_Alessandro Samuel-Rosa & Taciara Zborowski Horst_<br>
_2023 CC-BY_

## Material and Methods

Soil covariates are predictor variables created using data on soil properties such as clay content, pH, and soil classification. These covariates will be used later on for training a random forest regression model. The regression model will be used to estimate the bulk density of soil samples that are missing data on this variable. Bulk density is a crucial soil property for calculating SOC stocks.

We start processing the data by filtering out soil layers with a thickness greater than 50 cm. Many of these layers are below our lowest depth of interest, which is 30 cm, or may be the result of typing errors. It is a common recommendation in soil description and sampling manuals to use a maximum layer thickness of 50 cm. Additionally, we filter out soil layers that start below 30 cm depth. We focus only on data from the first 30 cm and deeper layers that start at or before 30 cm. We also disregard organic layers in mineral soils, which are generally indicated by negative depth values.

When creating soil covariates based on particle size distribution (PSD), we check if all three fractions (sand, silt, and clay) are present. If all three fractions are present, we then check if their sum equals 100% or 1000 g/kg, which is the standard. If the sum of the particle size fractions does not equal 1000, we adjust the values of all three fractions accordingly.

Soil covariates prepared in this script include both categorical and continuous variables. One important aspect of creating soil covariates is handling missing data. For categorical variables, a new category named "UNKNOWN" is created to replace the missing values (NAs). For continuous variables, three new variables are generated as replacements:

1. A continuous variable where the missing values (NAs) are replaced with "+Inf".
2. A continuous variable where the missing values (NAs) are replaced with "-Inf".
3. A categorical variable with two levels, "ISNA" and "ISNOTNA", indicating whether the data for a particular sample was missing or not.

This approach for handling missing data is based on the concept of incorporating missingness in attributes, as described by Twala et al. (2008; 10.1016/j.patrec.2008.01.010). However, our implementation differs from the original algorithm in that MIA was specifically designed to work within decision/regression trees at each partition node. We acknowledge that the differences in implementation may have an impact on the prediction results, although the specific impacts are not yet known or fully understood.

The exception to the process described above is the data related to the concentration of soil organic carbon (SOC). In the case of SOC, we initially performed a data imputation step using the average (mean) concentration of SOC for specific types of soil horizons and layers, such as organic layers, Ap, Bt, E, etc. Following the group imputation step, the quasi-MIA approach was applied.

We also create bivariate categorical variables to indicate the presence or absence of specific features in the soil, such as concretions and nodules. The levels for these covariates are TRUE and UNKNOWN, eliminating the need for imputation of missing values.

Finally, we noticed that in some soil samples from the latest (2021) FEBR snapshot, the fine earth and skeleton concentration data are inverted. This inversion is common in cases where the skeleton concentration exceeds 800 g/kg. To address this issue, we swap the values between the two variables for soil horizons such as A, Ap, Bt, and Bw.

## Results

We found 3,729 layers with a thickness greater than 50 cm, which were subsequently filtered out. As a result, the database was reduced to 12,455 events and 40,072 layers. Additionally, we identified 4 layers with profund_sup < 0 and 20,814 layers with profund_sup >= 30, which were also filtered out. This led to a final dataset of 12,186 events and 19,254 layers that were used to create the covariates.

The following soil covariates were created:

* ORDER, SUBORDER, GREATGROUP, and SUBGROUP: Soil classification (multivariate; MIA)
* STONESOL: Soil classes known for having a skeleton (bivariate)
* ORGANIC: Organic layers with carbon >= 80 g/kg (bivariate)
* STONES: Soil layers known for having concretions, nodules, rock fragments, rock-like pedogenic: layers, and human artifacts (bivariate)
* AHRZN: A horizon (bivariate)
* BHRZN: B horizon (bivariate)
* CLAY: Clay content (continuous; MIA)
* SAND: Sand content (continuous; MIA)
* SILT: Silt content (continuous; MIA)
* CARBON: Carbon content (continuous; group imputation + MIA)
* CEC: Cation exchange capacity (continuous; MIA)
* PH: pH (continuous; MIA)
* LONG: Geographic coordinates - Longitude (continuous; MIA)
* LAT: Geographic coordinates - Latitude (continuous; MIA)
