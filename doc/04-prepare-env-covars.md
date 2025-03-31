# MapBiomas Soil (beta): Script 04. Prepare environmental covariates

_Alessandro Samuel-Rosa & Taciara Zborowski Horst_<br>
_2023 CC-BY_

## Material and Methods

Environmental covariates are predictor variables sampled from maps of soil properties and other environmental information. Like the soil variables, the environmental covariates will be used to train a random forest regression model to estimate the bulk density of soil samples that are missing data on such variable.

Sampling environmental covariates requires the events to have spatial coordinates. Thus, we start by filtering out those event that are not geolocalized. Then we sample two data sets. The first is SoilGrids 250m v2.0, a collection of soil property maps available for six depth intervals, three of which are of our interest: 0-5, 5-15, and 15-30 cm. The soil properties of our interest are:

* clay content (clay_mean)
* sand content (sand_mean)
* SOC content (soc_mean)
* coarse fragments volume (cfvo_mean)
* bulk density (bdod_mean)
* pH (phh2o_mean)

The second data set is MapBiomas Land Use/Land Cover Collection 7.1. This data set contains data covering the period between 1985 and 2021. After sampling the raster layers, we identify and retain for each event the land use/land cover at the year at which it was collected in the field.

Both SoilGrids and MapBiomas data are available on Google Earth Engine:

* GEE/projects/soilgrids
* GEE/projects/mapbiomas-workspace/public/collection7_1/mapbiomas_collection71_integration_v1
  
Because sampling data on Google Earth Engine has limitations, we perform the operation using subsets containing at most 5000 or 1000 events for SoilGrids and MapBiomas, respectively. For both data sources, missing data is handled using the MIA approach described earlier for soil covariates.

## Results

Out of the 12 186 existing events (19 254 layers), 12 139 are geolocalized (19 141 layers). With these events, we sampled data from SoilGrids 250m v2.0 (clay, sand, SOC, pH and bulk density) at three depth intervals (0-5, 5-15, and 15-30 cm) -- 340 events returned NAs.

With the same events we also sampled MapBiomas Land Use/Land Cover Collection 7.1 -- six events returned NAs.
