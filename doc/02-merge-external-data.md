# MapBiomas Soil (beta): Script 02. Merge external data

_Alessandro Samuel-Rosa & Taciara Zborowski Horst_<br>
_2023 CC-BY_

## Material and Methods

Several datasets were selected to be added to the FEBR in the last few months. However, these
datasets have not been structured according to the data model used in the FEBR. The reason for this
is that our main goal was to gather data that could be easily and readily used to model the
spatio-temporal variation of soil organic carbon stocks in Brazil. Therefore, we focused solely on
collecting data that could help improve predictions, prioritizing key soil variables.

During this phase, the National Forest Inventory (NFI) served as the main data source. Additionally,
we included data from a private natural reserve in the Pantanal biome (SESC). Lastly, we integrated
data from the National Soil Survey and Interpretation Program of Brazil (PronaSolos).

* NFI: <https://snif.florestal.gov.br/pt-br/inventario-florestal-nacional-ifn/ifn-dados-abertos>
* SESC: <https://www.sescpantanal.com.br/arquivos/cadastro-itens/layout-6/arquivos/file-636003159185881485.pdf>
* PRONASOLOS: <http://geoinfo.cnps.embrapa.br/layers/geonode%3Aperfis_pronasolos_20201202v2>

The soil data underwent a series of standardization and filtering steps. As part of this process,
the coordinate reference system was standardized to EPSG:4326 to ensure consistency across the
datasets. Moreover, the sampling date was cleaned, with any dates prior to 1950 or beyond 2023 being
assigned the value NA_integer_. The decision not to infer the sampling date was made to avoid
introducing further uncertainties into the data. In light of limited resources, variations in
laboratory methods were disregarded during the data processing, and the new data was merged with the
existing data without any alterations.

Next, the data from events and layers from different data sources were merged, and important data
fields such as 'terrafina' and 'camada_nome' were added. The next step involved removing events and
layers that lacked sampling dates, some of which were created in the previous processing step. This
step was necessary to discard problematic data from PronaSolos.

After completing the previous steps, the previously processed FEBR data, consisting of 14,190
events, was loaded. During this data processing step, samples with 'terrafina' = 0 were corrected.
It was assumed that these samples had missing data, and when the data was missing, the value of fine
earth was assumed to be 1000 g/kg.

All the data from the different data sources was then merged, and the next step was to check for any
replicated sample points in the resulting merged dataset. During this step, it was noticed that some
events in the FEBR data had the same ID but different spatial or temporal coordinates. Although this
could also happen in the external data, it was not checked. The problem events were identified by
calculating the standard deviation of the coordinates of each event. For non-duplicated events, the
standard deviation should be zero. The duplicated events, which had equal spatial and temporal
coordinates, were then removed.

Lastly, duplicated events that have the same spatial and temporal coordinates were removed. Events
are often reused in more than one dataset, and there is also a significant overlap between the FEBR
and PronaSolos data. For example, many sample points used in PronaSolos had spatial coordinates
attributed using the FEBR data.

## Results

We started with 10,098 events and 36,883 layers from the three selected external data sources: NFI,
PronaSolos, and SESC. We noticed that not all layers of the National Forest Inventory were linked to
events, indicating errors in their database. After merging the data, only 9,487 events and 36,690
layers remained.

We were left with 7,681 events and 28,414 layers after merging the data and removing samples without
a sampling date. This step was crucial in discarding problematic data from PronaSolos.

We merged the previously processed 14,190 events from the FEBR data using the previous scripts. The
resulting data consisted of 21,871 events and 78,799 layers.

We also discovered 32 duplicated events with the same ID but different spatial or temporal
coordinates. We identified them by computing the standard deviation of the coordinates of each
event. The standard deviation should be zero for non-duplicated events. We removed these duplicated
events, resulting in 21,839 events and 78,419 layers.

To eliminate duplicated events with equal spatial and temporal coordinates, which are often reused
in more than one dataset due to the large overlap between FEBR and PronaSolos data, we removed such
events. As a result, we were left with 12,729 events and 44,158 layers. We saved these results to
disk for further processing.
