# MapBiomas Soil (beta): Script 02. Merge external data

_Alessandro Samuel-Rosa & Taciara Zborowski Horst_

_2023 CC-BY_

## Material and Methods

Several datasets were selected to be added to the FEBR in the last few months. However, these datasets have not been structured according to the data model used in the FEBR. The reason for this is that our main goal was to gather data that could be easily used to model the spatio-temporal variation of soil organic carbon stocks in Brazil. Therefore, we focused solely on collecting data that could help improve predictions, prioritizing key soil variables.

The primary data source during this phase was the National Forest Inventory (NFI). We also incorporated data from a private natural reserve in the Pantanal biome (SESC). Finally, we incorporated data from the Programa Nacional de Levantamento e Interpretação de Solos do Brasil (PronaSolos).

* NFI: https://snif.florestal.gov.br/pt-br/inventario-florestal-nacional-ifn/ifn-dados-abertos
* SESC: https://www.sescpantanal.com.br/arquivos/cadastro-itens/layout-6/arquivos/file-636003159185881485.pdf
* PRONASOLOS: http://geoinfo.cnps.embrapa.br/layers/geonode%3Aperfis_pronasolos_20201202v2

The soil data was processed in a series of steps to ensure the data was standardized and unwanted samples were removed. Due to resource limitations, laboratory method variations were disregarded during data processing, and new data was merged with the existing data without any modification. However, the coordinate reference system was standardized to EPSG:4326 to ensure consistency across the datasets. Additionally, the sampling date was cleaned, and any dates earlier than 1950 or later than 2023 were removed from the dataset and set to NA_integer_.

Next, the data from events and layers were merged, and essential columns such as terrafina and camada_nome were added. The following step was to remove events or layers that lacked sampling dates, which helped to discard any unwanted data from PronaSolos.

After completing the above steps, the previously processed FEBR data containing 14,190 events was read. During this data processing step, samples with terrafina = 0 were corrected. These samples were presumed to have missing data, and when data was missing, the value of fine earth was assumed to be 1000 g/kg.

All the data was then merged, and the next step was to check for any replicated sample points in the resulting dataset. Some events in the FEBR data had the same ID but different spatial or temporal coordinates. Although this could also happen in the external data, it was not checked. The problem events were identified by calculating the standard deviation of the coordinates of each event. For non-duplicated events, the standard deviation should be zero. The duplicated events, which had equal spatial and temporal coordinates, were then removed.

Lastly, duplicated events that have the same spatial and temporal coordinates were removed. Events are often reused in more than one dataset, and there is also significant overlap between the FEBR and PronaSolos data.

## Results

We started with 10,098 events and 36,883 layers from external data sources. We noticed that not all layers of the National Forest Inventory were linked to events, indicating errors in their database. After merging the data, only 9,487 events and 36,690 layers remained.

We were left with 7,682 events and 28,416 layers after merging the data and removing samples without a sampling date. This step was crucial in discarding any unwanted data from PronaSolos.

We merged the previously processed 14,190 events from the FEBR data using the previous scripts. The resulting data consisted of 21,872 events and 78,801 layers.

We also discovered 32 duplicated events with the same ID but different spatial or temporal coordinates. We identified them by computing the standard deviation of the coordinates of each event. The standard deviation should be zero for non-duplicated events. We removed these duplicated events, resulting in 21,840 events and 78,421 layers.

To eliminate duplicated events with equal spatial and temporal coordinates, which are often reused in more than one dataset due to the large overlap between FEBR and PronaSolos data, we removed such events. As a result, we were left with 12,729 events and 44,155 layers. We saved these results to disk for further processing.
