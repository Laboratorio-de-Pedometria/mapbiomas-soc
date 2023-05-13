# MapBiomas Soil (beta): Script 01a. Process FEBR data - Rondônia

_Alessandro Samuel-Rosa & Taciara Zborowski Horst_<br>
_2023 CC-BY_

## Material and Methods

The FEBR repository comprises three datasets obtained during the
Socioeconomic-Ecological Zoning of the State of Rondônia, conducted in
1996 and 1997. Only one of these datasets (ctb0032) has been fully
processed, but it contains only morpho-mechanic soil properties that
are irrelevant to the current study. This dataset has already been
processed in the previous script (01a-process-febr-data-time.r).
However, the other two datasets (ctb0033 and ctb0034) contain crucial
information regarding the physical and chemical properties of soil
necessary to calculate soil organic carbon stocks. In this script, we
process and merge these two datasets and discard the first (ctb0032).

To start, we read the event data from each dataset and process both
the temporal and spatial coordinates. Since the original soil survey
report did not contain information on the coordinate reference system,
we set EPSG:4326 as the standard coordinate reference system for
spatial coordinates. Next, we read the layer data and standardize the
measurement units of key soil properties used to calculate soil
organic carbon stocks.

The dataset includes events that contain seemingly duplicated layers
for the 0-20 cm depth. However, these samples are actually additional
ones collected near the event (soil profile) for soil fertility
assessment. It is worth noting that these samples use the same ID as
the soil profile, which is a common practice in soil surveys.

Finally, we merge the resulting data of the Socioeconomic-Ecological
Zoning of the State of Rondônia with the data processed in the
previous script (01a-process-febr-data-time.r).

## Results

The dataset ctb0033 comprises 2998 events, while ctb0034 contains
another 107 events. Upon merging, we obtained a total of 2999 events,
indicating that one event from ctb0034 was not present in ctb0033. Of
these events, 87 were missing the sampling date, which we set to 1996
for consistency.

The dataset ctb0033 contains 10,779 layers, while ctb0034 contains an
additional 419 layers. Upon merging, we obtained a total of 10,785
layers, indicating that ctb0034 contributed 6 layers that were not
originally present in ctb0033.

Upon investigation, it was discovered that 63 layers initially thought
to be duplicated were actually extra samples from 24 events. To
distinguish these samples, we renamed them by concatenating the
corresponding event and layer IDs, resulting in a total of 3061 unique
events. To avoid clustering of the extra samples at the exact same
coordinates, we applied a random perturbation to their coordinates
using the st_jitter() function from the R package sf. The "amount"
argument was set to 200 m, with the perturbation randomly generated
within this range using the runif(1, -amount, amount) function.

After merging the Rondônia dataset with the previously processed data,
we obtained a total of 14,190 events and 50,385 layers. This
represents a net increase of 147 events and 85 layers compared to the
previous processing step, which was documented in
01a-process-febr-data-time.
