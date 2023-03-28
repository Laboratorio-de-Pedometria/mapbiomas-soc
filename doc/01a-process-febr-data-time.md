# MapBiomas Soil (beta): Script 01a. Process FEBR data - time coordinate

_Alessandro Samuel-Rosa & Taciara Zborowski Horst_

_2023 CC-BY_

## Material and Methods

We start processing the latest (2021) FEBR snapshot. The 2021 snapshot contains data of 50 470
layers from 14 043 events. Some 4848 events are missing the time coordinate (year of sampling).
These events were identified and written to disk in a file named no-time-coord.csv. Next, our team
of data curators searched for the date of sampling of each of these events in the source soil
survey reports (PDF files). The recovered time coordinates were registered in a Google
Spreadsheet. These data is used in this script to update the year of sampling of the events in the
2021 FEBR snapshot. However, for various events (3402), the date of sampling is not registered in
the source soil survey report or the soil survey report itself was not available. Three approaches
are used to attribute a year of sampling to these events:

1. If the year of sampling is known for at least a few events in a soil survey report, then we 
  compute the average (mean year of sampling) and use that estimate as the most likely year of
  sampling of any events for which such information is missing.
2. For soil surveys published in 1985 onwards and the year of sampling is unknown for all of its
  events, we check the year of publication of the report and of its latest references. Based on
  that time interval, we select an intermediate year, generally about two years before the
  publication of the survey report, and use that year as the most likely year of sampling of all
  of the events.
3. For soil surveys published before 1985, we attribute a random year of sampling. The random
  value is sampled from a uniform distribution using runif(min = 1970, max = 1984) for RADAMBRASIL
  survey reports and runif(min = 1960, max = 1984) for all others.

The year of 1985 is used as the lowest limit of interest because it is the first year of the time
series of Landsat imagery and MapBiomas land use/land cover maps for which there is consistent
data available. These data will be used to train the digital soil mapping models that will be
used to map the soil organic carbon stocks throughout the Brazilian territory. During the digital
soil mapping phase, any value lower than 1985 will be overwritten and set to 1985. Here, for the
purpose of quality assessment of the training data, we keep the year of sampling and indicate
those events for which it had to be guessed.

## Results

We started with 14 043 events and 50 470. Some 4848 events did not have the sampling date
recorded in the latest (2021) FEBR snapshot. Our team of data curators recovered the sampling date
of 4848 - 3402 = 1446 events from between 1957 and 2007. Using the average sampling year of the
source survey report, we attributed a sampling date to 3402 - 1869 = 1533 events. For the
remaining 1869 events, we tried to attribute a sampling date based on the year of publication of
the source survey report. However, this worked for only 1869 - 1712 = 157 events because they
belonged to survey report published after 1984. Then, for 1712 - 786 = 926 events belonging to the
RADAMBRASIL project, we attributed a random year of sampling between 1970 and 1984. For the
remaining 786 events, we attributed a random year of sampling between 1960 and 1984.
