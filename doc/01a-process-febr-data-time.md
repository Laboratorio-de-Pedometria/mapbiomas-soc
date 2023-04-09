# MapBiomas Soil (beta): Script 01a. Process FEBR data - time coordinate

_Alessandro Samuel-Rosa & Taciara Zborowski Horst_

_2023 CC-BY_

## Material and Methods

We start processing the latest (2021) FEBR snapshot. The 2021
snapshot contains data of 50 470 layers from 14 043 events. Some 4848
events are missing the time coordinate (year of sampling).
These events were identified and written to disk in a file named
no-time-coord.csv. Next, our team of data curators searched for the
date of sampling of each of these events in the source soil
survey reports (PDF files). The recovered time coordinates were
registered in a Google Spreadsheet. These data is used in this script
to update the year of sampling of the events in the 2021 FEBR
snapshot. However, for various events (3402), the date of sampling is
not registered in the source soil survey report or the soil survey
report itself was not available for us. Three approaches are used to
attribute a year of sampling to these events:

1. If the year of sampling is known for at least a few events in a
  soil survey report, then we compute the average (mean year of
  sampling) and use that estimate as the most likely year of
  sampling of any events for which such information is missing.
1. For soil surveys published in 1985 onwards and the year of
  sampling is unknown for all of its events, we check the year of
  publication of the report and of its latest references. Based on
  that time interval, we select an intermediate year, generally about
  two years before the publication of the survey report, and use that
  year as the most likely year of sampling of all of the events.
1. For soil surveys published before 1985, we set the year of
  sampling to 1948: this is done only for plotting purposes.

## Results

We started with 14 043 events and 50 470. Some 4848 events did not
have the sampling date recorded in the latest (2021) FEBR snapshot.
Our team of data curators recovered the sampling date of 4848 - 3402
= 1446 events from between 1957 and 2007. Using the average sampling
year of the source survey report, we attributed a sampling date to
3402 - 1869 = 1533 events. For the remaining 1869 events, we tried to
attribute a sampling date based on the year of publication of the
source survey report. However, this worked for only 1869 - 1712 = 157
events because they belonged to survey report published after 1984.
Then, for the remaining 1712 events, we set the year of sampling to
1948.
