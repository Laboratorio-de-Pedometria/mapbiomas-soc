# MapBiomas Soil (beta): Script 01a. Process FEBR data - time coordinate

_Alessandro Samuel-Rosa & Taciara Zborowski Horst_<br>
_2023 CC-BY_

## Material and Methods

We began processing the latest (2021) FEBR snapshot, which contains
data on 50,470 layers from 14,043 events. However, 4,848 events were
missing the time coordinate (year of sampling), and were identified
and saved to a file named "no-time-coord.csv". Our team of data
curators then searched for the sampling date of each of these events
in the source soil survey reports (PDF files). The recovered time
coordinates were recorded in a Google Spreadsheet, which was used to
update the year of sampling of the events in the 2021 FEBR snapshot.
However, for some events (3,402), the date of sampling was not
registered in the source soil survey report, or the report itself was
not available to us. To attribute a year of sampling to these events,
three approaches were used:

1. If the year of sampling is known for at least a few events in a
  soil survey report, then we compute the average (mean year of
  sampling) and use that estimate as the most likely year of sampling
  of any events for which such information is missing.
1. For soil surveys published in 1985 onwards and the year of sampling
  is unknown for all of its events, we check the year of publication
  of the report and of its latest references. Based on that time
  interval, we select an intermediate year, generally about two years
  before the publication of the survey report, and use that year as
  the most likely year of sampling of all of the events.
1. For soil survey events published prior to 1985 that lack a known sampling
  date, we set an arbitrarily low year as the sampling date. This allows us
  to plot these events on a separate column of the histogram, distinguishing
  them from the other data. Please note that this approach is solely intended
  for visualization purposes.

## Results

We started with 14,043 events and 50,470 layers. However, 4,848 events
did not have a sampling date recorded in the latest (2021) FEBR
snapshot. Our team of data curators was able to recover the sampling
date of 1,446 events from between 1957 and 2007. For 3,402 events, we
attributed a sampling date based on the average sampling year of the
source survey report. For the remaining 1,869 events, we attempted to
attribute a sampling date based on the year of publication of the
source survey report, but this only worked for 157 events because they
belonged to a survey report published after 1984. For the remaining 1,712
events, we set the year of sampling to an arbitrarily low value, and this
approach is intended solely for visualization purposes.
