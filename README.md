# ZoopQU39eDNA

**WORK IN PROGRESS:** *Zooplankton time series analysis for QU39*

**Contact:** Andreas Novotny, a.novotny\@oceans.ubc.ca

*This project is a collaboration between the Hakai Institute & The University of British Columbia, Institute for the Oceans and Fisheries.*

## About

This directory contains the code needed for visual and statistical analysis of QU39 Time series. For the bioinformatic analysis, see <https://github.com/andreasnovotny/AmpliconSeqAnalysis>. For the methods paper see: <https://github.com/andreasnovotny/ZoopMethodsComp>.

### Data:

This public directory does **not** contain any data. It contains code for retrieving the data from Google Drive.

### Files:

-   All data modifications, plotting and analysis is done in, **QU39ZP_Analysis.Rmd**.

-   **QU39Viewer.Rmd** provides a shiny-dashboard for some visualization.

-   The **Code** directory contains functions and scripts imported by the above notebooks.

## Status of analysis:

Currently the analysis covers COI data from 2017-2019 as well as environmental variables downloaded from the Hakai data portal.

### To Do:

-   Include 18S analysis, possibly for more years depending on availability.

-   Figure out a better way for handling the datasets.

-   "Correlation" analyses with moving frames. Taxa \~ Environment and Taxa \~ Taxa

-   Functional trait annotations?

-   ...
