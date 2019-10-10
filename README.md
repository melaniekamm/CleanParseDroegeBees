# CleanParseDroegeBees

The goal of these scripts is to clean publically available wild bee occurence data from the United State Geological Survey Native Bee Inventory and Monitoring Lab (USGS BIML).
A general overview of our data cleaning process is as follows:
1. remove non-bee occurences
2. correct mis-spelled species and genus names
3. filter occurences to only those sampled with pan traps
4. create site, site-year, sampling event, and transect identifying variables
5. filter sites geograpically to Maryland, Delaware, and Washington DC USA
6. extract pan trap color, volume, number, and number missing from text field notes
7. summarize pan trapping method (trap color and volume) per transect
8. calculate bee abundance/day/trap per transect for each species

Original Data are from:
>Droege S, Sellers E (2017). USGS PWRC - Native Bee Inventory and Monitoring Lab (BIML). Version 1.5. United States Geological Survey. Occurrence dataset https://doi.org/10.15468/6autvb accessed via GBIF.org on 2018-11-09. 

R Scripts require current versions of data.table, dplyr, plyr, tidyr, reshape2, rgdal, sf, raster, stringr, and XML packages. Code was developed and tested in R version 3.6.0.
