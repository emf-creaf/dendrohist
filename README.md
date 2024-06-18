# Preprocessing and analysis for the DENDROHIST project

## Introduction
The need for the functions in this package arose while processing datasets from the DENDROHIST project (PID2021-126411OB-I00, Ministerio de Cienca e Innovación de España, www.creaf.cat/long-term-perspective-current-enviornmental-crisis-iberian-mediterranean-region-dendrochronological-and-historical-archives), whose IP is Dr. Laia Andreu Hayles (www.creaf.cat/staff/laia-andreu-hayles) from CREAF (www.creaf.cat).
The objectives of the project are the following (see URL of the project above):

> *"DENDROHIST aims to provide long-term critical climate information for the Iberian Mediterranean region using tree-ring and historical records. The integration of these two independent paleoclimate archives will  allow for a multidisciplinary approach to estimate the full range of past climate variability at different temporal and spatial scales, as well as the detailed occurrence and recurrence frequencies of extreme climate events."*

The achievement of these objectives involve, among other things, the acquisition, preprocessing and analyses of different datasets from varying origins. Some of those datasets are publicly available (see below), whereas some others are not. With these ideas in mind we have set out to design and implement a series of functions, written in R and collected into a so-called R package, that can simplify the tasks ahead. 
Those tasks for which functions in this package are suited are the following:.

1. Processing of time series of rain data from Spanish meteorological stations.
2. Checking of internal coherence of the historical flood database.


## Datasets
Datasets that are employed within the context of this project have different origins, as mentioned above. First, there are databases from several public Spanish institutions (e.g. INE - Instituto Nacional de Estadística, or the IGN - Instituto Geológico Nacional), available at a number of official webs, that play an important role in characterizing the study sites from a topographical, hydrological, meteorologic or climatic point of view. Second, there are other non-public databases, collected and curated for this project, that describe flood, droughts and other disturbances of the  past that took place across the Spanish mainland and archipelagos (at this time, only the Balearic Islands are included).


## Processing of precipitation time series
Precipitation time series include daily and monthly datasets from a sets of meteorological stations across the Spanish mainland and the Balearic islands (the Canary Islands have been excluded for the time being). Those time series usually cover *** years, although that coverage vary from site to site.

## Checking of historical flood database
A great deal of work during the past years has been devoted to collecting catastrophic flood data from public and private records at local, county, province or regional level.
