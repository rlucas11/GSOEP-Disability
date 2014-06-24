GSOEP-Disability
================

This goal of this project is to clarify the discrepancies between two papers that examine the effect of disability on life satisfaction using longitudinal data from the GSOEP. Specifically, I test whether differnt analytic strategies (Fixed Effects Models versus Multilevel modeling) are responsible for these discrepancies. 

## Functions

The functions needed to clean and extract data, along with some basic information that is used as input to those functions, is stored in lib/gsoep.R. Source this file before doing anything.

## Extracting Data

The file munge/01-A.R actualy does most of the data extraction and cleaning. Run this file second to get usable data files. Data are cached and stored in the cache directory, though the datafiles are ignored by git so as not to be uploaded to public servers. 

## Analyses

The file src/disability.R includes a few analysis-specific data-cleaning procedures. Most importantly, it contains the model statements themselves. Run this to test the relevant models.