# Estimating Patagonian toothfish (*Dissostichus eleginoides*) length at first maturity from their age, sex and temperature experience around South Georgia

Author(s): [Sarah Alewijnse](https://github.com/sarah-alewijnse){:target="_blank"}, [Stephen Gregory](https://github.com/stephendavidgregory){:target="_blank"}

Contact: [sarah.alewijnse@cefas.gov.uk](mailto:sarah.alewijnse@cefas.gov.uk)

This repository contains code used in this paper [http://doi.org/10.1111/jfb.70487](http://doi.org/10.1111/jfb.70487){:target="_blank"}.

To cite the paper:

> Marsh, J. E., Alewijnse, S. R., Gregory, S. D., Hollyman, P. R. and Söffker, M. (2026) Estimating Patagonian toothfish (*Dissostichus eleginoides*) length at first maturity from their age, sex and temperature experience around South Georgia. Journal of Fish Biology, *Early view*, DOI: [http://doi.org/10.1111/jfb.70487](http://doi.org/10.1111/jfb.70487){:target="_blank"}.

## Data

Data from Copernicus Marine Environment Monitoring Service (CMEMS) can be downloaded [here](https://data.marine.copernicus.eu/product/GLOBAL_MULTIYEAR_PHY_001_030/download?dataset=cmems_mod_glo_phy_my_0.083deg_P1M-m_202311){:target="_blank"}. 
You will need to select the monthly dataset for data from 1993 - 2021, and the interim monthly dataset for data from 2021 onwards.
The variable to select is `Sea water potential temperature at seafloor bottomT[°C]`

Ageing data are available on request from the [UK Polar Data Centre](https://www.bas.ac.uk/data/uk-pdc/){:target="_blank"}.

You will need to add these to a folder called **data** and create a config file pointing to the data files.

## Code

All code used in this projects is contained within the `code` folder:

* **01-data-wrangling** - code for combining the CMEMS bottom temperature data with the toothfish data, subsetting data, and producing summaries.
* **02-simulation-code** - code for running model simulations, in QMD format.
* **03-model-code** - code for running each model, including producing diagnostic and predictive plots.
* **04-plot-code** - additional plot code not contained within 03-model-code. Includes Figure 1a and the supplementary plot of degree months over time.

## Models

Contains the JAGS code for each of the candiate models.

## Licence

> All work was done for non-commercial purposes and is licenced under the conditions of the Open Government Licence found at: [http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3](http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3){:target="_blank"}

