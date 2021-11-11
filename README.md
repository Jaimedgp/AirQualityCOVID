# Estimating changes in air pollutant levels due to COVID-19 lockdown measures in Spain

This repository contains all the code developed for the article _"Estimating changes in air pollutant levels due to COVID-19 lockdown measures based on a business-as-usual prediction scenario using data mining models: A case-study for urban traffic sites in Spain"_, submitted to _Environmental Software & Modelling_ by J. González-Pardo et al. (2021). For the sake of reproducibility, it includes Jupyter notebooks with worked examples which allow to reproduce the results shown in that paper.

Contact: jaime.diez.gp@gmail.com

## pyAEMET

During the course of this research the [pyAEMET](https://github.com/Jaimedgp/pyAEMET) python library
has been developed in order to download daily meteorological observations from the Spanish Met Service ([AEMET](http://www.aemet.es/es/portada)) via its OpenData API REST and it is needed to perform the data curation process.

```bash
pip install pyaemet=1.0.1
```

## Data

All the data used for this study are publicly available at this persistent [repository](https://doi.org/10.5281/zenodo.5642868). To ease reproducibility, we recommend the interested user to download and include them in the same directory of this repository.

```
AirQualityCOVID
    |
    +---data/
    |
    +---src/
    |
    +---notebook/
```

## Funding

This research was developed in the framework of the project “Contaminación atmosférica y
COVID-19: ¿Qué podemos aprender de esta pandemia?”, funded through the Extraordinary BBVA
Foundation grant call for SARS-CoV-2 and COVID-19 research proposals within the area of
ecology and veterinary science.
