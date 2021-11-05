# Estimating changes in air pollutant levels due to COVID-19 lockdown measures

This repository contains all the script developed to the study of the changes in air pollutant
levels due to COVID-19 lockdown measures. The repository includes jupyter notebooks with examples
and results of the development of the study.

Anyone interested in the analysis are encouraged to contact Jaime Diez: jaime.diez.gp@gmail.com

## pyAEMET

During the course of this research the [pyaemet](https://github.com/Jaimedgp/pyAEMET) python library
has been developed in order to download AEMET clima data via its OpenData API REST and it is needed
to execute the AEMET curation process

```bash
pip install -i https://test.pypi.org/simple/ pyAEMET-Jaimedgp
```

## DataSet

The data used in the study are publicly accessible in a persistent data repository in
[10.5281/zenodo.5642868](https://doi.org/10.5281/zenodo.5642868) and we encourage to
download and include in the same directory of this repository for reproduction.

```
AirQualityCOVID
    |
    +---data/
    |
    +---src/
    |
    +---notebook/
```

## Founding

This research was developed in the framework of the project “Contaminación atmosférica y
COVID-19: ¿Qué podemos aprender de esta pandemia?”, selected in the Extraordinary BBVA
Foundation grant call for SARS-CoV-2 and COVID-19 research proposals, within the area of
ecology and veterinary science.
