# Estimating changes in air pollutant levels due to COVID-19 lockdown measures based on a business-as-usual prediction scenario using data mining models: A case-study for urban traffic sites in Spain.

This repository contains all the script developed to the study of the changes in air pollutant
levels due to COVID-19 lockdown measures. The repository includes jupyter notebooks with examples
and results of the development of the study.

Anyone interested in the analysis are encouraged to contact Jaime Diez: jaime.diez.gp@gmail.com

## Abstract

In response to the COVID-19 pandemic, governments declared severe restrictions throughout 2020, presenting an unprecedented scenario of reduced anthropogenic emissions of air pollutants derived mainly from traffic sources. To analyse the effect of the COVID-19 pandemic on air quality levels, changes in NO, NO2, O3, PM10 and PM2.5 concentrations were estimated at urban traffic sites in the most populated Spanish cities over different periods with distinct restrictions in 2020. Three different data mining techniques plus a quantile-quantile mapping correction have been used to estimate the business-as-usual concentrations in 2020 and to account for meteorological variability. The results obtained show a decreasing pattern for NOx, with the largest reduction in the lockdown period above -50%, whereas the increase observed for O3 contrasts with the NOx patterns with a maximum increase of 23.9%. The slight reduction in PM10 (-4.1%) and PM2.5 levels (-2.3%) during lockdown indicates a lower relationship with traffic sources.

## DataSet

The data used in the study are publicly accessible in a persistent data repository in
[https://zenodo.org/deposit/5642868](https://zenodo.org/deposit/5642868) and we encourage to
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
