##############################################################################
#               Estaciones Calidad del Aire
#
# Seleccionamos aquellas estaciones que se encuentren en un núcleo con una
#     población mayor o igual a cien mil habitantes poblacion >= 100 000 hab.
#     En la hoja 8 ciudades-100000-A-JA de la base de datos de poblaciones
#     (estaciones-CA.xlsx) aparecen todas las estaciones de trafico de las
#     ciudades de mas de 100000 habitantes con sus nombres.
# Importamos la informacion de las estaciones de calidad de aire de españa
#     obtenidas de la base de datos y filtramos segun los criterios de
#     estudio.
#
#                   |  Criterio   |      Valores      |
#                   |-------------|-------------------|
#                   |Contaminantes| NO, NO2, O3, PM10 |
#                   |Fecha Inicio |   01 Enero 2015   |
#                   | Fecha Final | 31 Diciembre 2020 |
#                   |  Site Type  |       traffic     |
#
#
# @author Jaimedgp
##############################################################################

# Loading
suppressMessages(library(saqgetr))
suppressMessages(library(tidyverse))
suppressMessages(library(lubridate))
suppressMessages(library(openxlsx))

get.spain.sites <- function(start_dt,
                            to.save="data/Curation/AirQuality/sitesAQ.csv",
                            file.es="data/xlsx/estaciones-CA-JA.xlsx",
                            sheets="ciudades-100000-A") {

    # xlsx files
    sites.100mil <- read.xlsx(file.es, sheet=sheets) %>%
        select("Municipio", "Población",
            "Estación.tráfico", "Código.estación")

    #----------------------------------------------------
    # Obtener datos de CA de España. Salen los códigos
    # de las estaciones de Calidad de aire (941)
    #----------------------------------------------------
    spain.sites <- get_saq_sites() %>%
        filter(country == "spain",
            site %in% sites.100mil$"Código.estación",
            site_type == "traffic",
            site_area == "urban",
            date_start <= start_dt,
            ) %>%
        select(site, site_name, latitude, longitude, elevation,
            country, site_type, site_area, date_start, date_end)

    sites.AQ <- merge(x = spain.sites,
                      y = sites.100mil,
                      by.x = "site", by.y="Código.estación",
                      all.x = TRUE)

    write.csv(sites.AQ, to.save, row.names=FALSE)

    sites.AQ
}


if(interactive()) {

    setwd("~/Repositories/AirQualityCOVID")

    # fechas de inicio y final de toma de datos
    start_dt <- ymd_hms("2013-01-01 00:00:00")

    get.spain.sites(start_dt)
    print("HOLA")
}
