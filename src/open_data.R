library(readxl)
library(tidyverse)
library(saqgetr)
library(lubridate)

open.sites <- function(file="../data/csv/estaciones.csv"){
    if (file.exists(file)) {
            sites <- read.csv(file)
    } else {
        sites <- extraxct.sites()
    }
    sites
}


open.data <- function(file="../data/csv/dataAQV.csv", numSites=5) {

    sites <- levels(as.factor(open.sites()$site))

    sites <- sites[1:numSites]

    if (file.exists(file)) {
        dataAQV <- read.csv(file)

        # Convert date to datetime format
        dataAQV$date <- ymd_hms(dataAQV$date)

        dataAQV <- dataAQV[dataAQV$site %in% sites, ]
    } else {
        dataAQV <- extract.data(sites=sites)
    }
    dataAQV
}


extract.sites <- function(
                          file="../data/xlsx/estaciones-CA-JA.xlsx",
                          sheet="ciudades-100000-A",
                          start_dt = ymd_hms("2010-01-01 00:00:00"),
                          end_dt = ymd_hms("2020-10-01 00:00:00"),
                          site_type = "traffic",
                          pollutants = c("no", "no2", "o3", "pm10")
                         ) {

    if (file.exists(file)) {
        sites.100mil <- read_excel(file, sheet=sheet)

        # obtener datos de CA de España. Salen los códigos de las estaciones
        # de Calidad de aire (941)
        spain.sites <- get_saq_sites() %>%
            filter(country == "spain",
                site %in% sites.100mil$"Código estación",
                site_type == "traffic",
                site_area == "urban",
                date_start <= start_dt,
                date_end >= end_dt,
                )
        sites.info <- get_saq_processes() %>%
            filter(site %in% spain.sites$site,
                variable %in% pollutants,
                date_start <= start_dt,
                #date_end >= end_dt,
                ) %>%
            select(site, variable, period, unit, date_start, date_end)

        sites.geo <- sites.100mil %>%
            select("Municipio", "Población",
                "Estación tráfico", "Código estación") %>%
            rename(site = "Código estación",)

        sites <- merge(x = sites.info, y = sites.geo, by = "site", all.x = TRUE)

    } else {
        print("No spain sites file")
    }
    sites
}


extract.data <- function(
                         sites,
                         pollutants = c("no", "no2", "o3", "pm10"),
                         start_dt = ymd_hms("2010-01-01 00:00:00"),
                         end_dt = ymd_hms("2020-10-01 00:00:00")
                        ){

    dataAQV <- get_saq_observations(
        site = sites,
        variable = pollutants,
        valid_only = TRUE,
        start = start_dt,
        end = end_dt,
        verbose = TRUE
    )
}
