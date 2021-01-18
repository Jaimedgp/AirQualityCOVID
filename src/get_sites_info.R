##
# Script with R function to obtain the information of air quality and meteo
# sites. This script is equivalent of notebooks "notebooks/sitesAQ.ipynb"
# and "notebooks/sitesMto.ipynb"
#
# @author Jaimedgp
##

# libraries for AQ
suppressMessages(library(saqgetr))
suppressMessages(library(tidyverse))
suppressMessages(library(lubridate))

# libraries for Mto
suppressMessages(library(worldmet))
source("../src/open_data.R")


get.sitesAQ.info <- function(file="../data/xlsx/estaciones-CA-JA.xlsx",
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
                  ) %>%
            select(site, site_name, latitude, longitude, elevation,
                   country, site_type, site_area, date_start, date_end)

        sites.info <- get_saq_processes() %>%
            filter(site %in% spain.sites$site,
                   variable %in% pollutants,
                   date_start <= start_dt,
                   #date_end >= end_dt,
                  ) %>%
            select(process, site, variable, variable_long,
                   period, unit, observation_count)

        sites.geo <- sites.100mil %>%
            select("Municipio", "Población",
                   "Estación tráfico", "Código estación") %>%
            rename(site = "Código estación",)

        # Merge all dataframes with the main information of each air quality stations
        sites <- merge(x = sites.info, y = sites.geo, by = "site", all.x = TRUE)
        sites <- merge(x = sites, y = spain.sites, by = "site", all.x = TRUE)

    } else {
        print("No spain sites file")
    }
    write.csv(sites, "../data/csv/sitesAQ.csv", row.names=FALSE)
    sites
}

}


get.sitesMto.info <- function(nearest=3){
    # Function to obtain the information about the three closest meteo sites
    # from air quality sites

    sitesAQ <- open.sites()

    sites.mto <- data.frame() # Initialize sites.mto dataframe
    sites.lv <- levels(as.factor(sitesAQ$site)) # get sitesAQ codes

    for (i in 1:length(sites.lv)) {
        mto <- getMeta(lat = sites[sites$site == sites.lv[i], ]$latitude[1],
                       lon = sites[sites$site == sites.lv[i], ]$longitude[1],
                       end.year = "current",
                       n = 3, returnMap = F)
        mto$site <- sites.lv[i]
        sites.mto <- rbind(sites.mto, mto)
    }

    write.csv(sites.mto, "../data/csv/sitesMto.csv", row.names=FALSE)
    sites.mto
}
