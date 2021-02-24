##############################################################################
#               Juntar Datos de la CA y de Meteo
#
# Script completo utilizado para juntar los datasets obtenidos para la
#     calidad del aire y la para los datos Meteorologicos de la AEMET. Puesto
#     que los datos de la AEMET tienen resolucion diaria, se ha de agrupar
#     los datos de CA a resolucion diaria y pivotar la tabla para que cada
#     contamiante este en una columna.
#
# @author Jaimedgp
##############################################################################

# Packages
suppressMessages(library(tidyverse))
suppressMessages(library(lubridate))

# Working directory
setwd("~/Repositories/AirQualityCOVID/")

# Source some scripts
source("src/Curation/general.R")

#########################################
# Variables Principales del Estudio
#########################################

start_dt <- ymd_hms("2013-01-01 00:00:00")
end_dt <- ymd_hms("2020-12-31 00:00:00")


#########################################
# Estaciones ya filtradas
#########################################

sitesAQ <- read.csv("data/Curation/AirQuality/checked_sitesAQ.csv",
                    stringsAsFactor=F)

sitesMto <- read.csv("data/Curation/AEMET/checked_sites_AEMET.csv",
                     stringsAsFactor=F)


##############################
# Ejecucion del Programa
##############################

site.lv <- levels(as.factor(sitesAQ$site))
for (st in site.lv) {
    pll <- sitesAQ[sitesAQ$site == st, "Pollutant"]
    dataAQ <- get.AQdata(st, pll, start_dt=start_dt, end_dt=end_dt,
                         data.by.file=TRUE,
                         fileName="data/Curation/AirQuality/Values/")

    dataAQ <- group.by.date(dataFrame=dataAQ,
                            valueList = list(value=dataAQ$value),
                            byList = list(date=dataAQ$date,
                                          site=dataAQ$site,
                                          variable=dataAQ$variable),
                            unit="day", FUN="mean"
                           ) %>% data.as.datetime("date", "ymd") %>%
                           pivot.long.table(valueCl = "value",
                                            variableCl="variable")

    nameMto <- paste("data/Curation/AEMET/Values/",
                     sitesMto[sitesMto$siteAQ == st, "indicativo"][1],
                     ".csv", sep="")

    dataMto <- read.csv(nameMto, stringsAsFactor=F) %>%
                data.as.datetime("fecha", "ymd") %>%
                select("fecha", "indicativo",
                       "tmed", "prec", "tmin", "tmax", "dir",
                       "velmedia", "racha", "presMax", "presMin"
                      )

    data.st <- merge(dataAQ, dataMto,
                     by.x="date", by.y="fecha", all.x = T)

    comun.nm <- c("date", "site", "indicativo")
    variable.nm <- names(data.st)[-which(names(data.st) %in% comun.nm)]

    data.st <- cbind(data.st[, comun.nm ], data.st[, variable.nm])

    write.csv(data.st,
              paste("data/Curation/Values/",
                    st, ".csv", sep=""),
              row.names=F)
}
