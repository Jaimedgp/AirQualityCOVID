##############################################################################
#                     Estaciones Meteorologicas
#
# Obtener los codigos y los metadatos de las 3 estaciones mas cercanas a cada
# estacion de calidad del aire y almacenarlas en un archivo. Solo se cogen las
# estaciones cuya fecha de inicio sea anterior al 2015
#               estacion$begin < 2015-01-01
#
# @author Jaimedgp
##############################################################################

# Loading
suppressMessages(library(worldmet))
suppressMessages(library(tidyverse))
suppressMessages(library(lubridate))

setwd("~/Repositories/AirQualityCOVID/")

sitesAQ <- read.csv("data/curation/sitesAQ.csv",
                    stringsAsFactor=TRUE)

sitesMto <- data.frame()
sites.lv <- levels(sitesAQ$site)

for (i in 1:length(sites.lv)) {
    mto <- getMeta(lat = sitesAQ[sitesAQ$site == sites.lv[i], ]$latitude,
                   lon = sitesAQ[sitesAQ$site == sites.lv[i], ]$longitude,
                   end.year = "current",
                   n = 3, returnMap = F)
    mto$siteAQ <- sites.lv[i]
    sitesMto <- rbind(sitesMto, mto[mto$begin < "2015-01-01", ])

    print(paste(i, length(sites.lv), sep="/"))
}

# Guardar Datos en csv
write.csv(sitesMto,
          "data/curation/sitesMto.csv", row.names=FALSE)
