##############################################################################
#               Estaciones Calidad del Aire
#
# Script completo utilizado para obtener los datos de calidad del aire
#     validos para el estudio
#
# @author Jaimedgp
##############################################################################

# Packages
suppressMessages(library(lubridate))
suppressMessages(library(plyr))

# Working directory
setwd("~/Repositories/AirQualityCOVID")

# Source some scripts
source("src/Curation/general.R")

source("src/Curation/AirQuality/sitesAQ.R")
source("src/Curation/AirQuality/info_sitesAQ.R")


#----------------------------------------------------
# Variables Principales del Estudio
#----------------------------------------------------

start_dt <- ymd_hms("2013-01-01 00:00:00")
end_dt <- ymd_hms("2020-12-30 00:00:00")

pollutants <- c("no", "no2", "o3", "pm10", "pm2.5")


#----------------------------------------------------
# Ejecucion del Programa
#----------------------------------------------------

# Obtencion de las estaciones de estudio
sitesAQ <- get.spain.sites(start_dt)


# Obtener que estaciones tienen los datos necesarios
all.info <- do.call(rbind.fill,
                    lapply(levels(as.factor(sitesAQ$site)),
                           function(st, polluts){
                               do.call(rbind.fill,
                                       lapply(polluts,
                                              info_sitesAQ,
                                              st, start_dt, end_dt))
                           }, pollutants))

# Filtrar con los datos validos
valid.info <- all.info[all.info$hv.min == TRUE &
                       !is.na(all.info$hv.min) &
                       all.info$mss.yr < 5
                      ,]

# descargar datos
for (st in levels(as.factor(valid.info$site))) {

    data.st <- do.call(rbind,
                       lapply(levels(as.factor(valid.info[valid.info$site == st,
                                                          "Pollutant"])),
                              get.AQdata,
                              site=st, start_dt=start_dt, end_dt=NA,
                              data.by.file=FALSE, fileName=""))
    write.csv(data.st,
              paste("data/Curation/AirQuality/Values/",
                    st, ".csv", sep=""), row.names=FALSE)
}
