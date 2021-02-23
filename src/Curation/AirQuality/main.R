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


#########################################
# Variables Principales del Estudio
#########################################

start_dt <- ymd_hms("2013-01-01 00:00:00")
end_dt <- ymd_hms("2020-12-30 00:00:00")

pollutants <- c("no", "no2", "o3", "pm10", "pm2.5")


##############################
# Ejecucion del Programa
##############################

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
write.csv(all.info,
          "data/Curation/AirQuality/info_sitesAQ.csv", row.names=FALSE)
all.info  <- mutate_if(all.info, is.factor, as.character)

#--------------------------------------
# Filtrar con los datos validos
#--------------------------------------

valid.info <- all.info[all.info$hv.min == TRUE &
                       !is.na(all.info$hv.min)
                      ,]

valid.info <- valid.info[valid.info$mss.yr < 5,]

write.csv(sitesAQ[sitesAQ$site %in% levels(as.factor(valid.info$site)),],
          "data/Curation/AirQuality/checked_sitesAQ.csv", row.names=FALSE)
