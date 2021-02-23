##############################################################################
#               Estaciones Meteorologicas AEMET
#
# Script completo utilizado para obtener los datos meteorologicos de la AEMET
#     validos para el estudio
#
# @author Jaimedgp
##############################################################################

# Packages
suppressMessages(library(lubridate))
suppressMessages(library(tidyverse))
suppressMessages(library(plyr))

# Working directory
setwd("~/Repositories/AirQualityCOVID")

# Source some scripts
source("src/Curation/general.R")

get.info <- function(st) {

    dataframe <- read.csv(paste("data/Curation/AEMET/Values/",
                                st,
                                ".csv", sep="")) %>%
                    data.as.datetime("fecha", "ymd")
    period <- as.numeric(ymd("2020-12-31")-ymd("2013-01-01")) +2

    cbind(data.frame(site=st,
                     start_dt=min(dataframe$fecha),
                     end_dt=max(dataframe$fecha),
                     miss_dy=period - nrow(dataframe)
                    ),
          rbind(apply(dataframe, 2,
                      function(df, period) {
                          (sum(!is.na(df)) / period)
                      }, period))
         )
}


##############################
# Ejecucion del Programa
##############################

sites.AEMET <- read.csv("data/Curation/AEMET/sites_AEMET.csv",
                  stringsAsFactor=FALSE)

info.sites <- do.call(rbind.fill,
                    lapply(levels(as.factor(sites.AEMET$indicativo)),
                           get.info))
write.csv(info.sites,
          "data/Curation/AEMET/info_sites_AEMET.csv",
          row.names=FALSE)

#------------------------------------------------
# Quitar variables horarias y variable 'sol'
#------------------------------------------------
var.horarias <- c('sol',
                  'horatmin', 'horatmax', 'horaracha',
                  'horaPresMax', 'horaPresMin')
info.sites <- info.sites[, -which(names(info.sites) %in% var.horarias)]

#---------------------------------------------------------------------------------
# Quitar aquellas estaciones con < 90% de los datos en alfuna de las variables
#---------------------------------------------------------------------------------
info.sites[,"hv.min"] <- apply(info.sites, 1,
                               function(row) {
                                   numVar <- sum(row[5: length(row)] > 0.8,
                                                 na.rm=T)
                                   length(row[5: length(row)]) == numVar
                               })

info.sites <- info.sites[-which(info.sites$hv.min == FALSE),]

#---------------------------------------------------------------------------------
# Dejar solo la estacion AEMET mas cercana a cada estacion de caliad del aire
#---------------------------------------------------------------------------------

new.sites <- sites.AEMET[which(sites.AEMET$indicativo %in% info.sites$site),]

checked_sites <- data.frame()

for (st in levels(as.factor(new.sites$siteAQ))) {
    rows <- new.sites[new.sites$siteAQ == st,]
    checked_sites <- rbind(checked_sites,
                           rows[rows$dist == min(rows$dist),])
}

write.csv(checked_sites,
          "data/Curation/AEMET/checked_sites_AEMET.csv",
          row.names=FALSE)
