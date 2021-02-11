##############################################################################
#               Curation Estaciones Meteorologicas
#
# Estudiar la cantidad de datos validos disponibles para cada variable en cada
# estacion. Para ello tambien es necesario obtener la resolucion minima de cada
# variable ya que el porcentaje de datos validos sera a partir de dicha
# resolucion. Los datos se descargan con resolucion horaria, siendo esta la
# referencia.
#
# @author Jaimedgp
##############################################################################

suppressMessages(library(worldmet))
suppressMessages(library(lubridate))
suppressMessages(library(tidyverse))

setwd("~/Repositories/AirQualityCOVID")
source("src/general.R")


get.info <- function(column, dataFrame) {
    new.df <- dataFrame[, c("date", column)]
    new.df <- new.df[complete.cases(new.df),]

    resolutions <- c()

    for (i in 1:(nrow(new.df)-1)) {
        resolutions <- c(resolutions,
                         interval(new.df$date[i],
                                  new.df$date[i+1]) /3600
                        )
    }

    if (!is.na(min(resolutions)) & min(resolutions) > 24) {
        resol <- NA
        amount <- sum(!is.na(dataFrame[, column])) / nrow(dataFrame[, column])
    } else {
        resol <- min(resolutions)
        amount <- resol * sum(!is.na(dataFrame[, column])) / nrow(dataFrame[, column])
    }

    c(resol, amount)

}


get.amount <- function(dataFrame, column, resolution) {
    resolution * sum(!is.na(dataFrame[, column])) / nrow(dataFrame[, column])
}


######################
###      MAIN       ##
######################

main <- function(sites.lv, years) {
    variables <- c('ws', 'wd', 'air_temp', 'atmos_pres', 'visibility', 'dew_point', 'RH',
                   'ceil_hgt', 'pwc', 'precip', 'cl_1', 'cl_2', 'cl_3', 'cl', 'cl_1_height',
                   'cl_2_height', 'cl_3_height', 'precip_12', 'precip_6')

    all.resolution <- data.frame()
    all.amount <- data.frame()

    for (st in sites.lv) {
        # Get Data from worlmet
        dataMto <- importNOAA(code = st,
                            year = years,
                            hourly = TRUE,
                            n.cores = 6
                            )

        new.row <- data.frame(site = st,
                            start_dt = as_date(min(dataMto$date)),
                            end_dt = as_date(max(dataMto$date))
                            )

        info.apply <- lapply(names(dataMto)[-(1:7)], get.info, dataFrame=dataMto)
        info.df <- data.frame(info.apply)
        names(info.df) <- names(dataMto)[-(1:7)]

        resolution.row <- cbind(new.row, info.df[1,])
        amount.row <- cbind(new.row, info.df[2,])

        for (cl in variables[-which(variables %in% names(dataMto))]) {
            resolution.row[, cl] <- 0
            amount.row[, cl] <- 0
        }

        all.resolution <- rbind(resolution.row, all.resolution)
        all.amount <- rbind(amount.row, all.amount)
    }

    list(resolution=all.resolution,
         amount=all.amount)
}


if(!interactive()) {

    years <- 2013:2020

    # Get sites of study
    all.sites <- read.csv("data/curation/sitesMto.csv",
                          stringsAsFactor=TRUE)
    sites.lv <- levels(all.sites$site)[1:2]

    all.info <- main(sites.lv, years)

    write.csv(all.info["resolution"],
              "data/curation/info_sitesMto_resolution.csv", row.names=FALSE)
    write.csv(all.info["amount"],
              "data/curation/info_sitesMto_amount.csv", row.names=FALSE)
}
