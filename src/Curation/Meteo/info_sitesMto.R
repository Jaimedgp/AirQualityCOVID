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
    new.df$date <- as.numeric(new.df$date)
    new.df <- new.df[complete.cases(new.df),]


    resolutions <- apply(new.df[-2], 2, diff) / 3600

    resol <- min(resolutions)
    amount <- resol * sum(!is.na(dataFrame[, column])) / nrow(dataFrame[, column])

    c(resol, amount)
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
    i <- 1

    for (st in sites.lv) {
        # Get Data from worlmet
        dataMto <- importNOAA(code = st,
                            year = years,
                            hourly = TRUE,
                            n.cores = 9
                            )

        new.row <- data.frame(site = st,
                              start_dt = as_date(min(dataMto$date)),
                              end_dt = as_date(max(dataMto$date))
                              )

        nm <- names(dataMto)[-(1:6)]
        if ("pwc" %in% nm) {
            nm <- nm[-which(nm == "pwc")]
        }

        info.df <- data.frame(lapply(nm, get.info, dataFrame=dataMto))
        names(info.df) <- nm

        resolution.row <- cbind(new.row, info.df[1,])
        amount.row <- cbind(new.row, info.df[2,])

        for (cl in variables[-which(variables %in% nm)]) {
            resolution.row[, cl] <- 0
            amount.row[, cl] <- 0
        }

        all.resolution <- rbind(resolution.row, all.resolution)
        all.amount <- rbind(amount.row, all.amount)

        print(paste(i, length(sites.lv), sep="/"))
        i <- i+1
    }

    list(all.resolution, all.amount)
}


if(!interactive()) {

    years <- 2013:2020

    # Get sites of study
    all.sites <- read.csv("data/curation/sitesMto.csv",
                          stringsAsFactor=TRUE)
    sites.lv <- levels(all.sites$code)

    all.info <- main(sites.lv, years)

    write.csv(all.info[1],
              "data/curation/info_sitesMto_resolution.csv", row.names=FALSE)
    write.csv(all.info[2],
              "data/curation/info_sitesMto_amount.csv", row.names=FALSE)
}
