##############################################################################
#               Curation Estaciones Calidad del Aire
#
# Estudiar de que contaminantes no hay datos suficientes para el estudio en
#     cada estacion. Se ha de comprobar que hay datos ($> 90\%$) durante el
#     periodo de interes (`1-Enero-2020` <=> `31-Junio-2020`). Tambien se
#     comprueban si hay intervalos largos de tiempo sin datos, utilizando
#     una resolucion minima semanal, mensual y anual.
#
# |   site  | Pollutant |  start_yr  |   end_yr   | hv.min | missing.wk | missing.mnth | missing.yr |
# |---------|-----------|------------|------------|--------|------------|--------------|------------|
# | es0001a |    no2    | 01-01-2015 | 02-01-2015 |  TRUE  |     34     |      2       |     0      |
# | es0001a |    no     | 01-01-2015 | 02-01-2015 |  TRUE  |     40     |     12       |     1      |
# | es0001a |    o3     | 01-01-2015 | 02-01-2015 |  FALSE |      4     |      0       |     0      |
#
# @author Jaimedgp
##############################################################################

suppressMessages(library(lubridate))
suppressMessages(library(tidyverse))

have.2020 <- function(dataFrame, start_dt = ymd("2020-03-01"),
                                 end_dt = ymd("2020-05-01")) {

    new.df <- dataFrame[dataFrame$date >= start_dt &
                        dataFrame$date <= end_dt, ]
    period <- as.integer(interval(round_date(start_dt, unit="hour"),
                                  round_date(end_dt, unit="hour")
                                  )
                         / 3600)

    amount.data <- (sum(!is.na(new.df$value)) / period)

    amount.data >= 0.8
}


get.missing <- function(dataFrame, unit="week",
                                   start_dt=ymd("2013-01-01"),
                                   end_dt=ymd("2020-12-31")) {

    conversion <- list("hour" = 3600,
                       "day" = 3600*24,
                       "week" = 3600*24*7,
                       "month" = (3600*24*365)/12,
                       "year" = 3600*24*365
                       )

    new.df <- group.by.date(list(value=dataFrame$value),
                            list(date=dataFrame$date),
                            dataFrame, unit=unit, FUN=mean)
    period <- (interval(round_date(start_dt, unit=unit),
                        round_date(end_dt, unit=unit)
                       )
               / conversion[[unit]])


    as.integer(period) - sum(!is.na(new.df$date))
}


info_sitesAQ <- function(pollut, st, start_dt, end_dt) {
    dataAQPLL <- get.AQdata(site=st, pollutant=pollut, start_dt = start_dt,
                            end_dt=end_dt, data.by.file=TRUE,
                            fileName="data/Curation/AirQuality/Values/")

        if (nrow(dataAQPLL) > 0) {
            start_yr <- as_date(min(dataAQPLL$date))
            end_yr <- as_date(max(dataAQPLL$date))

            hv.min <- have.2020(dataAQPLL)

            mss.wk <- get.missing(dataAQPLL, unit="week",
                                  start_dt, end_dt)
            mss.mnth <- get.missing(dataAQPLL, unit="month",
                                    start_dt, end_dt)
            mss.yr <- get.missing(dataAQPLL, unit="year",
                                  start_dt, end_dt)

            new.row <- data.frame(site=st, Pollutant=pollut,
                                  start_yr, end_yr,
                                  hv.min, mss.wk, mss.mnth, mss.yr)
            new.row
        }
}



if(interactive()) {

    setwd("~/Repositories/AirQualityCOVID")

    min.interval <- c(ymd("2020-01-01"),
                      ymd("2020-06-30"))

    start_dt <- ymd("2013-01-01")
    end_dt <- ymd("2020-12-30")

    # Get sites of study
    all.sites <- read.csv("data/Curation/AirQuality/sitesAQ.csv",
                      stringsAsFactor=TRUE)
    sites.lv <- levels(all.sites$site)[1:2]
    pollutants.lv <- c("no", "no2", "o3", "pm10", "pm2.5")

    all.info <- do.call(rbind.fill,
                        lapply(sites.lv,
                               function(st, polluts){
                                   do.call(rbind.fill,
                                           lapply(polluts,
                                                  info_sitesAQ,
                                                  st, ymd("2013-01-01"),
                                                  ymd("2020-12-31"))
                                           )
                               }, pollutants.lv))

    #write.csv(all.info, "data/Curation/info_sitesAQ.csv", row.names=FALSE)
}
