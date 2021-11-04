##############################################################################
#               WorldMet Meteorological data Curation
#
# Script with the data curation process done for worldMet meteo data. The
#     wind speed and direction are taken from worldmet package, which import
#     data from NOAA database.
#
# functions:
#     - main.curation: Main function for curation process
#
# @author Jaimedgp
##############################################################################


# Loading
suppressMessages(library(tidyverse))
#suppressMessages(library(worldmet))
#suppressMessages(library(lubridate))
#suppressMessages(library(openair))


if(sys.nframe() == 0) {

    print("Executing main...")
    setwd("~/Repositories/AirQualityCOVID")

    #--------------------------
    #      Main Variables
    #--------------------------

    min.proportion <- 0.8  # >80%
    years <- 2013:2020

    study.prd <- c(lubridate::ymd_hms("2013-01-01 00:00:00"),
                   lubridate::ymd_hms("2020-12-31 00:00:00"))

    period <- as.integer(lubridate::interval(lubridate::floor_date(study.prd[1],
                                                                   unit="day"),
                                             lubridate::floor_date(study.prd[2],
                                                                   unit="day")
                                             ) / (3600*24)) + 2


    #------------------------------
    #      sites Information
    #------------------------------

    sites.AQ <- read.csv("data/Curation/checked_AQ.csv",
                         stringsAsFactor=TRUE)

    sites.lv <- levels(sites.AQ$site)


    #------------------------------
    #      Curation Process
    #------------------------------

    sites.Mto <- data.frame()

    for (st in sites.lv) {
        print(st)
        mto <- worldmet::getMeta(lat = sites.AQ[sites.AQ$site == st, ]$latitude[1],
                       lon = sites.AQ[sites.AQ$site == st, ]$longitude[1],
                       end.year = "current",
                       n = 7, returnMap = F)

        for (cd in mto[order(mto$dist), ]$code) {
            fileName <- paste("data/Curation/NOAA-ISD/", cd, ".csv", sep="")

            if (file.exists(fileName)) {
                mto[mto$code == cd, "siteAQ"] <- st
                sites.Mto <- rbind(sites.Mto, mto[mto$code == cd, ])

                break
            } else {
                data.Mto <- worldmet::importNOAA(code = cd,
                                                 year = years,
                                                 hourly = TRUE,
                                                 n.cores = 12
                                       ) %>%
                            select("date", "code", "ws", "wd") %>%
                            openair::timeAverage(avg.time = "day",
                                                 type="code",
                                                 vector.ws=FALSE)

                if(sum(colSums(!is.na(data.Mto))/ period > min.proportion) == ncol(data.Mto)) {
                    mto[mto$code == cd, "siteAQ"] <- st
                    sites.Mto <- rbind(sites.Mto, mto[mto$code == cd, ])

                    write.csv(data.Mto, fileName, row.names=F)

                    break
                }
            }
        }
    }


    #------------------------------
    #      Save Curated data
    #------------------------------

    write.csv(sites.Mto,
              "data/Curation/checked_NOAA-ISD.csv", row.names=FALSE)
}
