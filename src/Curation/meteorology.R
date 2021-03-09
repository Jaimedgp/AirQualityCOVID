##############################################################################
#                                Merge All Data
#
# Merge air quality, AEMET climate and ERA5-Land data for each station.
#
# @author Jaimedgp
###############################################################################

# Load packages
suppressMessages(library(tidyverse))
suppressMessages(library(lubridate))


if(sys.nframe() == 0) {

    print("Executing main...")
    setwd("~/Repositories/AirQualityCOVID")

    # Source some scripts
    source("src/functions.R")


    #--------------------------
    #      Main Variables
    #--------------------------

    start_dt <- ymd_hms("2013-01-01 00:00:00")
    end_dt <- ymd_hms("2020-12-31 00:00:00")


    #-----------------------------
    #    Air Quality Stations
    #-----------------------------

    sites.AQ <- read.csv("data/Curation/checked_AQ.csv",
                         stringsAsFactor=T)

    #-----------------------------
    #       AEMET Stations
    #-----------------------------

    sites.AEMET <- read.csv("data/Curation/checked_AEMET.csv",
                            stringsAsFactor=F, colClasses=c("indicativo"="character"))

    #-----------------------------
    #       WorldMet Stations
    #-----------------------------

    sites.WorldMet <- read.csv("data/Curation/checked_WorldMet.csv",
                               stringsAsFactor=T)


    #--------------------------------------------------------
    #            n-Table
    #
    # Table with all the relations between Air Quality
    #     stations with AEMET and WorldMet stations code
    #--------------------------------------------------------

    nn.stations <- data.frame()

    for (st in levels(sites.AQ$site)) {
        nn.stations <- rbind(nn.stations,
                            data.frame(siteAQ = as.factor(st),
                                        AEMET = as.factor(sites.AEMET[sites.AEMET$siteAQ == st,
                                                                    "indicativo"]),
                                        WorldMet = sites.WorldMet[sites.WorldMet$siteAQ == st,
                                                                "code"]
                                    )
                            )
    }


    #-----------------------------
    #    ERA5-Land Data
    #-----------------------------

    folder.ERA5.Land <- "data/Curation/ERA5-Land/"

    # Relative Humidity
    load(paste(folder.ERA5.Land,
            "rh_daily_2010_2020_final_stations.rda", sep=""))

    rh.ERA5.Land <- data.as.datetime(df, "dates", "ymd") %>%
                        add_column(variable="RH", .after="dates")

    # Solar radiation
    load(paste(folder.ERA5.Land,
            "ssrd_daily_2010_2020_final_stations.rda", sep=""))

    ssrd.ERA5.Land <- data.as.datetime(df, "dates", "ymd") %>%
                        add_column(variable="solar.radiation", .after="dates")

    ERA5.Land <- rbind(rh.ERA5.Land, ssrd.ERA5.Land)


    #-----------------------------
    #    Merge Data
    #-----------------------------

    # Folder wherever take downloaded data, if it exists
    Mto.files <- "data/Curation/"

    data_Mto <- data.frame()

    for (st in levels(nn.stations$siteAQ)) {

        if (st %in% names(ERA5.Land)) {
            code <- nn.stations[nn.stations$siteAQ == st, "WorldMet"]

            data.WorldMet <- read.csv(paste(Mto.files, "WorldMet/",
                                        code, ".csv", sep=""), stringsAsFactor=F) %>%
                            data.as.datetime("date", "ymd") %>%
                            select(-"code")

            indicativo <- nn.stations[nn.stations$siteAQ == st, "AEMET"]

            data.AEMET <- read.csv(paste(Mto.files, "AEMET/",
                                        indicativo, ".csv", sep=""), stringsAsFactor=F) %>%
                            data.as.datetime("fecha", "ymd") %>%
                            select("fecha", "tmed", "prec",
                                "tmin", "tmax", "presMax", "presMin"
                                )

            data.row <- merge(x = data.WorldMet, y = data.AEMET,
                                by.x = "date", by.y = "fecha", all = TRUE)

            for (vr in levels(as.factor(ERA5.Land$variable))) {
                data.row <- merge(x = data.row,
                                y = ERA5.Land[ERA5.Land$variable == vr,
                                                c("dates", st)],
                                by.x = "date", by.y = "dates", all.x = TRUE
                                )
                names(data.row)[ncol(data.row)] <- vr
            }

            data.row[, "site"] <- st
            data_Mto <- rbind(data_Mto, data.row)
        }
    }


    #------------------------------
    #      Save Curated data
    #------------------------------

    save(data_Mto, nn.stations, file="data/meteorology.rda")
}
