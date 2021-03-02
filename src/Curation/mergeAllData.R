##############################################################################
#               Merge All data Curated
#
# Merge air quality, AEMET climate and ERA5-Land data for each station.
#
# @author Jaimedgp
##############################################################################

# Load packages
suppressMessages(library(tidyverse))
suppressMessages(library(lubridate))


# Working directory
setwd("~/Repositories/AirQualityCOVID")

# Source some scripts
source("src/Curation/functions.R")
source("src/functions.R")

# MAIN VARIABLES

start_dt <- ymd_hms("2013-01-01 00:00:00")
end_dt <- ymd_hms("2020-12-31 00:00:00")


#-----------------------------
#    Air Quality Stations
#-----------------------------

sitesAQ <- read.csv("data/Curation/AirQuality/checked_sitesAQ.csv",
                    stringsAsFactor=F)

sitesAQ <- sitesAQ[-which(sitesAQ$Municipio == "Las Palmas de Gran Canaria"), ]


# Sites codes
sites.lv <- levels(as.factor(sitesAQ$site))

# Folder wherever take downloaded data, if it exists
AQ.files <- "data/Curation/AirQuality/Values/"

#-----------------------------
#       AEMET Stations
#-----------------------------

sitesMto <- read.csv("data/Curation/AEMET/checked_sites_AEMET.csv",
                     stringsAsFactor=F)

# Folder wherever take downloaded data, if it exists
Mto.files <- "data/Curation/AEMET/Values/"


#-----------------------------
#    ERA5-Land Data
#-----------------------------

folder.ERA5.Land <- "data/Curation/ERA5-Land/Values/"

# Relative Humidity
rh.ERA5.Land <- read.csv(paste(folder.ERA5.Land,
                               "rh_daily_2010_2020_final_stations.csv",
                               sep=""), stringsAsFactor=F) %>%
                    data.as.datetime("dates", "ymd") %>%
                    add_column(variable="RH", .after="dates")

# Solar radiation
ssrd.ERA5.Land <- read.csv(paste(folder.ERA5.Land,
                                 "ssrd_daily_2010_2020_final_stations.csv",
                                 sep=""),stringsAsFactor=F) %>%
                    data.as.datetime("dates", "ymd") %>%
                    add_column(variable="solar.radiation", .after="dates")

ERA5.Land <- rbind(rh.ERA5.Land, ssrd.ERA5.Land)


##############################
#    Merge Process
##############################

list.data <- list()

for (i in 1:length(sites.lv)) {
    st <- sites.lv[i]
    print(paste(i, length(sites.lv), sep="/"))

    #-----------------------------
    #    Air Quality Data
    #-----------------------------

    dataAQ <- get.AQdata(st,
                         sitesAQ[sitesAQ$site == st, "Pollutant"],
                         start_dt=start_dt, end_dt=end_dt,
                         data.by.file=TRUE, fileName=AQ.files) %>%
                    group.by.date(formulation = value ~ date + site + variable,
                                  dateCl="date", unit="day", FUN="mean") %>%
                    data.as.datetime("date", "ymd")%>%
                    pivot.long.table(valueCl = "value", variableCl="variable") %>%
                    select(-site)

    #----------------------------------
    #       Meteorological Data
    #----------------------------------

    indicativo <- sitesMto[sitesMto$siteAQ == st, "indicativo"][1]

    dataMto <- read.csv(paste(Mto.files,
                              indicativo, ".csv",
                              sep=""), stringsAsFactor=F) %>%
                    data.as.datetime("fecha", "ymd") %>%
                    select("fecha", "tmed", "prec", "tmin", "tmax", "dir",
                           "velmedia", "racha", "presMax", "presMin"
                           )

    for (vr in levels(as.factor(ERA5.Land$variable))) {
        merge.data <- merge(x = dataMto,
                            y = ERA5.Land[ERA5.Land$variable == vr,
                                          c("dates", st)],
                            by.x = "fecha", by.y = "dates", all.x = TRUE
                           )
        names(merge.data)[ncol(merge.data)] <- vr
    }

    list.data[[st]] <- list(dataAQ=dataAQ,
                            dataMto=dataMto,
                            indicativo=indicativo
                           )
}


save(list.data, sites.lv, file="data/Curation/values.rda")
