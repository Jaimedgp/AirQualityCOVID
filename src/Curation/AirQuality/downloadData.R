# Working directory
setwd("~/Repositories/AirQualityCOVID")

# Source some scripts
source("src/Curation/general.R")
source("src/Curation/AirQuality/sitesAQ.R")

#----------------------------------------
# Variables Principales del Estudio
#----------------------------------------

start_dt <- ymd_hms("2013-01-01 00:00:00")
end_dt <- ymd_hms("2020-12-30 00:00:00")

pollutants <- c("no", "no2", "o3", "pm10", "pm2.5")

#-------------------
# descargar datos
#-------------------

# Obtencion de las estaciones de estudio
sitesAQ <- get.spain.sites(start_dt)
sites.lv <- levels(as.factor(sitesAQ$site))

for (st in sites.lv) {

    data.st <- get.AQdata(st, pollutants,
                          start_dt=start_dt, end_dt=NA,
                          data.by.file=FALSE, fileName="")
    write.csv(data.st,
              paste("data/Curation/AirQuality/Values/",
                    st, ".csv", sep=""), row.names=FALSE)
}

