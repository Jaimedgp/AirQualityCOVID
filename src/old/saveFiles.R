library(worldmet)
source("./open_data.R")

sites <- open.sites()

meteoCodes <- getMeta(lat=,
                      lon=,
                      end.year="current",
                      n=3, returnMap=F
                      )
write.csv(meteoCodes, "../data/csv/meteoCodes.csv", row.names = F)

meteoData <- importNOAA(code=meteoCodes$code,
                        year=2010:2020,
                        hourly=FALSE,
                        n.cores=3,
                        quiet=FALSE,
                        path="../data/meteo"
                        )