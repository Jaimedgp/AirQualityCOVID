##
# Script with R function to obtain how many data of air quality and meteo
# sites are available. This script is equivalent of notebook "notebooks/mainDB_ info.ipynb"
#
# @author Jaimedgp
##

suppressMessages(library(saqgetr))
suppressMessages(library(lubridate))
suppressMessages(library(worldmet))

get.countData.AQ <- function(sitesAQ.fl = "../data/csv/sitesAQ.csv",
                             dataAQ.fl = "../data/csv/dataAQ.csv",

                             pollutants = c("no", "no2", "o3", "pm10"), # contaminantes
                             start_dt = ymd_hms("2015-01-01 00:00:00"), # fecha inicio
                             end_dt = ymd_hms("2020-12-31 00:00:00"), # fecha final
                             lckdwn_strt = ymd_hms("2020-03-14 00:00:00"), # fecha confinamiento

                             save.data = TRUE
                            ){

    if (file.exists(sitesAQ.fl)) {
        sites <- read.csv(sitesAQ.fl, stringsAsFactors=T)

        if (file.exists(dataAQ.fl)) {
            data <- read.csv(dataAQ.fl, stringsAsFactors=T)
        } else {
            data <- get_saq_observations(site = levels(sites$site),
                                         variable = pollutants,
                                         valid_only = TRUE,
                                         start = start_dt,
                                         end = end_dt,
                                         verbose = TRUE
                                        )
            # Convert charactes into factors
            data <- mutate_if(data, is.character, as.factor)

            # Save data in a file for each station
            if (save.data) {
                print("Saving Data...")
                write.csv(data, dataAQ.fl, row.names=FALSE)

                split.by.site(data, site.lv="all",
                              folder="../data/csv/dataAQ/")
            }
        }

        numCount <- c()

        for (st in levels(sites$site)) {
            numCount <- c(numCount, nrow(data[data$site == st, ]))
        }

        nn.sitesAQ <- data.frame(siteAQ = levels(sites$site),
                                 countAQ = numCount)
        rm(data, numCount)
        # Convert charactes into factors
        nn.sitesAQ <- mutate_if(nn.sitesAQ, is.character, as.factor)
    } else {
        print("Something went wrong")
        nn.sitesAQ <- 0
    }

    nn.sitesAQ
}


get.countData.Mto <- function(sitesMto.fl = "../data/csv/sitesMto.csv",
                              dataMto.fl = "../data/csv/dataMto.csv",
                              years = 2010:2020,

                              save.data = TRUE
                             ){

    if (file.exists(sitesMto.fl)) {
        sites <- read.csv(sitesMto.fl, stringsAsFactors=T)

        if (file.exists(dataMto.fl)) {
            data <- read.csv(dataMto.fl, stringsAsFactors=T)
        } else {
            # Get Data from worlmet
            data <- importNOAA(code = levels(sites$code),
                               year = years,
                               hourly = FALSE,
                               n.cores = 4,
                               quiet = FALSE,
                               path = NA
                              )
            # Convert charactes into factors
            data <- mutate_if(data, is.character, as.factor)

            # Save data in a file for each station
            if (save.data) {
                print("Saving Data...")
                write.csv(data, dataMto.fl, row.names=FALSE)

               split.by.site(data, site.lv="all",
                              folder="../data/csv/dataMto/")
            }
        }

        #-----------------------
        # GET VALID DATA
        #-----------------------
        countMto <- NA
        siteMto <- NA
        code.lv <- levels(data$code)

        for (i in 1:length(code.lv)) {
            siteMto <- rbind(siteMto, code.lv[i])

            countMto <- rbind(countMto, colSums(!is.na(
                                                data[data$code == code.lv[i],
                                                !colnames(data) %in% c("date",
                                                                       "station",
                                                                       "latitude",
                                                                       "longitude",
                                                                       "elev"
                                                                       )])
                                               )
                                        / nrow(data[data$code ==code.lv[i], ])
                            )
            countMto[i+1, "code"] <- nrow(data[data$code == code.lv[i], ])
        }

        countMto <- data.frame(countMto[complete.cases(countMto), ]) %>%
            rename(countMto = code)

        siteMto <- data.frame(siteMto[complete.cases(siteMto), ])
        colnames(siteMto) <- c("siteMto")

        countMto <- cbind(siteMto, countMto)

        #final.sites.mto
        nn.sitesMto <- merge(x = sites %>%
                                    rename(siteMto=code) %>%
                                    select(siteMto, dist, siteAQ),
                                 y = countMto,
                                 by = "siteMto", all=TRUE
                                )
        rm(data, countMto, sites, siteMto)
        # Convert charactes into factors
        nn.sitesMto <- mutate_if(nn.sitesMto, is.character, as.factor)
    } else {
        print("Something went wrong")
        nn.sitesMto <- 0
    }

    nn.sitesMto
}


get.nnSites <- function(sitesMto, sitesAQ,
                        final.fl = "../data/csv/nn_sites.csv"
                       ){
    nn.sites <- merge(x = sitesAQ,
                      y = sitesMto,
                      by = "siteAQ", all = TRUE)
    # Convert charactes into factors
    nn.sites <- mutate_if(nn.sites, is.character, as.factor)

    write.csv(nn.sites, final.fl, row.names=FALSE)
}
