##
# Script with R function to obtain how many data of air quality and meteo
# sites are available. This script is equivalent of notebooks "notebooks/countAQdata.ipynb"
# and "notebooks/countMtodata.ipynb"
#
# @author Jaimedgp
##

suppressMessages(library(saqgetr))
suppressMessages(library(lubridate))

get.countData.AQ <- function(sitesAQ.fl = "../data/csv/sitesAQ.csv"
                             dataAQ.fl = "../data/csv/dataAQ.csv",
                             final.fl = "../data/csv/final_sites.csv"){
    # contaminantes a estudiar
    pollutants <- c("no", "no2", "o3", "pm10")

    # fechas de inicio y final de toma de datos
    start_dt <- ymd_hms("2015-01-01 00:00:00")
    end_dt <- ymd_hms("2020-10-01 00:00:00")

    # fecha de inicio de confinamiento
    lckdwn_strt <- ymd_hms("2020-03-14 00:00:00")


    if (file.exists(sitesAQ.fl)) {
        sites <- read.csv(sitesAQ.fl, stringsAsFactors=T)

        if (file.exists(dataAQ.fl)) {
            data <- read.csv(dataAQ.fl)
        } else {
            suppressMessages(data <- get_saq_observations(
                site = levels(sites$site),
                variable = pollutants,
                valid_only = TRUE,
                start = start_dt,
                end = end_dt,
                verbose = TRUE
            ))
        }

        numCount <- c()

        for (st in levels(sites$site)) {
            numCount <- c(numCount, nrow(dataAQV[dataAQV$site == st, ]))
        }

        final.info.sites <- data.frame(siteAQ=levels(sites$site),
                                       countAQ=numCount)

        if (file.exists(file)) {
            final.sites <- read.csv(file)

            for (st in final.info.sites$siteAQ) {
                final.sites[final.sites$siteAQ == st,
                            ]$countAQ <- final.info.sites[final.info.sites$siteAQ == st,
                                                         ]$countAQ
            }
        } else {
            write.csv(final.info.sites, file, row.names=FALSE)
        }

    } else {
        print("Something went wrong")
    }

