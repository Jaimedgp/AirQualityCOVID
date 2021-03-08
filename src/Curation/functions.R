########################################################################
#               Data Processing Functions
#
# General functions used to load and transform data for curation.
#     - get.AQdata: Load air quality data
#     - group.by.date: Change Date resolution
#
# @author Jaimedgp
########################################################################

# load packages
suppressMessages(library(saqgetr))
suppressMessages(library(lubridate))

source("src/functions.R")


sv.checkedAQ <- function(fileName="data/Curation/checked_AQ.csv") {
    if (file.exists(fileName)) {
        sites.data <- read.csv(fileName, stringsAsFactor=F)
        data_AQ <- data.frame()

        for (st in levels(as.factor(sites.data$site))) {
            pll <- levels(as.factor(sites.data[sites.data$site == st,
                                    "Pollutant"]))
            data_AQ <- rbind(data_AQ,
                              get.AQdata(st, pollutant=pll, start_dt=ymd("2020-01-01"))
                              )
        }
        save(data_AQ, file="data/data_AQ.rda")
    } else {
        data_AQ <- NULL
    }

    data_AQ
}


get.AQdata <- function(site, pollutant,
                       start_dt, end_dt=NA, data.by.file=FALSE,
                       fileName="data/Curation/AirQuality/Values/") {
    # Obtain annual air quiality data. Data can be obtained either from
    #     file with previous downloaded data or from saqgetr function
    #
    # @params:
    #     site: site code for air quality station. e.g.: "es0000a"
    #     pollutant: pollutants from which to download
    #                concentrations. e.g.: c("no2", "o3")
    #     start_dt: Start date for returned observations. if start_dt
    #               is a date, its year is taken as start date.
    #     end_dt: End date for returned observations. if end_dt
    #             is a date, its year is taken as end date.
    #     data.by.file: Boolean with the condition if the data
    #                   is taken from the file.
    #     fileName: Path of the downloaded data files from where the
    #               data is loaded if data.by.file is true
    # @return:
    #     data.AQ: dataframe with air quality data

    fileName <- paste(fileName, site, ".csv", sep="")

    if (data.by.file & sum(file.exists(fileName)) == length(site)) {
        data.AQ <- data.frame()

        if (is.na(end_dt)) {
            end_dt <- Sys.Date()
        }

        for (fl in fileName) {
            data.AQ <- rbind(data.AQ,
                             read.csv(fl, stringsAsFactor=FALSE) %>%
                                filter(variable %in% pollutant) %>%
                                data.as.datetime("date", "ymd_hms") %>%
                                filter(year(date) >= year(start_dt) &
                                       year(date) <= year(end_dt))
                            )
        }
    } else {
        print("Downloading...")

        suppressMessages(data.AQ <- get_saq_observations(
            site = site,
            variable = pollutant,
            start = start_dt,
            end = end_dt,
            valid_only = TRUE,
            verbose = TRUE
        ))
    }
    data.AQ
}
