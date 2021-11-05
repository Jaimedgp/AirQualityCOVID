#################################################################
#               General Functions
#
# General functions use in the proyect
#     - data.as.datetime: Convert date column into datetime format
#     - get.AQdata: Load air quality data
#     - sv.checkedAQ: Download AQ data from checked_AQ file
#
# @author Jaimedgp
#################################################################

#suppressMessages(library(saqgetr))
#suppressMessages(library(lubridate))
suppressMessages(library(dplyr))


#' data.as.datetime
#'
#' Converte date column into datetime format using lubridate package
#'
#' @param dataframe: dataframe with all data
#' @param column: name or names of date column
#' @param FUN: lubridate function to apply. Can be date ('ymd') or
#'          datetime ('ymd <- hms')
#'
#' @return same dataframe with date column as datetime format
#'
#' @author Jaimedgp
data.as.datetime <- function(dataframe, column, FUN){

    if (FUN == "ymd_hms") {
        dataframe[, column] <- lubridate::ymd_hms(dataframe[, column])
    } else if (FUN == "ymd") {
        dataframe[, column] <- lubridate::ymd(dataframe[, column])
    } else {
        print("No valid FUN. Must be: 'ymd_hms' or 'ymd'")
    }
    dataframe
}


#' get.AQdata
#'
#' Obtain annual air quiality data. Data can be obtained either from
#'     file with previous downloaded data or from saqgetr function
#'
#' @param site: site code for air quality station. e.g.: "es0000a"
#' @param pollutant: pollutants from which to download
#'          concentrations. e.g.: c("no2", "o3")
#' @param start_dt: Start date for returned observations. if start_dt
#'          is a date, its year is taken as start date.
#' @param end_dt: End date for returned observations. if end_dt
#'          is a date, its year is taken as end date.
#' @param data.by.file: Boolean with the condition if the data
#'          is taken from the file.
#' @param fileName: Path of the downloaded data files from where the
#'          data is loaded if data.by.file is true
#'
#' @return dataframe with air quality data
#'
#' @author Jaimedgp
get.AQdata <- function(site, pollutant,
                       start_dt, end_dt=NA, data.by.file=FALSE,
                       fileName="data/Curation/AirQuality/Values/") {

    fileName <- paste(fileName, site, ".csv", sep="")
    if (is.na(end_dt)) {
        end_dt <- Sys.Date()
    }

    if (data.by.file & sum(file.exists(fileName)) == length(site)) {
        data.AQ <- data.frame()

        for (fl in fileName) {
            data.AQ <- rbind(data.AQ,
                             read.csv(fl, stringsAsFactor=FALSE) %>%
                                filter(variable %in% pollutant) %>%
                                data.as.datetime("date", "ymd") %>%
                                filter(lubridate::year(date) >= lubridate::year(start_dt) &
                                       lubridate::year(date) <= lubridate::year(end_dt))
                            )
        }
    } else {
        print("Downloading...")

        data.AQ <- saqgetr::get_saq_observations(
            site = site,
            variable = pollutant,
            start = start_dt,
            end = end_dt,
            valid_only = TRUE,
            verbose = FALSE
        )  %>%
            mutate(value=ifelse(value<0, NA, value))

        if (nrow(data.AQ) > 0) {
            data.AQ <- data.AQ %>%
                        openair::timeAverage(avg.time = "day",
                                             type=c("variable", "site"))
        }
    }


    data.AQ %>% mutate(value=ifelse(value<=0, NA, value))
}


#' sv.checkedAQ
#'
#' Download Air quality using checked_AQ information to download
#'      only useful data. Save it in a .rda file
#'
#' @param fileName: Path to checked_AQ file
#'
#' @return dataframe with all air quality data
#'
#' @author Jaimedgp
sv.checkedAQ <- function(start_dt, end_dt, fileName="data/Curation/checked_AQ.csv") {

    if (file.exists(fileName)) {
        sites.data <- read.csv(fileName, stringsAsFactor=F)
        data_AQ <- data.frame()

        for (st in levels(as.factor(sites.data$site))) {
            pll <- levels(as.factor(sites.data[sites.data$site == st,
                                    "variable"]))

            data_AQ <- rbind(data_AQ,
                             get.AQdata(st, pollutant=pll,
                                        start_dt=start_dt, end_dt=end_dt) %>%
                                select(date, site, variable, value) %>%
                                data.frame
                             )
        }
        save(data_AQ, file="data/data_AQ.rda")
        write.csv(data_AQ,
                  "data/data_AQ.csv", row.names=FALSE)
    } else {
        data_AQ <- NULL
    }

    data_AQ
}
