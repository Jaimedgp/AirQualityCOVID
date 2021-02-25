########################################################################
#               Data Processing Functions
#
# General functions used to load and transform data for curation.
#     - data.as.datetime: Convert date column into datetime format
#     - get.AQdata: Load air quality data
#     - group.by.date: Change Date resolution
#
# @author Jaimedgp
########################################################################

# load packages
suppressMessages(library(saqgetr))
suppressMessages(library(lubridate))


data.as.datetime <- function(dataframe, column, FUN){
    # Converte date column into datetime format using lubridate package
    #
    # @params:
    #     dataframe: dataframe with all data
    #     column: name or names of date column
    #     FUN: lubridate function to apply. Can be date ('ymd') or datetime ('ymd <- hms')
    # @return:
    #     dataframe: same dataframe with date column as datetime format

    if (FUN == "ymd_hms") {
        dataframe[, column] <- ymd_hms(dataframe[, column])
    } else if (FUN == "ymd") {
        dataframe[, column] <- ymd(dataframe[, column])
    } else {
        print("No valid FUN. Must be: 'ymd_hms' or 'ymd'")
    }
    dataframe
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


group.by.date <- function(formulation, dataFrame, dateCl, unit="day", FUN="mean") {
    # Summarize data by date, changing the temporal resolution of the dataframe
    #
    # @params:
    #     valueList: List with the columns to summarize.
    #                e.g.: list(columnName=columnValues)
    #     byList: List with date columns to change date resolution
    #                e.g.: list(dateColumnName=dateColumnValues)
    #     dataFrame: Dataframe with all data
    #     unit: Target temporal resolution
    #     FUN: Function by which aggrgation is done
    # @return:
    #     dataframe with temporal resolution changed

    dataFrame[, dateCl] <- floor_date(dataFrame[, dateCl], unit=unit) # Change date resolution

    aggregate(formulation, data=dataFrame, FUN=FUN, rm.na=TRUE)
              #valueList,
              #by=byList,
              #FUN=FUN, rm.na=TRUE)
}
