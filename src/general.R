suppressMessages(library(saqgetr))


filter.by.std <- function(dataFrame, columns, numSTD=3) {
    # Remove data outside the confidence interval of numSTD sigmas

    for (cl in columns) {
        mn <- mean(dataFrame[, cl], na.rm=TRUE)
        std <- sd(dataFrame[, cl], na.rm=TRUE)

        dataFrame[which(dataFrame[, cl] < mn-numSTD*std), cl] <- NaN
        dataFrame[which(dataFrame[, cl] > mn+numSTD*std), cl] <- NaN
    }
    dataFrame
}


group.by.date <- function(df, unit="day", FUN="mean") {
    # Summarize data by date, changing the temporal resolution of the dataframe
    if (unit == "jaime") {
        df$date <- floor_date(ymd_hms(df$date), unit="8hours")
        df <- aggregate(value ~ date + site + variable + unit, df, FUN)

        df$date <- floor_date(ymd_hms(df$date), unit="day")
        FUN <- "max"

    } else {
        df$date <- floor_date(ymd_hms(df$date), unit=unit)
    }

    aggregate(df$value, by=list(df$date, df$variable), FUN, rm.na=TRUE)
}


get.AQdata <- function(site="", pollutant, start_dt,
                       data.by.file=FALSE, fileName="../data/curation/dataAQ/") {
    # Obtain air quiality data of site station.
    #
    # Data can be obtain from file (data.by.file = TRUE)
    #     or downloaded from saqgetr (data.by.file = FALSE)

    fileName <- paste(fileName, site, ".csv", sep="")

    if (data.by.file & sum(file.exists(fileName)) == 2) {
        data.AQ <- data.frame()

        for (fl in fileName) {
            data.AQ <- rbind(data.AQ,
                             read.csv(fl, stringsAsFactor=FALSE) %>%
                                filter(variable %in% pollutant) %>%
                                date.as.datetime() %>%
                                filter(year(date) >= start_dt)
                            )
        }
    } else {
        suppressMessages(data.AQ <- get_saq_observations(
            site = site,
            variable = pollutant,
            start = start_dt,
            valid_only = TRUE,
            verbose = TRUE
        ))
    }
    data.AQ
}
