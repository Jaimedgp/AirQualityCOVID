suppressMessages(library(saqgetr))
suppressMessages(library(lubridate))


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
