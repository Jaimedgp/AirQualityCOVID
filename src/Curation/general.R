#################################################################
#               Curation Functions
#
# Funciones generales para el proceso de curacion de los datos
#
# @author Jaimedgp
#################################################################

suppressMessages(library(saqgetr))
suppressMessages(library(lubridate))


data.as.datetime <- function(dataframe, column, FUN){
    # Converte time column into datetime format

    if (FUN == "ymd_hms") {
        dataframe[, column] <- ymd_hms(dataframe[, column])
    } else if (FUN == "ymd") {
        dataframe[, column] <- ymd(dataframe[, column])
    } else {
        print("No FUN")
    }

    dataframe
}


get.AQdata <- function(site, pollutant, start_dt, end_dt=NA,
                       data.by.file=FALSE, fileName="data/Curation/AirQuality/Values/") {
    #-------------------------------------------------------------------
    # Obtain air quiality data of site station.
    #
    # Data can be obtain from file (data.by.file = TRUE)
    #     or downloaded from saqgetr (data.by.file = FALSE)
    #-------------------------------------------------------------------

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
        print("Download")
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


group.by.date <- function(valueList, byList, dataFrame, unit="day", FUN="mean") {
    # Summarize data by date, changing the temporal resolution of the dataframe
    if (unit == "jaime") {
        byList[[1]] <- floor_date(byList[[1]], unit="8hours")

        dataFrame <- aggregate(valueList,
                               by=byList,
                               FUN=FUN, na.rm=TRUE)

        byList[[1]] <- floor_date(byList[[1]], unit="day")
        FUN <- "max"

    } else {
        byList[[1]] <- floor_date(byList[[1]], unit=unit)
    }

    aggregate(valueList,
              by=byList,
              FUN=FUN, rm.na=TRUE)
}


pivot.short.table <- function(df, cols) {
    cmn.nm <- names(df)[-which(names(df) %in% cols)]
    new.df <- data.frame()

    for (cl in cols) {
        new.row <- cbind(df[, cmn.nm],
                         variable=rep(cl, nrow(df)),
                         value=df[, cl])
        new.df <- rbind(new.df,
                        new.row)
    }

    new.df
}


pivot.long.table <- function(df, valueCl, variableCl) {
    cmn.nm <- names(df)[-which(names(df) %in% c(valueCl, variableCl))]
    lv <- levels(as.factor(df[, variableCl]))

    new.df <- cbind(df[df[, variableCl] == lv[1], cmn.nm],
                    df[df[, variableCl] == lv[1], valueCl])
    names(new.df)[ncol(new.df)] <- lv[1]

    if (length(lv) > 1) {
        for (l in lv[2:length(lv)]) {
            new.row <- df[df[, variableCl] == l, ]

            new.df <- merge(new.df,
                            cbind(new.row[, cmn.nm],
                                new.row[, valueCl]),
                            by=cmn.nm, all = T
                            )
            names(new.df)[ncol(new.df)] <- l
        }
    }
    new.df
}
