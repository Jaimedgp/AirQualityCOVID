##
#
# - date.as.datetime(dataFrame)
# - group.dt(dataFrame, unit="day", FUN="mean")
# -
#
# @author Jaimedgp
##


suppressMessages(library(lubridate))
suppressMessages(library(tidyverse))
suppressMessages(library(saqgetr))
suppressMessages(library(ggplot2))


date.as.datetime <- function(dataFrame){
    # Mutate date column from dataFrame into datatime format using
    # lubridate package assuming that date format yyyy-mm-dd hh:mm:ss

    dataFrame$date <- ymd_hms(dataFrame$date)
    dataFrame
}


group.dy <- function(dataFrame, unit="day", FUN="mean") {
    # Group data by unit using FUN function

    dataFrame$date <- floor_date(dataFrame$date, unit=unit)
    new.df <- aggregate(list(value=dataFrame$value),
                        by=list(date=dataFrame$date,
                                variable=dataFrame$variable,
                                site=dataFrame$site),
                        FUN, na.rm=TRUE, na.action=na.omit)
    new.df
}


get.site.municipio <- function(municipio,
                               fileName="../data/csv/sitesAQ.csv") {
    # Obtain air quality urban traffic stations in municipio

    stations <- read.csv(fileName,
                            stringsAsFactors=FALSE) %>%
                filter(Municipio == municipio)
    site <- levels(as.factor(stations$site))

    if (length(site) == 0) {
        print(paste("Cannot find any stations in", municipio))
    } else {
        site
    }
}


get.AQdata <- function(site="", pollutant, start_dt, end_dt,
                       data.by.file=FALSE, fileName="../data/csv/dataAQ/") {
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
                                date.as.datetime()
                            #%>% filter(year(date) >= start_dt)
                            )
        }
    } else {
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


group_yr.by.dt <- function(dataFrame, last.yr=2020) {
    # Group dataFrame (with data from different years) by date. The the mean of
    #     all data from all years before last.yr are group by date

    new.df <- dataFrame[year(dataFrame$date) < last.yr,]
    year(new.df$date) <- last.yr
    new.df <- aggregate(list(value=new.df$value),
                        by=list(date=new.df$date,
                                variable=new.df$variable,
                                site=new.df$site),
                        mean, na.rm=TRUE, na.action=na.omit)
    new.df
}


compare.years <- function(dataFrame, last.yr=2020) {
    # Create a new.dataFrame with the data of last.yr in one column, the data of
    #     the year before last.yr in other and the mean of all years before last.yr

    data.last <- dataFrame[year(dataFrame$date) == last.yr,]
    #data.19 <- dataFrame[year(dataFrame$date) == last.yr-1,]
    #year(data.19$date) <- last.yr

    #new.df <- merge(x=data.20,
    #                y=data.19,
    #                by=c("date", "variable", "site"), all=T)
    #names(new.df)[which(names(new.df) == "value.x")] <- as.character(last.yr)
    #names(new.df)[which(names(new.df) == "value.y")] <- as.character(last.yr-1)

    #new.df <- merge(x=new.df,
    new.df <- merge(x=data.last,
                    y=group_yr.by.dt(dataFrame, last.yr),
                    by=c("date", "variable", "site"), all=T)
    names(new.df)[which(names(new.df) == "value.x")] <- as.character(last.yr)
    names(new.df)[which(names(new.df) == "value.y")] <- paste(
                    as.character(year(min(dataFrame$date))),
                    as.character(last.yr-1),
                    sep="-")
    new.df
}


mean.in.period <- function(dataFrame, periods, columns) {
    # Divide dataFrame's columns by periods and calculate the mean of each

    all.data <- data.frame()

    site <- levels(as.factor(dataFrame$site))
    nnrow <- length(periods) - 1
    nncol <- 2*length(columns) + 3

    for (i in 1:length(site)) {
        # Create the dataframe for each station
        pollutant <- levels(as.factor(dataFrame[dataFrame$site == site[i],
                                                "variable"]))

        all.means <- data.frame(matrix(rep(NA, nnrow*nncol*length(pollutant)),
                                           nrow=nnrow*length(pollutant),
                                           ncol=nncol)
                                      )
        y.i <- (nnrow*length(pollutant))

        all.means[1:y.i, 2] <- rep(site[i], nnrow*length(pollutant))

        for (j in 1:length(pollutant)) {
            j.init <- (j-1)*nnrow

            all.means[(j.init+1):(j.init+nnrow), 3] <- rep(pollutant[j], nnrow)

            data.poll <- dataFrame[dataFrame$variable == pollutant[j],]

            for (k in 1:(length(periods)-1)) {
                x.k <- (j-1)*nnrow

                all.means[x.k+k, 1] <- names(periods)[k]
                for (l in 1:length(columns)) {
                    l.init <- (l-1)*2
                    all.means[x.k+k, l.init+4] <- mean(
                        data.poll[data.poll$date >= periods[[k]] &
                        data.poll$date < periods[[k+1]], columns[l]], na.rm=T
                    )
                    all.means[x.k+k, l.init+5] <- sd(
                        data.poll[data.poll$date >= periods[[k]] &
                        data.poll$date < periods[[k+1]], columns[l]], na.rm=T
                    )
                }
            }
        }

        all.data <- rbind(all.data,
                          all.means)
    }

    # Set columns names
    names(all.data)[1:3] <- c("period", "site", "variable")
    for (i in 1:length(columns)){
        i.init <- (i-1)*2
        names(all.data)[i.init+4] <- paste("mean(", columns[i], ")", sep="")
        names(all.data)[i.init+5] <- paste("std(" , columns[i], ")", sep="")
    }
    all.data
}


get.difference <- function(dataFrame, mainCol, restCol,
                                      mainSTD=NULL, restSTD=NULL) {
    # Calculate the difference relative between each restCol and mainCol and append by column

    new.df <- dataFrame[1:3]

    for (i in 1:length(mainCol)) {
        for (j in 1:length(restCol)) {
            new.df <- cbind(new.df,
                            ((dataFrame[, mainCol[i]] - dataFrame[, restCol[j]])
                                / dataFrame[, restCol[j]])
                            )
            names(new.df)[ncol(new.df)] <- paste(restCol[j], mainCol[i], sep=".vs.")

            if (!is.null(mainSTD)) {
                new.df <- cbind(new.df,
                                (dataFrame[, mainSTD[i]] / dataFrame[, restCol[j]]) -
                                (dataFrame[, restSTD[j]]*dataFrame[, mainCol[i]] / dataFrame[, restCol[j]]^2)
                                )
                names(new.df)[ncol(new.df)] <- paste(restSTD[j], mainSTD[i], sep=".vs.")
            }

        }
    }
    new.df
}


plot.var.data <- function(data.plot, dataFrame, columns) {

    data.plot <- data.plot + geom_line(aes(x = date,
                                           y = dataFrame[,columns[1]],
                                           color = columns[1]
                                          ),
                                       size = 2.0)

    data.plot +
       geom_hline(yintercept=0,
                  color = "gray", size=1.0) +
        labs(color = "Variaciones",
             x = "Date",
             y = "Variaciones")
}


plot.AQdata <- function(data.plot, dataFrame, columns) {

    data.plot <- data.plot + geom_line(aes(x = date,
                                           y = dataFrame[,columns[1]],
                                           color = columns[1]
                                          ),
                                       size = 2.0)
    data.plot <- data.plot + geom_line(aes(x = date,
                                           y = dataFrame[,columns[2]],
                                           color = columns[2]
                                          ),
                                       size = 2.0)

   data.plot +
       labs(color = "Years",
            x = "Date",
            y = "Concentration")
}


plot.data <- function(dataFrame, columns, periods, type) {
    # Plot the data
    site <- levels(as.factor(dataFrame$site))
    variable <- levels(as.factor(dataFrame$variable))

    data.plot <- ggplot(data=dataFrame) +
        facet_wrap(~variable+site, ncol=min(length(variable),
                                            length(site)))

    if (type == "aq") {
        data.plot <- plot.AQdata(data.plot, dataFrame, columns)
    } else if (type == "var") {
        data.plot <- plot.var.data(data.plot, dataFrame, columns)
    }

    for (k in 2:(length(periods)-1)) {
        data.plot <- data.plot + geom_vline(xintercept = as.numeric(periods[k]),
                                            linetype=4, color = "black", size=1.5)
    }
    data.plot
}
