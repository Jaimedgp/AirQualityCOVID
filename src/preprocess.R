########################################################################
#               preProcessing Functions
#
# General functions used to prepare data for data modelization.
#
# @author Jaimedgp
########################################################################


#' open.data
#'
#' open data for model training and join air quality and meteorology variables
#'
#' @param cv.sites: sites codes
#' @param aq.file: path directory to air quality file
#' @param meteo.file: path directory to meteo file
#'
#' @return unique data.frame with all data
#'
#' @author Jaimedgp
open.data <- function(cv.sites,
                      aq.file="data/data_AQ.rda",
                      meteo.file="data/meteorology.rda") {
    # Create dataSet
    load(aq.file)
    load(meteo.file)

    aq <- data_AQ %>%
                filter(site %in% cv.sites) %>%
                mutate(date = lubridate::as_date(date))
    aq[is.na(aq)] <- NA

    mto <- data_Mto %>%
                filter(site %in% cv.sites)

    rm(data_AQ)
    rm(data_Mto)

    joined.df <- merge(aq, mto,
                       by = c("date", "site"), all.x=T) %>%
                    mutate_if(is.factor, as.character)
}

#' filter.IQR.1D
#'
#' Remove all data outside n times the interquartile range. By default the
#'           method uses the interquartile range as reference (Q3 - Q1) but this
#'           can be change by changing the upper quantile.
#'
#' @param cl: numeric vector with data
#' @param n: number of times of iqr for outlayers
#' @param qntl: upper quantile for iqr. Default 0.75 which corresponds to Q3
#'
#' @return numeric vector with the same length of the original in which all
#'         valuers outside interval (n*(q3-q1)) is convert to NaN. This is done
#'         for the cases where cl is a column of a dataframe, so the row can be
#'         removed for all dataframe outside the function.
#'
#' @author Jaimedgp
filter.IQR.1D <- function(cl, n, qntl=0.7) {

    min.IQ <- quantile(cl, (1-qntl), na.rm=TRUE)
    max.IQ <- quantile(cl, qntl, na.rm=TRUE)

    IQ.range <- max.IQ - min.IQ

    cl[which(cl < (min.IQ - (n*IQ.range)))] <- NA
    cl[which(cl > (max.IQ + (n*IQ.range)))] <- NA

    cl
}

#' filter.IQR
#'
#' Apply filter.IQR.1D to the given columns of a dataframe
#'
#' @param dat.df: data.frame with data to transform
#' @param columns: columns to apply filter.IQR.1D
#' @param n: number of times of iqr for outlayers
#' @param qntl: upper quantile for iqr. Default 0.75 which corresponds to Q3
#'
#' @return data.frame with the same length of the original in which all
#'         values (of the selected columns or all numeric columns) outside
#'         interval (n*(q3-q1)) is convert to NaN. This is done for the cases
#'         where cl is a column of a dataframe, so the row can be removed for
#'         all dataframe outside the function.
#'
#' @author Jaimedgp
filter.IQR <- function(dat.df, columns=NULL, n=3, qntl=0.75) {

    if (is.null(columns)) {
        fixed <- dat.df %>% select(-where(is.numeric))
        to.change <- dat.df %>% select(where(is.numeric))
    } else {
        fixed <- dat.df %>% select(-all_of(columns))
        to.change <- dat.df %>% select(all_of(columns))
    }

    cbind(fixed, data.frame(apply(to.change, 2, filter.IQR.1D, n, qntl)))
}


#' add.yesterdaymeteo
#'
#' Addprevious meteorological data for the n.days before
#'
#' @param dat.df: data.frame with data to transform. Must have date column
#' @param n.days: Number of days before to take meteo data
#'
#' @return dataframe with added meteo data
#'
#' @author Jaimedgp
add.yesterday.meteo <- function(dat.df, n.days=1) {

    if (n.days < 1) {
        return(dat.df)
    }

    if (!("date" %in% names(dat.df))) {
        print("Must have a date column")
        return()
    } else {
        by.col <- c("date")
    }
    if("site" %in% names(dat.df)){
        by.col <- c(by.col, "site")
    }
    if("variable" %in% names(dat.df)){
        by.col <- c(by.col, "variable")
    }
    if("Municipio" %in% names(dat.df)){
        by.col <- c(by.col, "Municipio")
    }

    meteo.cl <- c("ws", "wd", "tmed", "prec", "tmin", "tmax",
                  "presMax", "presMin", "RH", "solar.radiation")
    col.add <- names(dat.df)[which(names(dat.df) %in% meteo.cl)]

    yesterday.df <- dat.df

    for (i in 1:n.days) {
        new.df <- dat.df %>%
                select(all_of(by.col), all_of(col.add)) %>%
                mutate(date=date+i)

        yesterday.df <- merge(yesterday.df, new.df,
                              by = by.col,
                              all.x=F, all.y=F,
                              suffixes = c("", paste("", i, "before", sep="."))
                              )
    }

    return(yesterday.df)
}
