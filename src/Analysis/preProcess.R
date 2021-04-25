########################################################################
#               preProcessing Functions
#
# General functions used to prepare data for data modelization.
#
# @author Jaimedgp
########################################################################


# Remove seasonal component of the additive time serie.
#
# @params:
#     - cl: numeric vector with additive time serie. Must have at
#           least two periods of length.
#     - freq: period of seasonality. Default: 365 corresponds
#             to one natural year in dialy resolution
# @return:
#     vector with time serie less its seasonal component. If there
#         is not enough data to deseasonal, return cl vector
#
# @author Jaimedgp
deseason.1D <- function(cl, freq = 365) {

    if (length(cl) > 2*freq) {
        new.cl <- na.omit(cl)
        dcomp <- decompose(ts(new.cl, frequency = freq))

        cl[which(!is.na(cl))] <- as.vector(new.cl - dcomp$seasonal)

        return(cl)
    } else {return(NaN) }
}


# Transform data by scaling to given interval.
#
# @params:
#     - cl: numeric vector with data to transform.
#     - interval: vector with the interval to transform data.
# @return:
#     numeric vector with the data scaled to the given interval. The max and
#         min value of the origin data are passed as attributes for un-do.
#
# @author Jaimedgp
range.1D <- function(cl, interval=c(0, 1)) {

    attributes(cl) <- list("min"=min(cl), "max"=max(cl))

    coeff <- (interval[2] - interval[1]) / (max(cl) - min(cl))

    (cl - min(cl)) * coeff + interval[1]
}


# Un-do data transformation by re-scaling to the original interval.
#
# @params:
#     - cl: numeric vector with data to transform.
#     - attri: if cl has not original interval as attribute, pass as param.
# @return:
#     numeric vector with the data scaled to the original interval.
#
# @author Jaimedgp
un.range.1D <- function(cl, attri=NULL) {

    if (is.null(attri)) {
        attri <- attributes(cl)
        attributes(cl) <- NULL
    }

    if (sum(c("min", "max") %in% names(attri)) < 2) {
        print("Need pass Max, Min values as attributes")
        return()
    }

    coeff <- (attri[["max"]] - attri[["min"]]) / (max(cl) - min(cl))

    (cl - min(cl)) * coeff + attri[["min"]]
}


# Remove all data outside n times the interquartile range. By default the
#     method uses the interquartile range as reference (Q3 - Q1) but this can
#     be change by changing the upper quantile.
#
# @params:
#     - cl: numeric vector with data
#     - n: number of times of iqr for outlayers
#     - qntl: upper quantile for iqr. Default 0.75 which corresponds to Q3
# @return:
#     numeric vector with the same length of the original in which all
#         valuers outside interval (n*(q3-q1)) is convert to NaN. This is done
#         for the cases where cl is a column of a dataframe, so the row can be
#         removed for all dataframe outside the function.
#
# @author Jaimedgp
filter.IQR.1D <- function(cl, n, qntl=0.7) {

    min.IQ <- quantile(cl, (1-qntl), na.rm=TRUE)
    max.IQ <- quantile(cl, qntl, na.rm=TRUE)

    IQ.range <- max.IQ - min.IQ

    cl[which(cl < (min.IQ - (n*IQ.range)))] <- NA
    cl[which(cl > (max.IQ + (n*IQ.range)))] <- NA

    cl
}


# Apply filter.IQR.1D to the given columns of a dataframe
#
# @params:
#     - dat.df: data.frame with data to transform
#     - columns: columns to apply filter.IQR.1D
#     - n: number of times of iqr for outlayers
#     - qntl: upper quantile for iqr. Default 0.75 which corresponds to Q3
# @return:
#     data.frame with the same length of the original in which all
#         values (of the selected columns or all numeric columns) outside
#         interval (n*(q3-q1)) is convert to NaN. This is done for the cases
#         where cl is a column of a dataframe, so the row can be removed for
#         all dataframe outside the function.
#
# @author Jaimedgp
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


# Transform data by scaling to given interval.
#
# @params:
#     - dat.df: data.frame with data to transform
#     - columns: columns to apply filter.IQR.1D
#     - interval: vector with the interval to transform data.
# @return:
#     data.frame with the data scaled to the given interval. The max and
#         min value of the origin data are passed as attributes of each
#         transformed columns for un-do.
#
# @author Jaimedgp

range.df <- function(dat.df, columns=NULL, interval=c(0, 1)) {

    if (is.null(columns)) {
        fixed <- dat.df %>% select(-where(is.numeric))
        columns <- dat.df %>% select(where(is.numeric)) %>% names()
    } else {
        fixed <- dat.df %>% select(-all_of(columns))
    }

    new.dat <- data.frame(lapply(columns, function(nm){
        range.1D(dat.df[, nm])
    } ))

    names(new.dat) <- columns

    cbind(fixed, new.dat)
}


# Un-do data transformation by re-scaling to the original interval.
#
# @params:
#     - dat.df: data.frame with data to transform
#     - columns: columns to apply filter.IQR.1D
#     - attri: if cl has not original interval as attribute, pass as param.
# @return:
#     data.frame with the data scaled to the original interval.
#
# @author Jaimedgp
un.range.df <- function(dat.df, columns=NULL, attri=NULL) {

    if (is.null(columns)) {
        fixed <- dat.df %>% select(-where(is.numeric))
        to.change <- dat.df %>% select(where(is.numeric))
    } else {
        fixed <- dat.df %>% select(-all_of(columns))
        to.change <- dat.df %>% select(all_of(columns))
    }

    cbind(fixed, apply(to.change, 2, un.range.1D, attri))
}
