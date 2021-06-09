########################################################################
#               Cross Validation Utilities
#
# General functions used to make cross validation.
#
# @author Jaimedgp
########################################################################


#' deseason.1D
#'
#' Remove seasonal component of the additive time serie.
#'
#' @param cl: numeric vector with additive time serie. Must have at least
#'          two periods of length.
#' @param freq: period of seasonality. Default: 365 corresponds to one
#'          natural year in dialy resolution
#'
#' @return vector with time serie less its seasonal component. If there
#'          is not enough data to deseasonal, return cl vector
#'
#' @author Jaimedgp
deseason.1D <- function(cl, freq = 365) {

    if (length(cl) > 2*freq) {
        new.cl <- na.omit(cl)
        dcomp <- decompose(ts(new.cl, frequency = freq))

        cl[which(!is.na(cl))] <- as.vector(new.cl - dcomp$seasonal)

        return(cl)
    } else {return(NaN) }
}


#' leave.year.out
#'
#' fold date values into years, substraction one year for test and using
#'           the resto for training model
#'
#' @param date: dataframe column with dates. Must have the same length
#'           than the original dataframe.
#'
#' @return list with train and test partitions
#'
#' @author Jaimedgp
leave.one.year.out <- function(date) {
    years <- as.factor(lubridate::year(date))

    if (length(levels(years)) < 2) {
        print("Not enough years")
        return()
    }

    yr.fold <- list()
    for (yr in levels(years)) {
        yr.fold[[yr]]$train <- which(lubridate::year(date) != as.numeric(yr))
        yr.fold[[yr]]$test <- which(lubridate::year(date) == as.numeric(yr))
    }
    yr.fold
}


#' compute.metrics
#'
#' Compute some metrics to evaluate model performance. cor2 corresponde to the correlation
#'       of the deseasonal data
#'
#' @param obs: raw observations
#' @param pred: predictions obtained by the model
#'
#' @return dataframe with the obtained values
#'
#' @author Jaimedgp
compute.metrics <- function(obs, pred) {
    pred.ds <- deseason.1D(cl = pred)
    obs.ds <- deseason.1D(cl = obs)

    metrics <- data.frame("bias"=mean(pred) / mean(obs),
                          "var.ratio"=var(pred) / var(obs),
                          "cor1"=cor(pred, obs),
                          "cor2"=cor(pred.ds, obs.ds),
                          "RMSE"=sqrt(mean((pred - obs)^2))
                         )
    metrics
}


#' qq.mapping
#'
#' Apply QQ Mapping technique to prediction
#'
#' @param model: model trained with caret package caret::train
#' @param pred: predictions obtained by the model
#' @param n.quantile: number of quantiles used
#' @param extrapolation: some parameter used by downscaleR
#'
#' @return prediction qith QQ Mapping correction
#'
#' @author Jaimedgp
qq.mapping <- function(model, pred, n.quantile=99, extrapolation="qwerty") {
    pred.qq <- downscaleR:::eqm(model$trainingData$`.outcome`,
                                predict(model),
                                pred,
                                n.quantile=n.quantile,
                                precip=FALSE, pr.threshold=0,
                                extrapolation=extrapolation
                               )
}


#' cbind.cv
#'
#' Method to cbind list returned by cross.validation function
#'
#' @param cv.list: list returned by cross.validation function and formed by two
#'           dataframes with metrics and series
#' @param value list: list with values to join. Names are used as columns names
#'
#' @return merged list
#'
#' @author Jaimedgp
cbind.cv <- function(cv.list, value.list) {

    for (nm in names(value.list)){
        vl <- value.list[[nm]]

        for (i in 1:length(cv.list)) {
            cv.list[[i]][, nm] <- vl
            cv.list[[i]] <- cv.list[[i]] %>% relocate(all_of(nm))
        }
    }

    cv.list
}


#' rbind.cv
#'
#' Method to rbind list returned by cross.validation function
#'
#' @param ...: lists returned by cross.validation function and formed by two
#'           dataframes with metrics and series
#'
#' @return merged list
#'
#' @author Jaimedgp
rbind.cv <- function(...) {

    cv.list <- enexprs(...)

    end <- list(metrics=data.frame(),
                series=data.frame())

    for (i in 1:length(cv.list)) {
        end$metrics <- rbind(end$metrics,
                             cv.list[[i]]$metrics)
        end$series <- rbind(end$series,
                            cv.list[[i]]$series)
    }

    end
}


