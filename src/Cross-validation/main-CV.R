suppressMessages(library(tidyverse))
suppressMessages(library(caret))
#suppressMessages(library(doMC))
#registerDoMC(cores=as.integer(detectCores()*0.75))

setwd("~/Repositories/AirQualityCOVID/")

source("src/Cross-validation/cv-utilities.R")
source("src/Cross-validation/preprocess.R")


#' cross.validation
#'
#' cross validation process
#'
#' @param data.df: dataframe with train and test data to train model
#' @param k.fold: list with train and test index
#' @param method: string with the statistical technique
#' @param param: parameter to adjust model (randomForest->ntree, knn->n)
#'
#' @return list with the time serie predicted and the metrics to evaluate
#'           the accuracy of the model
#'

#' @author Jaimedgp
cross.validation <- function(data.df, k.fold, method, param=0) {
    time.series <- data.frame()


    for (i in seq_along(k.fold)) {
        model <- tryCatch({
            model <- caret::train(value ~.,
                                  data=data.df %>% select(-date),
                                  subset=k.fold[[i]]$train,
                                  method=method,
                                  na.action=na.omit,
                                  family = param,
                                  #ntree=param,
                                  #tuneGrid=data.frame(k=param),
                                  #allowParallel = TRUE
                                  )
                }, error = function(cond) {
                    return(NA)
                }
                , warning = function(cond) {
                    return(NA)
                }
        )

        if (all(is.na(model))) {
            print("NAN")
            return(list(metrics=data.frame(method=method,
                                           param=as.factor(param),
                                           qq.Mapping=NA,
                                           "bias"=NA,
                                           "var.ratio"=NA,
                                           "cor1"=NA,
                                           "cor2"=NA,
                                           "RMSE"=NA),
                        series=data.frame(method=method,
                                          param=as.factor(param),
                                          date=NA,
                                          obs=NA,
                                          pred=NA,
                                          pred.qq=NA
                                         )))
        }

        pred <- predict(model, data.df[k.fold[[i]]$test,])
        time.series <- rbind(time.series,
                             data.frame(date=data.df[k.fold[[i]]$test, "date"],
                                        obs=data.df[k.fold[[i]]$test, "value"],
                                        pred=pred,
                                        pred.qq=qq.mapping(model, pred)
                                       )
                            )
    }
    metrics <- cbind(data.frame(qq.Mapping=c("No", "Yes")),
                     rbind(compute.metrics(time.series$obs, time.series$pred),
                           compute.metrics(time.series$obs, time.series$pred.qq)))

    cbind.cv(list(metrics=metrics,
                  series=time.series),
             list(method=method,
                  param=as.factor(param)))

}


if(sys.nframe() == 0) {
    method <- "glm"

    #params <- c(0)
    params <- c("Gamma")
    #params <- 1:10
    #params <- c(10, 20, 30, 50, 70, 100, 200)

    days <- 0:3
    n.iqr <- 5

    sites.cv <- list("es0118a" = c("no", "no2", "o3", "pm10", "pm2.5"),
                     "es1438a" = c("no", "no2"),
                     "es0890a" = c("no", "no2", "o3"),
                     "es1047a" = c("no", "no2", "o3", "pm10"),
                     "es1137a" = c("no", "no2", "o3", "pm10", "pm2.5"),
                     "es0110a" = c("pm2.5"),
                     "es1632a" = c("o3", "pm10"),
                     "es1580a" = c("no", "no2", "pm10"),
                     "es1340a" = c("pm10"),
                     "es1938a" = c("pm2.5"),
                     "es1239a" = c("no", "no2", "o3", "pm2.5"),
                     "es1181a" = c("pm10"),
                     "es1244a" = c("no", "no2", "pm10"),
                     "es1631a" = c("no", "no2", "pm2.5"),
                     "es1610a" = c("no", "no2", "o3", "pm10"),
                     "es1635a" = c("o3"),
                     "es1697a" = c("o3", "pm2.5"),
                     "es1272a" = c("o3", "pm10", "pm2.5"),
                     "es1096a" = c("pm2.5"),
                     "es1492a" = c("pm2.5")
                     )

    sites.cv <- do.call("rbind",
                        lapply(names(sites.cv),
                               function (x) t(mapply(c, sites.cv[[x]], x))))

    data.df <- open.data(sites.cv[,2],
                         aq.file = "data/all/data_AQ.rda",
                         meteo.file = "data/all/meteorology.rda") %>%
                    filter(date < lubridate::ymd("2020-01-01"))

    #-----------------------
    #  Cross validation
    #-----------------------
    init <- Sys.time()

    cv.results <- data.frame()

    for (dy in days) {
        process.df <- data.df %>%
                    add.yesterday.meteo(n.days = dy)
        for (i in 1:nrow(sites.cv)){
            print(sites.cv[i,])
            slice.df <- process.df %>%
                            filter(site == sites.cv[i, 2],
                                   variable == sites.cv[i, 1]) %>%
                            select(-site, -variable) %>%
                            filter.IQR(columns = "value", n = n.iqr) %>%
                            na.omit

            yr.fold <- leave.one.year.out(date = slice.df$date)

            results <- do.call(rbind.cv,
                               lapply(params,
                                      cross.validation,
                                      data.df=slice.df,
                                      k.fold=yr.fold,
                                      method=method
                                      )) %>%
                           cbind.cv(list(variable=sites.cv[i, 1],
                                         site=sites.cv[i, 2],
                                         days=as.factor(dy)))
            cv.results <- do.call(rbind.cv, list(cv.results, results))
            save(cv.results,
                    file = paste("~/Repositories/AirQualityCOVID/data/Cross-validation/", method, ".rda", sep=""))
        }
    }
    print(Sys.time()-init)
}
