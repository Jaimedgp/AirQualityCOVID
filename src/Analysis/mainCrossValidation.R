
suppressMessages(library(caret))
suppressMessages(library(tidyverse))

# parallelization
suppressMessages(library(doMC))
registerDoMC(cores=as.integer(detectCores()*0.75))

# Working directory
setwd("~/Repositories/AirQualityCOVID")

source("src/Analysis/useData.R")
source("src/Analysis/preProcess.R")
source("src/Analysis/modelImplementation.R")


calc.metrics <- function(model, x.obs, y.obs, date) {

    predictions <- qq.predict(model = model, x.obs = x.obs, y.obs = y.obs, date)

    x.ds.obs <- apply(x.obs, 2, deseason.1D)
    y.ds.obs <- deseason.1D(y.obs)

    predictions.ds <- qq.predict(model = model,
                                 x.obs = x.ds.obs, y.obs = y.ds.obs, date)

    mtrcs <- comp.metrics(obs = y.obs, pred = predictions$pred,
                          obs.ds = y.ds.obs, pred.ds = predictions.ds$pred,
                          pred.qq = predictions$pred.qq,
                          pred.qq.ds = predictions.ds$pred.qq
                         )

    list(metrics=mtrcs,
         predictions=predictions,
         predictions.ds=predictions.ds)
}


cross.validation <- function(dat, target, k.fold) {
    # Do k-iteration of k-fold cv by pop yr data for test
    #     and the rest for training
    #
    # @params: omit.cl: columns to omit in the regression

    y.train <- dat[k.fold$train, target]
    x.train <- dat %>% select(-all_of(target), -date) %>% slice(k.fold$train)
    y.test <- dat[k.fold$test, target]
    x.test <- dat %>% select(-all_of(target), -date) %>% slice(k.fold$test)

    date.test <- dat %>% select(date) %>% slice(k.fold$test)

    #params <- 0
    #params <- 1:10
    params <- c(10, 20, 30, 50, 100, 150, 200)

    cross.param <- do.call(rbind.cv,
                           lapply(params,
                                  function(param) {
                                      model <- train(
                                          y = y.train,
                                          x = x.train,
                                          method="rf",
                                          ntree=param,
                                          #tuneGrid=data.frame(k=param),
                                          allowParallel = TRUE
                                          )

                                      cbind.cv(calc.metrics(model,
                                                            x.test,
                                                            y.test,
                                                            date.test),
                                               list(param=as.factor(param)))
                                  }))
}



if(sys.nframe() == 0) {

    method <- "rf"

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

    all.df <- open.data(sites = names(sites.cv),
                        end_dt = lubridate::ymd("2020-01-01"),
                        airQuality.fl = "data/full_data/data_AQ.rda",
                        meteo.fl = "data/full_data/meteorology.rda"
                        )

    days <- 0
    n.iqr <- 5

    cross.val <- list()
    init <- Sys.time()

    # Create one model for each pair of station-pollutant
    for (st in names(sites.cv)) {
        print(st)
        pollutants <-sites.cv[[st]]

        for (pll in pollutants) {
            print(paste("", pll, sep="    "))
            for (dy in days) {
                data.st <- all.df[[st]] %>%
                            select(-all_of(
                                pollutants[-which(pollutants == pll)])) %>%
                            filter.IQR(columns=pll, n=n.iqr) %>%
                            add.yesterday.meteo(n.days=dy) %>%
                            na.omit

                yr.fold <- leave.one.year.out(data.st$date)
                cross.row <- do.call(rbind.cv,
                                     lapply(seq_along(yr.fold),
                                            function(ind, lists, name) {
                                                cross.validation(data.st,
                                                                 pll,
                                                                 lists[[ind]]
                                                                 ) %>%
                                                    cbind.cv(
                                                        list(year=name[ind],
                                                             variable=pll,
                                                             site=st,
                                                             days=as.factor(dy)
                                                             ))
                                            },
                                            lists=yr.fold,
                                            name=names(yr.fold)
                                     ))
                    cross.val <- do.call(rbind.cv, list(cross.val, cross.row))
            }
        }

        save(cross.val,
            file = paste("data/Cross-validation/",
                        method, ".rda", sep=""))
    }
    print(Sys.time()-init)
}
