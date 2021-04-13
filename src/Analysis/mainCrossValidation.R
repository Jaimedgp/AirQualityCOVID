
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
                                      model <- train(y = y.train,
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

    sites.lv <- c("es0118a", "es1438a",  # Madrid, Barcelona
                  "es1625a", "es0890a",  # Valencia, Sevilla
                  "es1047a", "es1137a",  # Zaragoza, Vigo
                  "es1632a", "es0110a",  # Valladolid, Bilbao
                  "es1580a", "es1340a"   # Santander, Huelva
                  )

    all.df <- open.data(sites = sites.lv,
                        end_dt = lubridate::ymd("2020-01-01"),
                        airQuality.fl = "data/full_data/data_AQ.rda",
                        meteo.fl = "data/full_data/meteorology.rda"
                        )

    days <- 0:3
    n.iqr <- 10

    cross.val <- list()
    init <- Sys.time()

    # Create one model for each pair of station-pollutant
    for (st in sites.lv[1]) {
        print(st)
        names.st <- names(all.df[[st]])
        pollutants <- names.st[which(names.st %in% c("no", "no2"))]  #, "pm10",
                                                      # "pm2.5", "o3"))]
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
    }
    print(Sys.time()-init)

    save(cross.val,
         file = paste("data/Cross-validation/cross_val/",
                      method, ".rda", sep=""))
}
