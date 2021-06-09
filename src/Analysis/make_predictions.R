suppressMessages(library(tidyverse))

setwd("~/Repositories/AirQualityCOVID/")
source("src/preprocess.R")


pairs <- read.csv("data/Curation/checked_AQ.csv") %>%
            select(site, variable) %>%
            filter(site != "es1573a") %>%
            mutate(names=paste(site, variable, sep="-"))

data.df <- open.data(pairs[,1],
                     aq.file = "data/all/data_AQ.rda",
                     meteo.file = "data/all/meteorology.rda") %>%
                add.yesterday.meteo(n.days = 3) %>%
                filter(site != "es1573a")

train.df <- data.df %>%
                filter(date < lubridate::ymd("2020-01-01"))
to.predict.df <- data.df %>%
                filter(date >= lubridate::ymd("2020-01-01"))

predictions <- data.frame()

for (i in 1:nrow(pairs)) {
    slice.train <- train.df %>%
                    filter(site == pairs[i, 1],
                           variable == pairs[i, 2]) %>%
                    filter.IQR(columns = "value", n = 5) %>%
                    select(-date, -site, -variable) %>%
                    na.omit

    model <- lm(value ~ ., data=slice.train, na.action=na.omit)

    slice.to.predict <- to.predict.df %>%
                            filter(site == pairs[i, 1],
                                   variable == pairs[i, 2])

    pred <- predict(model, newdata = slice.to.predict)

    pred <- downscaleR:::eqm(slice.train$value,
                             predict(model),
                             pred,
                             n.quantile=99,
                             precip=FALSE, pr.threshold=0,
                             extrapolation="qwerty"
                            )

    predictions <- rbind(predictions,
                         data.frame(site=pairs[i, 1],
                                    variable=pairs[i, 2],
                                    date=slice.to.predict$date,
                                    obs=slice.to.predict$value,
                                    pred=pred
                                   ))
}


write.csv(predictions,
          "data/Results/predictions.csv",
          row.names=F)
