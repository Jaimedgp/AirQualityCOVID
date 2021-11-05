library(tidyverse)

setwd("AirQualityCOVID/")
source("src/preprocess.R")
source("src/analysis/functions.R")

data.df <- merge(read.csv("data/results/predictions.csv"),
                 read.csv("data/curation/checked_AQ.csv"),
                 by = c("site", "variable"),
                 all.x = T, all.y=F
                ) %>%
            filter(variable == "no2") %>%
            group_by(Municipio, variable, date) %>%
            summarise(obs = mean(obs, na.rm=T),
                      pred = mean(pred, na.rm=T)
                     ) %>%
            mutate(diff = relative.change(obs, pred)) %>%
            mutate(diff=filter.IQR.1D(diff, 5))


write.csv(data.df,
          "data/results/predictions_municipios.csv",
          row.names=F)
