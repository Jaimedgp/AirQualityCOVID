suppressMessages(library(tidyverse))

# Working directory
setwd("~/Repositories/AirQualityCOVID")

source("src/Analysis/useData.R")
source("src/Analysis/preProcess.R")
source("src/Analysis/modelImplementation.R")

method <- "lm"

load(paste("data/Cross-validation/", method, ".rda", sep=""))


final.sites <- list("es0118a" = c("no", "no2", "o3", "pm10", "pm2.5"),
                    "es1438a" = c("no", "no2"),
                    "es0890a" = c("no", "no2", "o3"),
                    "es1047a" = c("pm10"),
                    "es1137a" = c("no", "no2", "o3", "pm10", "pm2.5"),
                    "es0110a" = c("pm2.5"),
                    "es1632a" = c("o3", "pm10"),
                    "es1580a" = c("no", "no2", "pm10"),
                    "es1340a" = c("pm10")
                    )

final.cv <- list()
for (st in names(final.sites)) {
    final <- list(metrics = cross.val$metrics %>%
                        filter(site == st,
                               variable %in% final.sites[[st]]),
                  predictions = cross.val$predictions %>%
                        filter(site == st,
                               variable %in% final.sites[[st]]),
                  predictions.ds = cross.val$predictions.ds %>%
                        filter(site == st,
                               variable %in% final.sites[[st]])
                  )

    final.cv <- do.call(rbind.cv, list(final, final.cv))
}

save(final.cv,
     file = paste("data/Cross-validation/",
                    method, "-final.rda", sep=""))
