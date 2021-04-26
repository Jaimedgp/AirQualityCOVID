suppressMessages(library(tidyverse))

# Working directory
setwd("~/Repositories/AirQualityCOVID")

source("src/Analysis/useData.R")
source("src/Analysis/preProcess.R")
source("src/Analysis/modelImplementation.R")

methodes <- c("lm", "knn", "rf")
all.cv <- list()

for (mth in methodes) {
    load(paste("data/Cross-validation/", mth, ".rda", sep=""))

    all.cv <- do.call(rbind.cv,
                      list(all.cv,
                           cbind.cv(cross.val,
                                    list(method = as.factor(mth)))
                           ))
    rm(cross.val)
}

cross.val <- all.cv
save(cross.val,
     file = "data/Cross-validation/final.rda")
