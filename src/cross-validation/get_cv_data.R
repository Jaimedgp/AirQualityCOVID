suppressMessages(library(tidyverse))

# Working directory
setwd("~/Repositories/AirQualityCOVID")

source("src/Cross-validation/cv-utilities.R")
source("src/Cross-validation/preprocess.R")

methodes <- c("lm", "glm", "knn", "rf")
all.cv <- list()

for (mth in methodes) {
    load(paste("data/Cross-validation/", mth, ".rda", sep=""))
    print(mth)

    all.cv <- do.call(rbind.cv,
                      list(all.cv,
                           cbind.cv(cv.results,
                                    list(method = as.factor(mth)))
                           ))
    rm(cv.results)
}

cv.results <- all.cv
save(cv.results,
     file = "data/Cross-validation/final.rda")
