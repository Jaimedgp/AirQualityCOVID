suppressMessages(library(tidyverse))

# Working directory
setwd("AirQualityCOVID/")

source("src/cross-validation/cv-utilities.R")
source("src/cross-validation/preprocess.R")

methodes <- c("lm", "glm", "knn", "rf")
all.cv <- list()

for (mth in methodes) {
    load(paste("data/cross-validation/", mth, ".rda", sep=""))
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
     file = "data/cross-validation/final.rda")
write.csv(cv.results,
          "data/cross-validation/final.csv", row.names=FALSE)
