filter.by.std <- function(dataFrame, columns, numSTD=3) {

    for (cl in columns) {
        mn <- mean(dataFrame[, cl], na.rm=TRUE)
        std <- sd(dataFrame[, cl], na.rm=TRUE)

        dataFrame[which(dataFrame[, cl] < mn-numSTD*std), cl] <- NaN
        dataFrame[which(dataFrame[, cl] > mn+numSTD*std), cl] <- NaN
    }
    dataFrame
}

