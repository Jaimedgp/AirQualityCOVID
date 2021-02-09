library(lubridate)
library(worldmet)

FUN <- function(st) {
    data.st <- dataMto <- importNOAA(code = st,
                                     year = 2010:2021,
                                     hourly = TRUE,
                                     n.cores = 12,
                                     quiet = FALSE,
                                     path = NA
                                    )

    resume <- data.frame(site = rep(st, rep(2)),
                         start_dt = min(data.st$date),
                         end_dt = max(data.st$date)
                        )
    resume <- cbind(resume, data.frame(matrix(rep(NA, 19), nrow=1)))

    row.names(resume) <- c("resolucion/h.", "proporcion")
    names(resume) <- c('site', 'start_dt', 'end_dt',
                       'ws', 'wd', 'air_temp', 'atmos_pres', 'visibility', 'dew_point', 'RH',
                       'ceil_hgt', 'cl_1', 'cl_2', 'cl_3', 'cl', 'cl_1_height', 'cl_2_height',
                       'cl_3_height', 'precip_12', 'precip_6', 'precip', 'pwc')

    columNames <- which(names(data.st) %in% names(resume))
    colDate <- which(names(data.st) == 'date')
    for (cl in columNames) {
        a <- data.st[, c(colDate, cl)]
        a <- a[complete.cases(a), ]

        if (nrow(a) > 0) {
            minResolut <- max(a$date) - min(a$date)

            for (i in 2:nrow(a)) {
                resolut <- a$date[i] - a$date[i-1]
                if (resolut > 0) {
                    minResolut <- min(minResolut, resolut)
                }
            }

            resolut.hr <- as.numeric(minResolut / 3600)
            resume[,names(data.st)[cl]] <- c(resolut.hr,
                                             resolut.hr*nrow(a) / nrow(data.st)
                                            )

        }
    }
    resume
}

data <- read.csv("../data/csv/sitesMto.csv")

all.resumen <- do.call("rbind", lapply(levels(data$code), FUN))
write.csv(all.resumen, "test.csv")
