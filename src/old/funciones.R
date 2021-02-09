split.by.site <- function(df, site.lv="all", folder="../data/csv/dataAQ/"){

    if (folder == "../data/csv/dataAQ/") {
        if (site.lv == "all") {
            site.lv <- levels(as.factor(df$site))
        }

        for (st in site.lv) {
            write.csv(df[df$site == st, ],
                    paste(folder, st, ".csv", sep=""),
                    row.names=FALSE
                    )
        }
    } else if (folder == "../data/csv/dataMto/") {
        if (site.lv == "all") {
            site.lv <- levels(as.factor(df$code))
        }

        for (st in site.lv) {
            write.csv(df[df$code == st, ],
                    paste(folder, st, ".csv", sep=""),
                    row.names=FALSE
                    )
        }
    } else {
        print("Something went wrong")
    }
}


pivot.by.pollut <- function(df, pollutants, by="day", site="default", FUN="mean") {
    pivoted.df <- lapply(pollutants,function(pll) {

        new.data <- subset(df, variable == pll)
        new.data <- group.by.date(new.data, by=by, FUN=FUN)

        names(new.data)[names(new.data) == "value"] <- pll
        new.data[, c("date", "site", pll)]

    }) %>%
    reduce(full_join, by = c("date", "site"))

    if (site != "default") {
        pivoted.df <- pivoted.df[pivoted.df$site == site,]
        pivoted.df$id <- seq.int(length(levels(as.factor(pivoted.df$date))))

    } else {
        level <- levels(as.factor(pivoted.df$date))

        id.date <- seq.int(length(level))
        pivoted.df$id <- rep(0, nrow(pivoted.df))

        for (i in levels(as.factor(pivoted.df$site))) {
            pivoted.df[pivoted.df$site == i, ]$id <- seq.int(nrow(pivoted.df[pivoted.df$site == i, ]))
        }
    }

    pivoted.df
}
