split.by.site <- function(df, site.lv="all", folder="../data/dataAQ/"){

    if (site.lv == "all") {
        site.lv <- levels(as.factor(df$site))
    }

    for (st in site.lv) {
        write.csv(df[df$site == st, ],
                  paste(folder, st, ".csv", sep=""),
                  row.names=FALSE
                 )
    }
}


pivot.by.pollut <- function(df, pollutants, by="day", site="default") {
    pivoted.df <- lapply(pollutants,function(pll) {

        new.data <- subset(df, variable == pll)

        new.data$date <- round_date(ymd_hms(new.data$date), unit=by)
        new.data <- aggregate(value ~ date + site, new.data, mean)

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


group.by.date <- function(df, by="day", FUN="mean") {
    df$date <- round_date(ymd_hms(df$date), unit=by)

    aggregate(value ~ date + site + variable + unit, df, FUN)
}
