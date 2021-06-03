
# Open data
#
# @params:
#     - sites:
#     - variables:
#     - start_dt:
#     - end_dt:
#     - airQuality.fl:
#     - meteo.fl:
# @return:
#     pass
#
# @author Jaimedgp

open.data <- function(sites,
                      variables=c("no", "no2", "o3", "pm10", "pm2.5"),
                      start_dt=lubridate::ymd("1800-01-01"),
                      end_dt=lubridate::now(),
                      airQuality.fl = "data/data_AQ.rda",
                      meteo.fl = "data/meteorology.rda") {

    require(tidyverse)

    # Create dataSet
    load(airQuality.fl)
    load(meteo.fl)

    return.list <- list()

    for (nm in sites) {
        aq <- data_AQ %>%
                    filter(site == nm,
                           variable %in% variables,
                           date > start_dt,
                           date < end_dt) %>%
                    pivot_wider(names_from = variable,
                                values_from = value)

        aq[is.na(aq)] <- NA

        mto <- data_Mto %>%
                    filter(site == nm,
                        date > start_dt,
                        date < end_dt)


        dat.df <- merge(aq, mto,
                        by = c("date", "site"), all.x=T) %>%
                    mutate_if(is.factor, as.character)

        return.list[[nm]] <- select(dat.df, -site)
    }

    rm(data_AQ)
    rm(data_Mto)

    return.list
}


#
#
# @params:
#     - :
# @return:
#     A
#
# @author Jaimedgp

add.yesterday.meteo <- function(dat.df, n.days=1) {

    if (n.days < 1) {
        return(dat.df)
    }

    if (!("date" %in% names(dat.df))) {
        print("Must have a date column")
        return()
    } else {
        by.col <- c("date")
    }
    if("site" %in% names(dat.df)){
        by.col <- c(by.col, "site")
    }
    if("variable" %in% names(dat.df)){
        by.col <- c(by.col, "variable")
    }
    if("Municipio" %in% names(dat.df)){
        by.col <- c(by.col, "Municipio")
    }

    meteo.cl <- c("ws", "wd", "tmed", "prec", "tmin", "tmax",
                  "presMax", "presMin", "RH", "solar.radiation")
    col.add <- names(dat.df)[which(names(dat.df) %in% meteo.cl)]

    yesterday.df <- dat.df

    for (i in 1:n.days) {
        new.df <- dat.df %>%
                select(all_of(by.col), all_of(col.add)) %>%
                mutate(date=date+i)

        yesterday.df <- merge(yesterday.df, new.df,
                              by = by.col,
                              all.x=F, all.y=F,
                              suffixes = c("", paste("", i, "before", sep="."))
                              )
    }

    return(yesterday.df)
}
