
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
                                values_from = value) %>%
                    openair::timeAverage(avg.time = "day",
                                        type="site") %>%
                    mutate(date = lubridate::as_date(date)) %>%
                    select(-date_end, -process, -summary, -validity)

        aq[is.na(aq)] <- NA

        mto <- data_Mto %>%
                    filter(site == nm,
                        date > start_dt,
                        date < end_dt) %>%
                    select(-atmos_pres)


        dat.df <- merge(aq, mto,
                        by = c("date", "site"), all.x=T) %>%
                    mutate_if(is.factor, as.character)

        return.list[[nm]] <- select(dat.df, -site)
    }

    rm(data_AQ)
    rm(data_Mto)

    return.list
}



