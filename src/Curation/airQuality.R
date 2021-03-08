##############################################################################
#               Air Quality data Curation
#
# Script with the data curation process done for air quality data. The air
#     quality data from urban traffic stations in Spanish cities with more
#     than >100000 inhabitants are studied in order to obtain the effect of
#     COVID-19 lockdown in the air quality.
#
# functions:
#     - get.sites: Get information of the relevant stations.
#     - have.min: Check if the station has enough data fro main study period
#     - get.missing: Get number of missing units of time in data
#     - main.curation: Main function for curation process
#
# @author Jaimedgp
##############################################################################

# load packages
suppressMessages(library(saqgetr))
suppressMessages(library(lubridate))
suppressMessages(library(tidyverse))
suppressMessages(library(openxlsx))
suppressMessages(library(plyr))
suppressMessages(library(openair))

source.file <- "functions.R"
if (!file.exists(source.file)) {
    source.file <- paste("src/", source.file, sep="")
}
source(source.file)


#----------------------------------------------
# date time Conversion from seconds used in
#     some curation functions
#----------------------------------------------
conversion <- list("hour" = 3600,
                   "day" = 3600*24,
                   "week" = 3600*24*7,
                   "month" = (3600*24*365)/12,
                   "year" = 3600*24*365
                   )


get.sites <- function(type, area, start_dt,
                      file.es="data/xlsx/estaciones-CA-JA.xlsx",
                      sheets="ciudades-100000-A") {
    # Get information of the relevant station.
    #
    # @params:
    #     type: Type of air quality station. e.g.: "traffic", "background"
    #     area: Aerea of air quality station. e.g.: "urban", "rural"
    #     start_dt: Minimum date of beginning of data collection of the station
    #     file.es: File with the codes of the stations if the cities
    #     sheets: Important sheet of file.es
    # @return:
    #     dataframe with merge information from file.es and saqgetr


    # xlsx file with information of the spanish cities with > 100000 inhabitants
    sites.100mil <- read.xlsx(file.es, sheet=sheets) %>%
                        select("Municipio", "Población",
                               "Estación.tráfico", "Código.estación")

    # saqgetr sites information and filtration
    spain.sites <- get_saq_sites() %>%
                        filter(country == "spain",
                               site %in% sites.100mil$"Código.estación",
                               site_type == "traffic",
                               site_area == "urban",
                               date_start <= start_dt,
                               ) %>%
                        select(site, site_name, latitude, longitude, elevation,
                               country, site_type, site_area, date_start, date_end)

    merge(x = spain.sites,
          y = sites.100mil,
          by.x = "site", by.y="Código.estación",
          all.x = TRUE)
}


have.min <- function(dataFrame, study.prd, minPercentage=0.8, unit="hour") {
    # Check if the station has enough data for main study period
    #
    # @params:
    #     dataframe: Dataframe with air quality data
    #     study.prd: vector with start and end date of main study period.
    #                e.g.: c(start_dt, end_dt)
    #     minPercentage: minimum amount of data to consider enough data in
    #                    main study period.
    #                           valid_values
    #                       ------------------  > minPercentage
    #                           Total_values
    #     unit: temporal resolution used
    # @return:
    #     Boolean if dataframe has enough data in main study period

    new.df <- dataFrame[dataFrame$date >= study.prd[1] &
                        dataFrame$date <= study.prd[2], ]
    if (nrow(new.df) > 0) {
        new.df <- timeAverage(new.df, avg.time = unit, type=c("variable", "site"))

        # num of hours in main study period
        period <- as.integer(interval(round_date(study.prd[1], unit=unit),
                                    round_date(study.prd[2], unit=unit)
                                    )
                            / conversion[[unit]])

        amount.data <- (sum(!is.na(new.df$value)) / period)

        boolean <- amount.data >= minPercentage
    } else {
        boolean <- FALSE
    }

    boolean

}


get.missing <- function(dataFrame, unit="week",
                        start_dt=ymd("2013-01-01"), end_dt=ymd("2020-12-31")) {
    # Get number of missing units of time in data between study time.
    #
    # @params:
    #     dataframe: Dataframe with air quality data
    #     unit: unit of time
    #     start_dt: start date of study
    #     end_dt: end date of study
    # @return:
    #     Numer of units missing in study time

    new.df <- timeAverage(dataFrame, avg.time=unit, type=c("variable", "site"))

    period <- (interval(round_date(start_dt, unit=unit),
                        round_date(end_dt, unit=unit)
                       )
               / conversion[[unit]])


    as.integer(period) - sum(!is.na(new.df$date))
}


main.curation <- function(pair.st.pll, study.prd, main.prd, minPercentage){
    # Main curation function to execute have.min and get.missing to obtain
    #     the relevant information about the amount of valid data each site has.
    #
    # @params:
    #     pair.st.pll: Vector with pair site code and pollutant
    #                  e.g.: c("es0000a", "no2")
    #     study.prd: Vector with start and end dates of study.
    #                e.g.: c(start_dt, end_dt)
    #     main.prd: Vector with start and end dates of main study period. This
    #               is the period used in have.min(). e.g.: c(start_dt, end_dt)
    #     minPercentage: Parameter use in have.min
    # @return:
    #     dataframe with the relevant information in each column
    #         - site: air quality station code
    #         - Pollutant: pollutant code
    #         - start_yr: date of beginning of data collection of the pollutant
    #                     for each station
    #         - end_yr: date of ending of data collection of the pollutant
    #                   for each station
    #         - hv.min: Boolean if has enough data in main study period
    #         - mss.wk: number of missing weeks
    #         - mss.mnth: number of missing months
    #         - mss.yr: number of missing years

    site <- pair.st.pll[1]
    pll <- pair.st.pll[2]

    dataAQPLL <- get.AQdata(site=site, pollutant=pll,
                            start_dt = study.prd[1], end_dt=study.prd[2],
                            data.by.file=TRUE,
                            fileName="data/Curation/AirQuality/Values/")

    if (nrow(dataAQPLL) > 0) {

        hv.min <- have.min(dataAQPLL, main.prd, minPercentage)

        # missing weeks
        mss.wk <- get.missing(dataAQPLL, unit="week",
                            start_dt = study.prd[1], end_dt=study.prd[2])
        # missing months
        mss.mnth <- get.missing(dataAQPLL, unit="month",
                            start_dt = study.prd[1], end_dt=study.prd[2])
        # missing years
        mss.yr <- get.missing(dataAQPLL, unit="year",
                            start_dt = study.prd[1], end_dt=study.prd[2])

        data.frame(site=site, Pollutant=pll,
                   start_yr=as_date(min(dataAQPLL$date)),
                   end_yr=as_date(max(dataAQPLL$date)),
                   hv.min, mss.wk, mss.mnth, mss.yr)
    }
}


if(sys.nframe() == 0) {

    print("Executing main...")
    setwd("~/Repositories/AirQualityCOVID")

    #--------------------------
    #      Main Variables
    #--------------------------

    site_type <- "traffic"
    site_area <- "urban"

    start_dt <- ymd_hms("2013-01-01 00:00:00")
    end_dt <- ymd_hms("2020-12-30 00:00:00")

    pollutants <- c("no", "no2", "o3", "pm10", "pm2.5")

    #------------------------------
    #      sites Information
    #------------------------------

    sites.AQ <- get.sites(site_type, site_area, start_dt,
                          file.es="data/xlsx/estaciones-CA-JA.xlsx",
                          sheets="ciudades-100000-A")

    #------------------------------
    #      Curation Variables
    #------------------------------

    hv.min.percent <- 0.8 # data > 80%
    main.prd <- c(ymd_hms("2020-03-01 00:00:00"),
                  ymd_hms("2020-05-01 00:00:00"))

    sites.lv <- levels(as.factor(sites.AQ$site))
    pairs.st.pll <- do.call(rbind,
                            do.call(rbind,
                                    lapply(pollutants, function(pll) {
                                                        lapply(sites.lv, c, pll)
                                                     })
                                    )
                            )

    #------------------------------
    #      Curation Process
    #------------------------------

    curate.info <- do.call(rbind.fill,
                           apply(pairs.st.pll, 1, main.curation,
                                 c(start_dt, end_dt), main.prd, hv.min.percent)
                           )

    #-----------------------------------------
    #      Filter Data by parameters
    #
    #        | Parameter | Value |
    #        |-----------|-------|
    #        |   hv.min  | TRUE  |
    #        |   miss.yr | $< 5$ |
    #-----------------------------------------

    valid.info <- curate.info[curate.info$hv.min == TRUE,]
    valid.info <- valid.info[valid.info$mss.yr < 5,]

    checked_sitesAQ <- merge(x = valid.info %>%
                                  select(site, Pollutant),
                             y = sites.AQ,
                             by = "site", all.x = T, all.y=F)

    #------------------------------
    #      Save Curated data
    #------------------------------

    write.csv(valid.info,
              "data/Curation/checked_AQ.csv", row.names=FALSE)

    sv.checkedAQ("data/Curation/checked_AQ.csv")
}
