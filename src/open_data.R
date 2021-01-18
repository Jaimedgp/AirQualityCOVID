library(readxl)
library(tidyverse)
library(saqgetr)
library(lubridate)

open.sites <- function(file="../data/csv/sitesAQ.csv"){
    if (file.exists(file)) {
            sites <- read.csv(file)
    } else {
        print("Something went wrong")
        sites <- 0
    }
    sites
}


open.data <- function(file="../data/csv/dataAQV.csv", numSites=5) {

    sites <- levels(as.factor(open.sites()$site))

    sites <- sites[1:numSites]

    if (file.exists(file)) {
        dataAQV <- read.csv(file)

        # Convert date to datetime format
        dataAQV$date <- ymd_hms(dataAQV$date)

        dataAQV <- dataAQV[dataAQV$site %in% sites, ]
    } else {
        print("Something went wrong")
        dataAQV <- 0
    }
    dataAQV
}



extract.data <- function(
                         sites,
                         pollutants = c("no", "no2", "o3", "pm10"),
                         start_dt = ymd_hms("2010-01-01 00:00:00"),
                         end_dt = ymd_hms("2020-10-01 00:00:00")
                        ){

    dataAQV <- get_saq_observations(
        site = sites,
        variable = pollutants,
        valid_only = TRUE,
        start = start_dt,
        end = end_dt,
        verbose = TRUE
    )
}
