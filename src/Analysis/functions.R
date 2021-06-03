###################################################################
#               General Functions
#
# General functions use in the proyect
#     - relative.change: Calculate the relative change
#     - sep.periods: Mark each day with its corresponding periods
#     - group.municipio: group sites by Municipio
#
# @author Jaimedgp
###################################################################

#' relative.change
#'
#' Calculate the relative change
#'
#' @param a:
#' @param b: reference data
#'
#' @return relative change in percentage
#'
#' @author Jaimedgp
relative.change <- function(a, b) {
    100*(a - b) / b
}


#' sep.periods
#'
#' Mark each day with its corresponding periods
#'
#' @param data: dataframe with date column
#' @param periods: list with periods dates and labels
#'
#' @return dataframe with data columns added periods column
#'          with periods labels
#'
#' @author Jaimedgp
sep.periods <- function(data, periods) {

    change <- data.frame()

    for (i in 2:length(periods)) {
        change <- rbind(change,
                        data %>%
                filter(date >= periods[[i-1]],
                       date <= periods[[i]]
                      ) %>%
                mutate(periods = names(periods)[i-1])) %>%
                relocate(periods, .after = date)
    }

    change
}


#' group.municipio
#'
#' group sites by Municipio
#'
#' @param data: dataframe with sites values. The dataframe must
#'          have the following columns:
#'       - site: with sites codes
#'       - variable: pollutants labesl
#'       - periods: periods labels by date
#'       - mean: values for each site
#' @param filename: path to csv with the municipios information
#'
#' @return dataframe with the mean of each municipio
#'
#' @author Jaimedgp
group.municipio <- function(data.df,
                            filename="data/Curation/checked_AQ.csv") {
    sites.lv <- read.csv(filename)

    merge(data.df,
          sites.lv,
          by = c("site", "variable"), all.x=TRUE) %>%
        group_by(variable, Municipio, periods) %>%
        summarise(mean = mean(mean, na.rm=T),
                  latitude = mean(latitude),
                  longitude = mean(longitude)
                 )
}
