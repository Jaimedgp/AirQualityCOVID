##
# Script with R functions to filter which sites are more relevant for the
# study by their importance and valid values, as well as which meteo variables
# are more relevant for the study. This script is equivalent of notebook
# "notebooks/nn-sites.ipynb"
#
# @author Jaimedgp
##


filter.row <- function(sites, minValue=0.0){
    new.sites <- c()

    for (i in 1:nrow(sites)) {
        if (TRUE %in% !(sites[i, ] > minValue)) {
            new.sites <- c(new.sites, -i)
        }
    }

    sites[new.sites, ]
}


filter.percent <- function(sites, percent=0.7){
    columns <- 6:ncol(sites)

    for (i in 1:nrow(sites)) {
        sites[i, "numVar"] <- sum(sites[i, columns] >= percent)
    }

    sites
}


filter.order <- function(sites, numVar=7){
    new.sites <- data.frame()

    for (st in levels(as.factor(sites$siteAQ))) {
        new.order <- sites[sites$siteAQ == st &
                     sites$numVar >= numVar, ][order(
                                                sites[sites$siteAQ == st &
                                                sites$numVar == numVar, ]$dist
                                                   ), ]

        if (nrow(new.order) >= 1) {
            new.sites <- rbind(new.sites, new.order[1,])
        }
    }
    new.sites
}


order.all <- function(sites){
    sites[order(-sites$countAQ - sites$countMto),]
}


unique.cities <- function(sites){
    new.sites <- data.frame()
    municipios <- levels(as.factor(sites[, "Municipio"]))

    for (i in 1:length(municipios)) {
        sm.city <- sites[sites$Municipio == municipios[i], ]

        new.sites <- rbind(new.sites,
                           sm.city[which.max(sm.city$countAQ), ]
                          )
    }

    new.sites
}


relevant.sites <- function(sites.fl="../data/csv/nn_sites.csv",
                           cols=1:12,
                           minValue=0.0,
                           percent=0.7,
                           numVar=7,
                           unique.cty=TRUE){

    nn.sites <- read.csv(sites.fl)

    sites.row <- filter.row(sites = nn.sites[, cols],
                            minValue = minValue)
    sites.percent <- filter.percent(sites = sites.row,
                                    percent = percent)
    sites <- filter.order(sites = sites.percent,
                                numVar = numVar)
    if (unique.cty) {
        sites <- unique.cities(sites)
    }

    order.all(sites = sites)
}
