# Load packages
suppressMessages(library(lubridate))
suppressMessages(library(tidyverse))
suppressMessages(library(openair))

suppressMessages(library(caret))
suppressMessages(library(BBmisc))

# Paralelizar
suppressMessages(library(doMC))
registerDoMC(cores=14)


open.data <- function(sites) {
    load("data/data_AQ.rda")
    load("data/meteorology.rda")

    aq <- data_AQ %>%
                filter(site %in% sites,
                    date < ymd("2020-01-01")
                    ) %>%
                timeAverage(avg.time = "day", type=c("site", "variable"))
    aq$date <- as_date(aq$date)

    mto <- data_Mto %>%
                filter(site %in% sites,
                    date < ymd("2020-01-01"))

    rm(data_AQ)
    rm(data_Mto)

    merge(aq, mto, by = c("date", "site"), all.x=T) %>%
        drop_na() %>%
        mutate_if(is.factor, as.character) %>%
        select(-date_end, -process, -summary, -validity)
}


deseason <- function(cl) {
    # Remove seasonal component of the additive time serie.
    #     This function is called below by an apply

    dcomp <- decompose(ts(cl, frequency = 365))

    cl - dcomp$seasonal
}


range.df <- function(df, omit.cl) {
    # Scale dataframe ignoring no numeric columns (omit.cl)

    split.df <- df[, -omit.cl]

    cbind(df[, omit.cl],
          normalize(split.df, method = "range",
                    range = c(0, 1), margin = 1))
}


leave.one.year.out <- function(yr, dat, omit.cl) {
    # Do k-iteration of k-fold cv by pop yr data for test
    #     and the rest for training
    # @params: omit.cl: columns to omit in the regression

    test <- which(year(dat$date) == yr)
    train <- which(year(dat$date) != yr)

    if(length(test) == 0) {
        return()
    }
    if (length(train) == 0) {
        return()
    }

    cv.k <- data.frame()

    for (ntree in c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250, 300)) {
        model <- train(value ~.,
                       data=dat[, -omit.cl], subset=train,
                       method="rf", ntree=ntree,
                       allowParallel = TRUE
                      )

        y.th <- predict(model, newdata=dat[test,])

        if (sum(!is.na(dat$date)) > (365*2)) {
            # if there is enough data (more than 2 years), deseasonalized
            ds.dat <- cbind(dat[, omit.cl],
                         apply(dat[, -omit.cl], 2, deseason))
            ds.y.th <- predict(model, newdata=ds.dat[test,])

            cor2 <- cor(ds.y.th, ds.dat[test,]$value)
        } else {cor2 <- NaN}

        cv.k <- rbind(cv.k,
                      data.frame("year"=yr,
                                 "ntree"=ntree,
                                 "bias"=mean(y.th) / mean(dat[test,]$value),
                                 "var.ratio"=var(y.th) / var(dat[test,]$value),
                                 "cor1"=cor(y.th, dat[test,]$value),
                                 "cor2"=cor2,
                                 "RMSE"=sqrt(mean((y.th - dat[test,]$value)^2))
                                ))
    }
    cv.k
}

####################################
##          MAIN PROGRAM
####################################

# Working directory
setwd("~/Repositories/AirQualityCOVID")

sites.lv <- c("es0118a", "es1438a") # Big cities (Madrid and Barcelona)",
sites.lv <- c(sites.lv, "es1580a", "es1340a") # small cities (Santander and Huelva)

data.df <- open.data(sites.lv)


#-------------------------------
#       Cross-validation
#-------------------------------

years <- 2018:2019

no.num.cl <- 1:3 # no numeric columns
cv.df <- data.frame()

t_init <- Sys.time()
i <- 1
# Create one model for each pair of station-pollutant
for (st in sites.lv) {
    print(paste(i, "/", length(sites.lv)))
    data.st <- data.df[data.df$site == st,]
    j <- 1

    for (pll in levels(as.factor(data.st$variable))) {
        print(paste("    ", j, "/", length(levels(as.factor(data.st$variable)))))
        dats <- data.st %>%
                filter(variable == pll) %>%
                range.df(no.num.cl) %>% drop_na()

        cv.row <- do.call(rbind, lapply(years, leave.one.year.out,
                                        dats, no.num.cl))

        cv.df <- rbind(cv.df,
                       cbind(data.frame("variable"=rep(pll, nrow(cv.row)),
                                        "site"=rep(st, nrow(cv.row))),
                             cv.row)
                       )
        j <- j+1
    }
    i <- i+1
}
print(Sys.time() - t_init)


#-----------------------
#    Save all Data
#-----------------------

write.csv(cv.df, "data/Cross-validation/rf-ntree.csv", row.names=F)


#-----------------------
#    Save Plots
#-----------------------

pivot.cv.df <- cv.df %>%
        pivot_longer(cols = c(5:ncol(cv.df)),
                     names_to = "Error", values_to = "Err.Val")
pivot.cv.df$ntree <- as.factor(pivot.cv.df$ntree)


# One figure with Boxplots for each Error
ggplot(data=pivot.cv.df, aes(x=ntree, y=Err.Val, fill=ntree)) +
        geom_boxplot() +
        facet_wrap(~Error, scale="free")

ggsave("rf-CV-ntree.png", plot = last_plot(), device = "png",
       path = "plots/cross-validation/randomForest/",
       width=12, height=8)


# Figure for each pollutant with Boxplots for each Error
for (pll in levels(as.factor(pivot.cv.df$variable))) {

    ggplot(data=pivot.cv.df[pivot.cv.df$variable == pll,],
           aes(x=ntree, y=Err.Val, fill=ntree)) +
        geom_boxplot() +
        facet_wrap(~Error, scale="free")

    ggsave(paste("rf-CV-ntree-", pll, ".png", sep=""),
           plot = last_plot(), device = "png",
           path = "plots/cross-validation/randomForest/byNtree/",
           width=12, height=8)
}



