suppressMessages(library(lubridate))
suppressMessages(library(tidyverse))
suppressMessages(library(ggplot2))
suppressMessages(library(segmented))

setwd("~/Repositories/AirQualityCOVID/")

source("src/Analisis/Variacion/functions.R")
source("src/general.R")

municipio = "Santander"
site = "" # c("es1580a", "es0118a")

data.by.file = TRUE

pollutant = c("no2")#, "no", "o3", "pm10", "pm2.5")
start_dt = 2013

periods <- list(
    # lockdown
    lockdown = as.numeric(ymd_hms("2020-03-14 00:00:00")),
    #end.lockdown = as.numeric(ymd_hms("2020-04-28 00:00:00")),

    # poslockdown
    fases = as.numeric(ymd_hms("2020-05-01 00:00:00")),
    normalidad = as.numeric(ymd_hms("2020-06-21 00:00:00")),

    # New lockdown
    new.lockdown = as.numeric(ymd_hms("2020-10-25 00:00:00"))
)

if (site == "" && municipio != "") {
    site <- get.site.municipio(municipio,
                               fileName="data/Curation/sitesAQ.csv")
}

data.AQ <- get.AQdata(site, pollutant, start_dt, end_dt=2020, data.by.file)

data.AQ.dy <- data.AQ %>%
                group.dy(unit="day", FUN=mean)

data.all <- compare.years(data.AQ.dy, last.yr=2020)

# Calcular variaciones
var.med <- get.difference(data.all, "2020",c(paste(start_dt, "-2019", sep="")))
var.med[,4:ncol(var.med)] <- var.med[,4:ncol(var.med)]*100

fil.1 <- filter.by.std(var.med, c("2013-2019.vs.2020"), 5)

var <- data.frame(y.1=fil.1$"2013-2019.vs.2020",
                  x=as.numeric(fil.1$date)
                 )

my.seg.1 <- segmented(lm(y.1 ~ x + 1, data=var),
                      seg.Z = ~ x,
                      psi = list(x = periods)
                     )

plot.AQ <- ggplot(data=var, aes(x=as_datetime(x)))+
    scale_color_manual(name = "Years",
                       values = c("2019" = "blue"))

plot.AQ <- plot.AQ + geom_line(aes(y=y.1, color="2019"), size=0.2)

if (length(my.seg.1$psi) != 0) {
    for (k in 1:(nrow(my.seg.1$psi))) {
        plot.AQ <- plot.AQ + geom_vline(xintercept = my.seg.1$psi[k,"Est."],
                                  linetype=5, color = "red", size=1.5)
        plot.AQ <- plot.AQ + geom_vline(xintercept = my.seg.1$psi[k,"Est."] - my.seg.1$psi[k,"St.Err"],
                                  linetype=5, color = "orange", size=1)
        plot.AQ <- plot.AQ + geom_vline(xintercept = my.seg.1$psi[k,"Est."] + my.seg.1$psi[k,"St.Err"],
                                  linetype=5, color = "orange", size=1)
    }
}

for (k in 1:(length(periods))) {
    plot.AQ <- plot.AQ + geom_vline(xintercept = as.numeric(periods[k]),
                              linetype=4, color = "black", size=1)
}

ggsave(filename=paste("Test.png", sep=""),
        plot=plot.AQ,
        device="png",
        path="./",
        width=20,
        height=10, dpi=100
        )
