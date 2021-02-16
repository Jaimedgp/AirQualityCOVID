suppressMessages(library(lubridate))
suppressMessages(library(tidyverse))

setwd("~/Repositories/AirQualityCOVID")
source("src/Analisis/Variacion/functions.R")


data.from.site <- function(site="", municipio="",
                           start_dt, end_dt,
                           pollutant, data.by.file=FALSE) {
    # obtener estaciones en municipio
    if (site == "" && municipio != "") {
        site <- get.site.municipio(municipio)
    }

    # Obtener los datos de calidad del aire
    data.AQ <- get.AQdata(site, pollutant, start_dt, end_dt, data.by.file)
    #head(data.AQ)
    data.AQ
}


download.site <- function(st, dataframe, data.by.file=FALSE) {
    pollutant <- levels(as.factor(dataframe[dataframe$site == st,
                                                    "Pollutant"]
                             ))
    start_dt <- min(dataframe[dataframe$site == st,
                              "start_yr"])
    end_dt <- max(dataframe[dataframe$site == st,
                            "end_yr"])
    get.AQdata(st, pollutant,
            start_dt, end_dt, data.by.file)
}


data.study <- function(data.by.file=FALSE) {
    checked.sites <- read.csv("data/Curation/checked-AQ.csv",
                              stringsAsFactor=FALSE)

    do.call("rbind", lapply(levels(as.factor(checked.sites$site)),
                            download.site,
                            checked.sites, data.by.file))
}


data.by.municipio <- function(dataframe) {
    municipios <- read.csv("data/Curation/sitesAQ.csv",
                           stringsAsFactor=TRUE)

    for (st in levels(municipios$site)) {

        dataframe[dataframe$site == st, "site"] <- municipios[municipios$site == st, "Municipio"]
    }

    new.df <- aggregate(list(value=dataframe$value),
                        by=list(date=dataframe$date,
                                site=dataframe$site,
                                variable=dataframe$variable),
                        mean, na.rm=TRUE, na.action=na.omit)

    new.df
}


#####################################
##           MAIN PROGRAM          ##
#####################################
main <- function(dataFrame, periods) {

    start_dt <- year(min(dataFrame$date))
    end_dt <- year(max(dataFrame$date))

    #------------------------------------
    #  Calculo Propio de Media diaria
    #------------------------------------

    data.AQ.dy <- data.AQ %>%
                    group.dy(unit="day", FUN=mean)

    #------------------------------------
    #  Comparar Datos por Año
    #------------------------------------

    data.all <- compare.years(data.AQ.dy, last.yr=end_dt)

    #-----------------------------------------------------------------------
    #     Variacion de las medias en cada periodo
    #
    # Calcular primero la media en cada intervalo de estudio para cada uno
    #     de los periodos. Despues se calcula la variacion relativa de las
    #     medias en cada intervalo.
    #-----------------------------------------------------------------------

    # Calcular Medias
    columns <- c(end_dt,
                paste(start_dt, "-", end_dt-1,  sep=""))
    medias <- site.mean.in.period(data.all, periods, columns)

    # Calcular variaciones
    var.med <- get.difference(medias, mainCol=paste("mean(",
                                                    end_dt, ")", sep=""),
                                      restCol=c(paste("mean(",
                                                      start_dt, "-",
                                                      end_dt-1, ")", sep="")),
                                      mainSTD=paste("std(",
                                                    end_dt, ")", sep=""),
                                      restSTD=paste("std(",
                                                    start_dt, "-",
                                                    end_dt-1, ")", sep="")
                             )
    # variacion relativa en porcentaje
    var.med[,4:ncol(var.med)] <- var.med[,4:ncol(var.med)]*100

    list(data.all, var.med)
}



if(!interactive()) {

    group.by.municipio <- TRUE

    #-------------------------------------
    # Intervalo de estudio con todos los
    #    anhos en los que se va a comparar
    #-------------------------------------

    start_dt = ymd_hms("2013-01-01 00:00:00")
    end_dt = ymd_hms("2020-12-31 00:00:00")

    #-----------------------------------------------------------------------------
    # Fechas con los inicios o finales de cada uno de los periodos en los que se
    #     van a separar los datos para estudiarlos
    #-----------------------------------------------------------------------------

    periods <- list(
        # Prelockdown
        pre.lckdwn = ymd_hms("2020-01-01 00:00:00"),

        # lockdown
        lckdwn = ymd_hms("2020-03-14 00:00:00"),
        #end.lckdwn = ymd_hms("2020-04-28 00:00:00"),

        # poslockdown
        fases = ymd_hms("2020-05-01 00:00:00"),
        normalidad = ymd_hms("2020-06-21 00:00:00"),

        # 2 lockdown
        new.lockdown = ymd_hms("2020-10-25 00:00:00"),
        end.year = ymd_hms("2020-12-31 00:00:00")
    )

    #-----------------------------------------------------------------------------
    # Datos de calidad del aire para el estudio. Se pueden estudiar casos
    #    particulares dando el codigo de la estacion o el municipio de estudio.
    #    Tambien se permite coger todas las estaciones individuales del estudio
    #    a partir del fichero o estudiar los datos por municipios, haciendo la
    #    media de todas las estaciones de un mismo municipio.
    #-----------------------------------------------------------------------------

    #data.AQ <- data.from.site(site = "es1580a", #c("es1580a", "es0118a"),
    #                          municipio = "", #Santander
    #                          start_dt = start_dt,
    #                          end_dt = end_dt,
    #                          pollutant=c("no2", "no"),
    #                          data.by.file = FALSE)

    data.AQ <- data.study(data.by.file = FALSE)

    if (group.by.municipio) {
        data.AQ <- data.by.municipio(data.AQ)

        folder.name <- "Plots/Analisis/Variacion/by_municipio/"
        file.name <- "data/Analisis/Variacion/variacion_media_municipios.csv"
    } else {
        file.name <- "data/Analisis/Variacion/variacion_media.csv"
        folder.name <- "Plots/Analisis/Variacion/by_site/"
    }

    #-------------------------------------
    # Obtencion de la Variacon de las
    #     medias en cada intervalo
    #-------------------------------------

    compute <- main(data.AQ, periods)

    data.all <- compute[[1]]
    var.med <- compute[[2]]

    #-------------------------------------
    # Guardar datos en archivo csv
    #-------------------------------------

    write.csv(var.med,
              file.name,
              row.names=FALSE)

    #-------------------------------------
    # Representar datos y guardar en
    #     un archivo
    #-------------------------------------

    for (st in levels(as.factor(data.all$site))) {

        names.df <- names(data.all)[(ncol(data.all)-1):ncol(data.all)]

        # Representar los datos de calidad del aire
        plot.AQ <- plot.data(data.all[data.all$site == st, ],
                             names.df,
                             periods, type="aq")

        # Guardar grafica como imagen
        ggsave(filename=paste(st, ".png", sep=""),
               plot=plot.AQ,
               device="png",
               path=folder.name,
               width=20,
               height=10, dpi=100
               )
    }

    #X11() # Para la ejecucion desde terminal (linea de comandos)
    #plot(plot.AQ) # Mostrar grafica por pantalla
}
