
suppressMessages(library(lubridate))
suppressMessages(library(tidyverse))

source("./functions.R")

#X11() # Para la ejecucion desde terminal (linea de comandos)

#---------------------------------------------------------------
# Directorios de los archivos de los que leer/escribir datos
#---------------------------------------------------------------

# Archivo con las estaciones y sus municipios
sites.fl <- "../../data/csv/sitesAQ.csv"

# Carpeta con los datos de calidad del aire
data.AQ.fl <- "../data/csv/dataAQ/"

# Carpeta donde guardar los datos en tablas
data.fl <- "../../data/csv/variaciones/"

# Carpeta donde guardar las graficas
plot.fl <- "../../Plots/"


#-----------------------------------------------------------------------------
#                         PARAMETROS
#
# Parametros para la obtencion de los datos de calidad del aire.
# Si no se pasa ningun codigo de estacion pero se para un municipio
#     se obtienen las estaciones de trafico urbano del municipio a partir
#     de la tabla "../data/csv/sitesAQ.csv
# Si data.by.file= TRUE y existe el archivo fileName/esXXXXa.csv
#     con los datos de la estacion esXXXXa, los datos son leidos del archivo,
#     En caso contrario se descargan los datos
# start <- dt define desde que anho se obtienen los datos
#-----------------------------------------------------------------------------

municipio = "" #Santander
site = "es1580a"#c("es1580a", "es0118a")

data.by.file = FALSE

pollutant = c("no2")#, "no", "o3", "pm10", "pm2.5")
start_dt = 2015

#-----------------------------------------------------------------------------
# Fechas con los inicios o finales de cada uno de los periodos en los que se
#     van a separar los datos para estudiarlos
#-----------------------------------------------------------------------------

periods <- list(
    # Prelockdown
    pre.lckdwn = ymd_hms("2020-01-01 00:00:00"),

    # lockdown
    lckdwn = ymd_hms("2020-03-14 00:00:00"),
    end.lckdwn = ymd_hms("2020-04-28 00:00:00"),

    # poslockdown
    start.pos.lckdwn = ymd_hms("2020-05-01 00:00:00"),
    end.pos.lckdwn = ymd_hms("2020-06-21 00:00:00"),

    # 2 lockdown
    start.2.lckdwn = ymd_hms("2020-10-25 00:00:00"),
    end.2.lckdwn = ymd_hms("2020-12-31 00:00:00")
)

#####################################
##           MAIN PROGRAM          ##
#####################################

# obtener estaciones en municipio
if (site == "" && municipio != "") {
    site <- get.site.municipio(municipio)
}

# Obtener los datos de calidad del aire
data.AQ <- get.AQdata(site, pollutant, start_dt, data.by.file)
#head(data.AQ)


#------------------------------------
#  Calculo Propio de Media diaria
#------------------------------------

data.AQ.dy <- data.AQ %>%
                group.dy(unit="day", FUN=mean)

#------------------------------------
#  Comparar Datos por Año
#------------------------------------

data.all <- compare.years(data.AQ.dy, last.yr=2020)


#-----------------------------------------------------------------------
#     Variacion de las medias en cada periodo
#
# Calcular primero la media en cada intervalo de estudio para cada uno
#     de los periodos. Despues se calcula la variacion relativa de las
#     medias en cada intervalo.
#-----------------------------------------------------------------------

# Calcular Medias
columns <- c("2020",
             "2019",
             paste(start_dt, "-2019", sep=""))
medias <- mean.in.period(data.all, periods, columns)

# Calcular variaciones
var.med <- get.difference(medias, mainCol="mean(2020)",
                                  restCol=c("mean(2019)",
                                            paste("mean(",
                                                  start_dt,
                                                  "-2019)", sep="")),
                                  mainSTD="std(2020)",
                                  restSTD=c("std(2019)",
                                            paste("std(",
                                                  start_dt,
                                                  "-2019)", sep=""))
                         )
# variacion relativa en porcentaje
var.med[,4:ncol(var.med)] <- var.med[,4:ncol(var.med)]*100

# Guardar datos en fichero
#write.csv(var.med,
#          paste(data.fl,
#                "variacion_media.csv",
#                sep=""),
#          row.names=FALSE)

# Mostrar valores segun estacion, contaminante e intervalo
var.med[var.med$site == site[1] &
        var.med$variable == pollutant[1] &
        var.med$period %in% names(periods)
       ,]


#-----------------------------------------------------------------------
#     Media de las variaciones en cada periodo
#
# Calcular primero la variacion relativa para cada uno de los periodos
#     en cada intervalo de estudio. Despues se calcula la media de las
#     variacion relativa en cada intervalo.
#-----------------------------------------------------------------------

# Calcular variaciones
var.med <- get.difference(data.all, mainCol="2020",
                                    restCol=c("2019",
                                              paste(start_dt, "-2019", sep=""))
                         )
# variacion relativa en porcentaje
var.med[,4:ncol(var.med)] <- var.med[,4:ncol(var.med)]*100


# Calcular Medias
columns <- c("2019.vs.2020",
             paste(start_dt, "-2019.vs.2020", sep=""))
medias <- mean.in.period(var.med, periods, columns)

# Guardar datos en fichero
#write.csv(medias,
#          paste(data.fl,
#                "media_variacion.csv",
#                sep=""),
#          row.names=FALSE)

# Mostrar valores segun estacion, contaminante e intervalo
medias[medias$site == site[1] &
        medias$variable == pollutant[1] &
        medias$period %in% names(periods)
       ,]


################################
#   Representar Resultados
################################

#---------------------------------
#    Variacion Relativa
#---------------------------------

# Representar las variaciones relativas diarias
plot.change <- plot.data(var.med,
          c("2019.vs.2020",
            paste(start_dt,
                  "-2019.vs.2020", sep="")),
          periods, type="var")

# Guardar grafica como imagen
#ggsave(filename="cambio_relativo.png",
#       plot=plot.change,
#       device="png",
#       path=plot.fl)


#---------------------------------
#    Series TEmporales
#---------------------------------

# Representar los datos de calidad del aire
plot.AQ <- plot.data(data.all,
          c("2020",
            "2019",
            paste(start_dt, "-2019", sep="")),
          periods, type="aq")

# Guardar grafica como imagen
#ggsave(filename="comparacion_series_temporales.png",
#       plot=plot.AQ,
#       device="png",
#       path=plot.fl)


#plot(plot.AQ) # Mostrar grafica por pantalla
