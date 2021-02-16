##############################################################################
##                     LEER DATOS DE VARIACION
##
## Script para leer los datos de los cambios relativos en la media de cada
##     periodo. Los datos de todas las estaciones y contaminantes se
##     encuentran en formato csv y en este script se filtran para mostrar
##     los datos deseados filtrando por estacion, contamiante y periodo.
##     Para facilitar la busqueda se accede a la informacion de las estaciones
##     para mostrar sitios por municipios.
##
## Tambien se muestran los datos por municipios obtenidos de la media de todas
##     las estaciones del municipio.
##
## @author Jaimedgp
##
##############################################################################

# Directorio en el que se encuentran los archivos
setwd("~/Repositories/AirQualityCOVID/data/Analisis/Variacion/")


por.estacion <- function(estaciones) {
    # Funcion para obtener la infomacion de la estacion

    # informacion de las estaciones
    sitesAQ <- read.csv("../../Curation/sitesAQ.csv")

    list(estaciones=estaciones,
         nombres=sitesAQ[sitesAQ$site %in% estaciones,
                         "site_name"],
         municipios=sitesAQ[sitesAQ$site %in% estaciones,
                            "Municipio"])
}


por.nombre <- function(nombres) {
    # Funcion para obtener la infomacion de la estacion

    # informacion de las estaciones
    sitesAQ <- read.csv("../../Curation/sitesAQ.csv")

    list(estaciones=sitesAQ[sitesAQ$site_name %in% nombres,
                         "site"],
         nombres=nombres,
         municipios=sitesAQ[sitesAQ$site_name %in% nombres,
                            "Municipio"])
}


por.municipio <- function(municipios) {
    # Funcion para obtener la infomacion de la estacion

    # informacion de las estaciones
    sitesAQ <- read.csv("../../Curation/sitesAQ.csv")

    list(estaciones=sitesAQ[sitesAQ$Municipio %in% municipios,
                            "site"],
         nombres=sitesAQ[sitesAQ$Municipio %in% municipios,
                         "site_name"],
         municipios=rep(municipios,
                        length(sitesAQ[sitesAQ$Municipio %in% municipios,
                         "site_name"])))
}

# Leer datos de archivos
valores <- read.csv("variacion_media.csv")
valores.municipios <- read.csv("variacion_media_municipios.csv")

#-----------------------------
# Datos del filtrado
#-----------------------------

contaminantes <- c("no2")
periodos <- c("pre.lckdwn",
              "lckdwn",
              "fases",
              "normalidad",
              "new.lockdown"
             )


#-----------------------------------------------------------------
#                       ESTACIONES
#
# Se puede obtener las estaciones por su nombre, por su codigo
#     o escoger todas las de un mismo municipio
#-----------------------------------------------------------------

#estaciones <- por.estacion(c("es0118a", "es0115a"))
#estaciones <- por.nombre(c("ESCUELAS AGUIRRE"))
estaciones <- por.municipio(c("Madrid"))

for (i in 1:length(estaciones$estaciones)) {
    print(paste(estaciones$municipios[i],
                estaciones$nombres[i],
                estaciones$estaciones[i], sep=" | "))
}

print(valores[valores$site %in% estaciones$estaciones &
              valores$variable %in% contaminantes &
              valores$period %in% periodos,])
