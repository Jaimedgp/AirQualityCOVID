#version copia para seguir haciendo pruebas de descarga de datos meteo y calidad del aire y gráficos
#script para conseguir datos de calidad del aire y meteo de estaciones de España
#Primero habría que instalar los siguientes paquetes si no lo estuvieran instalados
# packages <- c("tidyverse", "saqgetr", "worldmet", "openair")
# Los cargo con library

library(tidyverse)
library(saqgetr)
library(worldmet)
library(openair)

#obtener datos de CA de España. Salen los códigos de las estaciones de Calidad de aire (941)

spain_sites <- get_saq_sites() %>%
  filter(country == "spain")
DT::datatable(spain_sites, 
              options = list(scrollX = TRUE))

#se puede usar el código de cada estación o bien intentar sacarlo de un mapa
#la opción de sacarlo del mapa la salto porque no funciona bien

#para saber que datos estan disponibles en cada estación
processes <- get_saq_processes()
#por ejemplo en cros (es1577a)
processes %>%
  filter(site %in% "es1577a") %>%
  DT::datatable(options = list(scrollX = TRUE))

#por ejemplo en varias estaciones: cros (es1577a) y guarnizo (es1576a)
processes %>%
  filter(site %in% c("es1577a", "es1576a")) %>%
  DT::datatable(options = list(scrollX = TRUE))
#podemos descargar datos de un año concreto de la estacion que queramos
# guarnizo y cros del 2019
guarcros <- get_saq_observations(
  site = c("es1576a", "es1577a"),
  valid_only = TRUE,
  start = 2019,
  end = 2019,
  verbose = TRUE
)

#descargo datos de calidad del aire en cros en los últimos años, desde 2001 al 2020; luego se
#ve que no hay datos de 2001 y 2002; solo a partir de 2003
crostodo <- get_saq_observations(
  site = "es1577a",
  valid_only = TRUE,
  start = 2001,
  end = 2020,
  verbose = TRUE
)

#descargo juntos datos de calidad del aire en cros y guarnizo en los últimos años, desde 2001 al 2020
guarcrostodo <- get_saq_observations(
  site = c("es1576a", "es1577a"),
  valid_only = TRUE,
  start = 2001,
  end = 2020,
  verbose = TRUE
)

#si queremos ver como estan organizados los datos
glimpse(guarcros)

#si queremos que esten organizados de una forma más legible para manipular despues con mas funciones

guarcros_clean <- saq_clean_observations(guarcros, 
                                      summary = "hour", 
                                      valid_only = TRUE, 
                                      spread = TRUE)

guarcrostodo_clean <- saq_clean_observations(guarcrostodo, 
                                         summary = "hour", 
                                         valid_only = TRUE, 
                                         spread = TRUE)

crostodo_clean <- saq_clean_observations(crostodo, 
                                             summary = "hour", 
                                             valid_only = TRUE, 
                                             spread = TRUE)

glimpse(guarcros_clean)
glimpse(guarcrostodo_clean)

#ahora grafico un resumen para Cros y Guarnizo juntos

summaryPlot(guarcros_clean,site="cros")
summaryPlot(guarcrostodo_clean,site="cros")

#podemos disponer de medias mensuales en vez de horarias
# calculate monthly average
guarcros_month <- timeAverage(guarcros_clean,
                           avg.time = "month", type = "site")

guarcrostodo_month <- timeAverage(guarcrostodo_clean,
                              avg.time = "month", type = "site")

head(guarcros_month)
head(guarcrostodo_month)

#podemos graficar los datos mensuales de las estaciones

summaryPlot(guarcros_clean,period = "months", type = "density")
summaryPlot(guarcrostodo_clean,period = "months", type = "density")

#podemos graficar los datos mensuales de cada estación, por ejemplo para Guarnizo

summaryPlot(guarcros_clean,period = "months", site="guar",type = "density")
summaryPlot(guarcrostodo_clean,period = "months",site="guar",type = "density")

#podemos visualizar la secuencia temporal de los contaminantes, por ejemplo para CROS
#secuencia temporal con timeplot
timePlot(crostodo_clean,avg.time = "month",pollutant="pm10")
timePlot(crostodo_clean,avg.time = "day",pollutant="pm10")
timePlot(crostodo_clean,avg.time = "year",pollutant="pm10")
timePlot(crostodo_clean,avg.time = "year",pollutant="o3")
timePlot(crostodo_clean,avg.time = "month",pollutant="so2")
timePlot(selectByDate(crostodo_clean,year=2006,month="feb"),avg.time = "day",pollutant="so2")
timePlot(selectByDate(crostodo_clean,year=2020,month="feb"),avg.time = "day",pollutant="so2")
timePlot(selectByDate(crostodo_clean,year=2006,month="feb"),avg.time = "day",pollutant="pm10")
timePlot(selectByDate(crostodo_clean,year=2020),avg.time = "day",pollutant="pm10")
timePlot(selectByDate(crostodo_clean,year=2020),avg.time = "month",pollutant="pm10")
timePlot(selectByDate(crostodo_clean,year=2006,month="feb",day=13),avg.time = "hour",pollutant="so2")

#analizar disminución NO2 en marzo 2020 despues de la entrada del estado de alerta, horario y diario
timePlot(selectByDate(crostodo_clean,year=2020,month="mar"),avg.time = "day",pollutant="no2")
timePlot(selectByDate(crostodo_clean,year=2020,month="mar"),avg.time = "day",pollutant="no")
timePlot(selectByDate(crostodo_clean,year=2020,month="mar"),avg.time = "hour",pollutant="no2")

#localizar episodio del 5 de marzo de 2015 en Ferroatlantica, corte de corriente duante dos horas desde las 9:41
timePlot(selectByDate(crostodo_clean,year=2015,month="mar",day=5),avg.time = "hour",pollutant="nox")
timePlot(selectByDate(crostodo_clean,year=2015,month="mar",day=5),avg.time = "hour",pollutant="pm10")

#otro episodio en 2017
timePlot(selectByDate(crostodo_clean,year=2017,month="nov",day=16),avg.time = "hour",pollutant="co")
timePlot(selectByDate(crostodo_clean,year=2017,month="nov",day=16),avg.time = "hour",pollutant="pm10")


#OBTENER DATOS METEO DE ESTACIONES AEMET
#Se pueden localizar las estaciones meteo mas cercanas a una estacion de CA mediante mapa

getMeta(lat = 43, lon = -3, returnMap = TRUE)
#descargo los datos meteo de STR-CMT de 2018
std_met = importNOAA(code = "080230-99999", year = 2018)
#descargo los datos meteo de Parayas de 2018
parayas_met = importNOAA(code = "080210-99999", year = 2018)

#descargo los datos meteo de Parayas desde 2001 a 2020
parayas_mettodo = importNOAA(code = "080210-99999", year=2001:2020)

#hago una rosa de vientos para comprobar
windRose(std_met,angle=10,type="season")
windRose(parayas_mettodo,type="season")
windRose(parayas_mettodo,type="month")

#rosa de los últimos 20 años en parayas es casi identica
windRose(parayas_mettodo,type="year")
windRose(parayas_mettodo,type="season")

#JUNTAMOS DATOS DE CALIDAD DEL AIRE CON METEO
#juntamos datos de CA de Cros y Guarnizo y meteo de parayas

guarcros_todo <- guarcros_clean %>%
  left_join(parayas_met[c("date", "wd", "ws", "air_temp")], by = "date")

#juntamos los datos de CA de Cros y meteo entre 2001 y 2020
crostodo_todo <- crostodo_clean %>%
  left_join(parayas_mettodo[c("date", "wd", "ws", "air_temp")], by = "date")

#hago un gráfico polar de CO en cros de 2019
polarPlot(guarcros_todo,site="cros",pollutant="co")

#hago un gráfico polar de CO en cros de los últimos 19 años
polarPlot(crostodo_todo,pollutant="co")
polarPlot(crostodo_todo,pollutant="co",type="year")
polarPlot(crostodo_todo,pollutant="pm10")
polarPlot(crostodo_todo,pollutant="pm10",type="year")
polarPlot(selectByDate(crostodo_todo,year=2020),pollutant="co")
polarPlot(selectByDate(crostodo_todo,year=2003),pollutant="pm10")
polarPlot(selectByDate(crostodo_todo,year=2019),pollutant="pm10")
polarPlot(selectByDate(crostodo_todo,year=2020),pollutant="pm10")

#hago un gráfico polar de CO en cros y guarnizo
polarPlot(guarcros_todo,type="site",pollutant="co")
#hago un gráfico polar pero de probabilidades de superar un percentil
#el CO parece un buen trazador de ferroatlantica viendo las rosas de cocentracion y de probabilidad
polarPlot(guarcros_todo,type="site",pollutant="co",statistic="cpf",percentile=99)
#En CROS
polarPlot(crostodo_todo,type="year",pollutant="co",statistic="cpf",percentile=99)
polarPlot(selectByDate(crostodo_todo,year=2017),pollutant="co",statistic="cpf",percentile=99)
polarPlot(selectByDate(crostodo_todo,year=2017),pollutant="co",statistic="cpf",percentile=c(90,99))
polarPlot(selectByDate(crostodo_todo,year=2019),pollutant="co",statistic="cpf",percentile=c(75,99))
polarPlot(selectByDate(crostodo_todo,year=2005),pollutant="co",statistic="cpf",percentile=98)
          