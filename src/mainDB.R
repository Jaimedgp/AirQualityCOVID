source("./get_sites_info.R")

save.all <- TRUE

sitesAQ <- get.sitesAQ.info(file="../data/xlsx/estaciones-CA-JA.xlsx",
                            sheet="ciudades-100000-A",
                            site_type = "traffic",
                            pollutants = c("no", "no2", "o3", "pm10"),
                            start_dt = ymd_hms("2010-01-01 00:00:00"),
                            end_dt = ymd_hms("2020-10-01 00:00:00"),
                            save=save.all
                           )

sitesMto <- get.sitesMto.info(nearest=3, save=save.all)


dataAQ <- get.countData.AQ(sitesAQ.fl = "../data/csv/sitesAQ.csv",
                           dataAQ.fl = "../data/csv/dataAQ.csv",
                           final.fl = "../data/csv/final_sites.csv",

                           pollutants = c("no", "no2", "o3", "pm10"), # contaminantes a estudiar
                           start_dt = ymd_hms("2015-01-01 00:00:00"), # fechas de inicio de toma de datos
                           end_dt = ymd_hms("2020-12-31 00:00:00"), # fechas de final de toma de datos
                           lckdwn_strt = ymd_hms("2020-03-14 00:00:00"), # fecha de inicio de confinamiento

                           save.data = save.all
                          )
