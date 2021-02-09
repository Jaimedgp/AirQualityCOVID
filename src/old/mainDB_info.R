##
# Script with R function to create a table with the relevant information of
# each station (AQ and meteo) with the n-n relation between each. This script
# is equivalent of notebooks "notebooks/mainDB_info.ipynb"
# and "notebooks/sitesMto.ipynb"
#
# @author Jaimedgp
##
source("./get_sites_info.R")
source("./count_data.R")
source("./funciones.R")

save.all <- TRUE

sitesAQ <- get.sitesAQ.info(file="../data/xlsx/estaciones-CA-JA.xlsx",
                            sheet="ciudades-100000-A",
                            site_type = "traffic",
                            pollutants = c("no", "no2", "o3", "pm10", "pm2.5"),
                            start_dt = ymd_hms("2010-01-01 00:00:00"),
                            end_dt = ymd_hms("2020-12-31 00:00:00"),
                            save=save.all
                           )
print("sitesAQ Done!")

sitesMto <- get.sitesMto.info(nearest=3, save=save.all)
print("sitesMto Done!")


nn.sitesAQ <- get.countData.AQ(sitesAQ.fl = "../data/csv/sitesAQ.csv",
                               dataAQ.fl = "../data/csv/dataAQ.csv",

                               pollutants = c("no", "no2", "o3", "pm10", "pm2.5"), # contaminantes
                               start_dt = ymd_hms("2010-01-01 00:00:00"), # fecha inicio
                               end_dt = ymd_hms("2020-12-31 00:00:00"), # fecha final

                               save.data = save.all
                              )
print("nn.sitesAQ Done!")

nn.sitesMto <- get.countData.Mto(sitesMto.fl = "../data/csv/sitesMto.csv",
                                 dataMto.fl = "../data/csv/dataMto.csv",

                                 years = 2010:2020,
                                 save.data = save.all
                                )
print("nn.sitesMto Done!")

nn.sites <- get.nnSites(nn.sitesMto, nn.sitesAQ,
                        final.fl = "../data/csv/nn_sites.csv"
                       )
print("All Done!")
