"""
                 AutoAEMET

 Automatizar la descarga de los datos de la AEMET a traves e su API.

 @author Jaimedgp

"""

from datetime import date

import pandas as pd

from downloadAEMET import DownloadAEMET
from apikey_file import apikey

HOME = "/home/jaimedgp/Repositories/AirQualityCOVID/"

# ----------------------------------------
#     ESTACIONES AEMET
# ----------------------------------------

# Initialize class with my api key
Aemet = DownloadAEMET(apikey=apikey)


def download_nearest_data(df_row):
    """ Download AEMET meteo data from the nearest station to the air quality
        station.  """

    nearest_station = Aemet.get_nearest_stations(lat=df_row["latitude"],
                                                 long=df_row["longitude"],
                                                 near=3)

    for i, (st, lat, long) in enumerate(
                                    zip(nearest_station["indicativo"].values,
                                        nearest_station["latitud"].values,
                                        nearest_station["longitud"].values)):

        all_data = Aemet.get_data(dates=[date(2013, 1, 1),
                                         date(2020, 12, 31)],
                                  station_id=st,
                                  )

        if all_data is not None:
            all_data["siteAQ"] = df_row["site"]
            all_data["latitud"] = lat
            all_data["longitud"] = long

            all_data.to_csv(HOME+"data/Curation/AEMET/Values/%s-%s.csv"
                            %(df_row["site"], i), index=False)


# ----------------------------------------
#     ESTACIONES CALIDAD DEL AIRE
# ----------------------------------------

sites_AQ = pd.read_csv(HOME+"data/Curation/AirQuality/sitesAQ.csv")

sites_AQ.apply(download_nearest_data, axis=1)
