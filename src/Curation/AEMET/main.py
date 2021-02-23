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

    station = []
    n_station = Aemet.get_nearest_stations(lat=df_row["latitude"],
                                           long=df_row["longitude"],
                                           near=10)
    n_station["siteAQ"] = df_row["site"]

    for i, st in enumerate(n_station["indicativo"].values):
        all_data = Aemet.get_data(dates=[date(2013, 1, 1),
                                         date(2020, 12, 31)],
                                  station_id=st,
                                  )

        if all_data is not None:
            station.append(i)

            all_data.to_csv(HOME+"data/Curation/AEMET/Values/%s.csv"
                            % (st), index=False)

    return n_station.iloc[station]


# ----------------------------------------
#     ESTACIONES CALIDAD DEL AIRE
# ----------------------------------------

sites_AQ = pd.read_csv(HOME+"data/Curation/AirQuality/checked_sitesAQ.csv")

all_station_info = [download_nearest_data(row)
                    for i, row in sites_AQ.iterrows()]

pd.concat(all_station_info,
          axis=0).to_csv(HOME+"data/Curation/AEMET/sites_AEMET.csv",
                         index=False)
