"""
                Meteorology Curation

 Do stuff

 @author Jaimedgp

"""


from datetime import date
from pyaemet import AemetClima

import pandas as pd


if __name__ == '__main__':

    # Define Home directory
    HOME = "/home/jaimedgp/Repositories/AirQualityCOVID/"

    # ------------------------------
    #      Air Quality sites
    # ------------------------------

    sites_AQ = pd.read_csv(HOME+"data/curation/checked_AQ.csv")

    unique_sites_AQ = sites_AQ.groupby(["site",
                                        "latitude",
                                        "longitude"]
                                        ).size().reset_index()

    # ------------------------------
    #      Curation Variables
    # ------------------------------

    start_date, end_date = date(2013, 1, 1), date(2020, 12, 31)
    NUM_STATIONS = 10
    MIN_PROPORTION = 0.8

    folder_vl = HOME+"data/curation/AEMET/"

    selected_cl = ["fecha", "tmed", "prec",
                   "tmin", "tmax", "presMax", "presMin"]

    # ------------------------------
    #      Curation Process
    # ------------------------------

    apikey = ""

    # Initialize AEMET API class with my api key
    aemet = AemetClima(apikey=apikey)
    aemet_sites = pd.DataFrame()

    for i, row in unique_sites_AQ.iterrows():
        nearest_site = aemet.estaciones_curacion(latitud=row["latitude"],
                                                 longitud=row["longitude"],
                                                 n_cercanas=NUM_STATIONS,
                                                 fecha_ini=start_date,
                                                 fecha_end=end_date,
                                                 umbral=MIN_PROPORTION,
                                                 variables=selected_cl,
                                                 save_folder=folder_vl)

        if not isinstance(nearest_site, bool):
            nearest_site["siteAQ"] = row["site"]

            aemet_sites = pd.concat([aemet_sites, nearest_site])
    else:
        aemet_sites.to_csv(HOME+"data/curation/checked_AEMET.csv")
