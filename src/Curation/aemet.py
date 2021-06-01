"""
                Meteorology Curation

 Do stuff

 @author Jaimedgp

"""


from datetime import date

import pandas as pd

from meteo.AEMET import DownloadAEMET
from meteo.apikey_file import apikey
from meteo.curation import download_nearest_data


# Define Home directory
HOME = "/home/jaimedgp/Repositories/AirQualityCOVID/"
# Initialize AEMET API class with my api key
Aemet = DownloadAEMET(apikey=apikey)

# ------------------------------
#      Air Quality sites
# ------------------------------

sites_AQ = pd.read_csv(HOME+"data/Curation/checked_AQ.csv")

unique_sites_AQ = sites_AQ.groupby(["site",
                                    "latitude",
                                    "longitude"]
                                    ).size().reset_index()

# ------------------------------
#      Curation Variables
# ------------------------------

study_prd = [date(2013, 1, 1), date(2020, 12, 31)]
NUM_STATIONS = 10
MIN_PROPORTION = 0.8

folder_vl = HOME+"data/Curation/AEMET/"

removed_cl = ["sol", "horatmin", "horatmax", "horaracha",
              "dir", "velmedia", "horaPresMax", "horaPresMin"]
selected_cl = ["fecha", "tmed", "prec",
               "tmin", "tmax", "presMax", "presMin"]

# ------------------------------
#      Curation Process
# ------------------------------

all_station_info = [download_nearest_data(aemet=Aemet, siteAQ=row,
                                          stdy_prd=study_prd,
                                          remove_cl=removed_cl,
                                          selected_cl=selected_cl,
                                          folder=folder_vl, n=NUM_STATIONS,
                                          min_prop=MIN_PROPORTION)
                    for i, row in unique_sites_AQ.iterrows()]

# ------------------------------
#      Save Curated data
# ------------------------------

file_st = HOME+"data/Curation/checked_AEMET.csv"

pd.concat(all_station_info,
          axis=0, ignore_index=True).to_csv(file_st, index=False)
