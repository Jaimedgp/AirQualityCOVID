"""
                 AutoAEMET

 Automatizar la descarga de los datos de la AEMET a traves e su API.

 @author Jaimedgp

"""

from datetime import date
from downloadAEMET import DownloadAEMET

HOME = "/home/jaimedgp/Repositories/AirQualityCOVID/"

# Initialize class with my api key
Aemet = DownloadAEMET(apikey=)

# Get stations information and select
sites = Aemet.get_stations()
station_id = sites[sites["provincia"] == "CANTABRIA"]

# Download data
for st in station_id["indicativo"].values:

    all_data = Aemet.get_data(start_dt=date(2020, 12, 1),
                              end_dt=date(2020, 12, 31),
                              station_id=st,
                              # json_name=,
                              csv_name=HOME+"data/AEMET/"+st+".csv",
                              )
