"""
                 AutoAEMET

 Automatizar la descarga de los datos de la AEMET a traves e su API.

 @author Jaimedgp

"""

import tempfile as tmpfl
import json
import requests
import pandas as pd
import numpy as np

from dateutil.relativedelta import relativedelta


def calc_dist(pos1, pos2, radius=3958.75):
    """ Calculate distance in latitudes and longitudes """

    pos1 = np.deg2rad(pos1.values)
    pos2 = np.deg2rad(pos2.values)

    dist = radius * np.arccos(np.cos(pos1[0] - pos2[0]) -
                              np.cos(pos1[0]) *
                              np.cos(pos2[0]) *
                              (1 - np.cos(pos1[1] - pos2[1])))

    return dist


def convert_long(longitude):
    """ Convert AEMET longitude notation into float number

        E -> +       |    W -> -
    """

    signo = {"E": 1, "W": -1}

    for i in range(0, longitude.shape[0]):
        orientation = longitude.iloc[i][-1]
        grados = float(longitude.iloc[i][:2])
        minutes = float(longitude.iloc[i][2:4]) / 60
        seconds = float(longitude.iloc[i][4:6]) / 3600

        longitude.iloc[i] = signo[orientation]*(grados+minutes+seconds)

    return longitude


def convert_lat(latitude):
    """ Convert AEMET latitude notation into float number

        N -> +       |    S -> -
    """

    signo = {"N": 1, "S": -1}

    for i in range(0, latitude.shape[0]):
        orientation = latitude.iloc[i][-1]
        grados = float(latitude.iloc[i][:2])
        minutes = float(latitude.iloc[i][2:4]) / 60
        seconds = float(latitude.iloc[i][4:6]) / 3600

        latitude.iloc[i] = signo[orientation]*(grados+minutes+seconds)

    return latitude


def dot_decimals(data_coma):
    """ Replace coma notation for decimals to dot """

    tmp_fl = tmpfl.NamedTemporaryFile().name

    data_coma.to_csv(tmp_fl, sep=";", index=False)
    data_dots = pd.read_csv(tmp_fl, sep=";", decimal=",")

    return data_dots


def split_date(start_dt, end_dt):
    """
        Check if interval between start_dt and end_dt is bigger
        than 5 years, and if so, divide it in interval of less
        than 5 years
    """

    if relativedelta(end_dt, start_dt).years < 5:
        dates = [(start_dt, end_dt)]
    else:
        new_dt = end_dt.replace(year=end_dt.year - 4)
        new_end = end_dt
        dates = [(new_dt, new_end)]

        while relativedelta(new_dt, start_dt).years > 5:
            dates.append((new_dt, new_end))

            new_end = new_dt
            new_dt = new_dt.replace(year=end_dt.year - 4)

        dates.append((start_dt, new_dt))

    return dates


def save_csv(dataframe, file_name):
    """Save Json into a file"""

    dataframe.to_csv(file_name, index=False)


def save_json(json_fl, file_name):
    """Save Json into a file"""

    if file_name is None:
        file_name = tmpfl.NamedTemporaryFile().name

    with open(file_name, "w") as outfile:
        json.dump(json_fl, outfile)

    return file_name


class DownloadAEMET():
    """ Class to download AEMET information"""

    def __init__(self, apikey):
        """ Get the needed API"""

        self.api = "?api_key=" + apikey
        self.main_url = "https://opendata.aemet.es/opendata/api"
        self.clima_url = "/valores/climatologicos/"

        self.stations = None

    def get_nearest_stations(self, lat, long, near=3):
        """
            Obtener las estaciones mas cercanas a una posicion (lat, long) o
            aquellas que se encuentren en la provincia
        """

        if self.stations is None:
            self.stations = self.get_stations()

        self.stations["dist"] = calc_dist([self.stations["latitud"].values,
                                           self.stations["longitud"].values],
                                          [lat, long])

        return self.stations.sort_values(by=['dist'], ascending=True)[:near]

    def get_stations(self, file_name=None):
        """ Inventario de estaciones"""

        to_obtain = requests.get((self.main_url +
                                  self.clima_url +
                                  "inventarioestaciones/todasestaciones/" +
                                  self.api
                                  ), {'accept': 'application/json'}).json()

        sites = requests.get(to_obtain["datos"],
                             {'accept': 'application/json'}).json()

        file = save_json(sites, file_name)
        self.stations = pd.read_json(file)
        self.stations["latitud"] = convert_lat(self.stations["latitud"].copy())
        self.stations["longitud"] = convert_long(self.stations["longitud"].copy())

        return self.stations

    def get_data(self, start_dt, end_dt, station_id,
                 json_name=None, csv_name=None):
        """                Obtain clima data

            Aemet include same values that have to be replace to make the
            dataset readable for everyone.

            - Replace coma '0,0' decimals to dot '0.0'
            - Replace strings in numerical columns by a numeric key

            | Codigo |         significado         | Nuevo Valor  |
            |:------:|:---------------------------:|:------------:|
            |   Ip   | prec < 0.1mm (Inapreciable) |      0       |
            | Varios |         Varias horas        |     -2       |
        """

        split_dt = split_date(start_dt, end_dt)
        data = []

        for i, j in split_dt:

            to_obtain = requests.get((self.main_url +
                                      self.clima_url +
                                      "diarios/datos/" +
                                      "fechaini/" + str(i) + "T00:00:00UTC/" +
                                      "fechafin/" + str(j) + "T23:59:59UTC/" +
                                      "estacion/" + station_id + "/" +
                                      self.api
                                      ), {'accept': 'application/json'}
                                     ).json()

            print(to_obtain['descripcion'])
            if to_obtain['descripcion'] == "exito":
                data_json = requests.get(to_obtain["datos"],
                                         {'accept': 'application/json'}).json()
                data += data_json
            else:
                return None

        file = save_json(data, json_name)
        data_pd = pd.read_json(file).replace({"Ip": 0,
                                              "Varias": -2})

        dataframe = dot_decimals(data_pd)

        if csv_name is not None:
            save_csv(dataframe, csv_name)

        return dataframe
