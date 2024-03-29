"""
    Clustering

    @author Jaimedgp

"""

import pandas as pd
import numpy as np

from tslearn.clustering import TimeSeriesKMeans
from tslearn.utils import to_time_series_dataset


def min_max_scaler(data):
    """ Re-scale data to [0, 1] interval """

    return (data - data.min()) / (data.max() - data.min())


def replace_nan(time_serie):
    """ Replace NaN with interpolation """
    time_serie.interpolate(limit_direction="both", inplace=True)

    return time_serie


def open_data(file="data/results/predictions_municipios.csv"):
    """ Open file and pivot table """

    data = pd.read_csv(file).drop(["pred", "obs"], axis=1)

    data["date"] = pd.to_datetime(data["date"])

    return pd.pivot_table(data,
                          columns=["Municipio"],
                          index=["date"]
                          )


def filter_municipio(data, municipios):
    """ Drop some cities """

    return data.drop([("diff", i) for i in municipios], axis=1)


if __name__ == '__main__':
    HOME = "AirQualityCOVID/"

    data_df = open_data(HOME+"data/results/predictions_municipios.csv")

    data_df = min_max_scaler(data_df)
    data_df = filter_municipio(data_df, "Marbella")

    data_df = data_df.apply(replace_nan).droplevel(None, axis=1)

    serie = to_time_series_dataset(
        data_df.T.to_numpy().reshape(data_df.shape[1], data_df.shape[0], 1))

    model = TimeSeriesKMeans(n_clusters=4,
                             metric="euclidean",
                             verbose=False,
                             max_iter_barycenter=5,
                             n_jobs=6,
                             random_state=13
                             )

    result = model.fit_predict(serie)

    groups = pd.DataFrame()

    for i, s in enumerate(model.cluster_centers_):

        grp_sites = data_df.columns[np.where(result == i)]
        print(grp_sites)
        groups = pd.concat([groups,
                            pd.DataFrame({"group": i+1,
                                          "Municipio": grp_sites})])

    groups.to_csv(HOME+"data/clustering/groups.csv", index=False)
