from typing import Dict

import numpy as np
import pandas as pd
import sklearn
from sklearn.datasets import load_breast_cancer

from nam.config import defaults
from nam.data.base import NAMDataset
from nam.data.folded import FoldedDataset

cfg = defaults()

def load_UKload(config=cfg,
                     data_path: str = 'C:/Users/Elhami/Desktop/nam/UKL.csv',
                     features_columns: list = ["wM", "Posan", "Dow","NetDemand.48", "Year" ,"Trend" ,"Holy"],
                     targets_column: str = "NetDemand") -> Dict:

    config.regression = True
    data = pd.read_csv(data_path)
    # data["NetDemand"] = data["NetDemand"] - np.mean(data["NetDemand"])

    # Find the minimum and maximum values of the specific column
    column_name = "NetDemand"
    min_value = data[column_name].min()
    max_value = data[column_name].max()

    # Print the results
    print("Min value:", min_value)
    print("Max value:", max_value)
    print(data.head())
    # Print the first value of the NetDemand column
    first_value = data.loc[0, "NetDemand"]
    print("First value of NetDemand:", first_value)


    if config.cross_val:
        return FoldedDataset(config,
                             data_path=data,
                             features_columns=features_columns,
                             targets_column=targets_column)
    else:
        return NAMDataset(config,
                          data_path=data,
                          features_columns=features_columns,
                          targets_column=targets_column)
