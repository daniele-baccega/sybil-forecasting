import numpy as np
import pandas as pd
import argparse
from neuralprophet import NeuralProphet, set_random_seed


def set_seed_fn(seed):
    np.random.seed(seed)
    set_random_seed(seed)

parser = argparse.ArgumentParser()

parser.add_argument('--directory', type=str, default=".", help='Directory with the data')
parser.add_argument('--variant', type=str, default="all", help='Variant')

args = parser.parse_args()

seed = 76456357
runs = 10

# Load your time series data
filename = args.directory + "neural_prophet_data_" + args.variant + ".csv"

data = pd.read_csv(filename)

forecast_7 = pd.DataFrame(np.zeros(7, dtype=float))
forecast_14 = pd.DataFrame(np.zeros(14, dtype=float))
forecast_21 = pd.DataFrame(np.zeros(21, dtype=float))
forecast_28 = pd.DataFrame(np.zeros(28, dtype=float))
for run in range(runs):
    # Set seed 
    set_seed_fn(seed + run)

    m = NeuralProphet(growth = "linear")

    m.fit(data)

    df_future_7 = m.make_future_dataframe(data, periods=7)
    df_future_14 = m.make_future_dataframe(data, periods=14)
    df_future_21 = m.make_future_dataframe(data, periods=21)
    df_future_28 = m.make_future_dataframe(data, periods=28)

    forecast_7_local = m.predict(df_future_7)
    forecast_14_local = m.predict(df_future_14)
    forecast_21_local = m.predict(df_future_21)
    forecast_28_local = m.predict(df_future_28)

    forecast_7 = forecast_7 + pd.DataFrame(np.array(np.exp(forecast_7_local.loc[:, "yhat1"])))
    forecast_14 = forecast_14 + pd.DataFrame(np.array(np.exp(forecast_14_local.loc[:, "yhat1"])))
    forecast_21 = forecast_21 + pd.DataFrame(np.array(np.exp(forecast_21_local.loc[:, "yhat1"])))
    forecast_28 = forecast_28 + pd.DataFrame(np.array(np.exp(forecast_28_local.loc[:, "yhat1"])))

forecast_7 = forecast_7 / runs
forecast_14 = forecast_14 / runs
forecast_21 = forecast_21 / runs
forecast_28 = forecast_28 / runs

forecast_7.to_csv(args.directory + "forecast_7_" + args.variant + ".csv")
forecast_14.to_csv(args.directory + "forecast_14_" + args.variant + ".csv")
forecast_21.to_csv(args.directory + "forecast_21_" + args.variant + ".csv")
forecast_28.to_csv(args.directory + "forecast_28_" + args.variant + ".csv")