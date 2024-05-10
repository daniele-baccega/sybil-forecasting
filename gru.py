import numpy as np
import pandas as pd
import argparse
import random
import os
from sklearn.preprocessing import MinMaxScaler
from tensorflow.random import set_seed
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Input, GRU, Dense


def set_seed_fn(seed):
    random.seed(seed)
    np.random.seed(seed)
    set_seed(seed)
    os.environ['TF_DETERMINISTIC_OPS']  = '1'

def create_sequences(data, n_steps):
    x, y = [], []
    for i in range(len(data) - n_steps):
        x.append(data[i:(i+n_steps), 0])
        y.append(data[i+n_steps, 0])
    return np.array(x), np.array(y)

def forecast_days(model, data, n_days, n_steps):
    last_sequence = data[-n_steps:].reshape(1, n_steps, 1)
    forecast = []
    for _ in range(n_days):
        pred = model.predict(last_sequence)
        forecast.append(pred[0, 0])
        print(last_sequence.shape)
        last_sequence = np.append(last_sequence[:, 1:, :], pred.reshape(1, 1, 1), axis=1)
    return np.array(forecast).reshape(-1, 1)



parser = argparse.ArgumentParser()

parser.add_argument('--directory', type=str, default=".", help='Directory with the data')
parser.add_argument('--variant', type=str, default="all", help='Variant')

args = parser.parse_args()

seed = 76456357
runs = 10

# Load your time series data
filename = args.directory + "gru_data_" + args.variant + ".csv"

data = pd.read_csv(filename)
data.set_index('ds', inplace=True)

# Feature Scaling
scaler = MinMaxScaler(feature_range=(0, 1))
data_scaled = scaler.fit_transform(data)

 # Prepare training sequences
n_steps = 10  # Historical window to look back
x_train, y_train = create_sequences(data_scaled, n_steps)

# Reshape for GRU [samples, timesteps, features]
x_train = x_train.reshape((x_train.shape[0], x_train.shape[1], 1))

forecast_7 = pd.DataFrame(np.zeros(7, dtype=float))
forecast_14 = pd.DataFrame(np.zeros(14, dtype=float))
forecast_21 = pd.DataFrame(np.zeros(21, dtype=float))
forecast_28 = pd.DataFrame(np.zeros(28, dtype=float))
for run in range(runs):
    # Set seed 
    set_seed_fn(seed + run)

    # Build GRU model
    model = Sequential([
        Input(shape=(n_steps, 1,), dtype="float32"),
        GRU(50, activation='relu'),
        Dense(1)
    ])
    model.compile(optimizer='adam', loss='mse')
    model.fit(x_train, y_train, epochs=20, batch_size=1, verbose=1)

    # Forecasting for the desired periods
    forecast_7_local = forecast_days(model, data_scaled, 7, n_steps)
    forecast_14_local = forecast_days(model, data_scaled, 14, n_steps)
    forecast_21_local = forecast_days(model, data_scaled, 21, n_steps)
    forecast_28_local = forecast_days(model, data_scaled, 28, n_steps)

    # Reverse the scaling for interpretation
    forecast_7 = forecast_7 + pd.DataFrame(np.exp(scaler.inverse_transform(forecast_7_local)))
    forecast_14 = forecast_14 + pd.DataFrame(np.exp(scaler.inverse_transform(forecast_14_local)))
    forecast_21 = forecast_21 + pd.DataFrame(np.exp(scaler.inverse_transform(forecast_21_local)))
    forecast_28 = forecast_28 + pd.DataFrame(np.exp(scaler.inverse_transform(forecast_28_local)))

forecast_7 = forecast_7 / runs
forecast_14 = forecast_14 / runs
forecast_21 = forecast_21 / runs
forecast_28 = forecast_28 / runs

forecast_7.to_csv(args.directory + "forecast_7_" + args.variant + ".csv")
forecast_14.to_csv(args.directory + "forecast_14_" + args.variant + ".csv")
forecast_21.to_csv(args.directory + "forecast_21_" + args.variant + ".csv")
forecast_28.to_csv(args.directory + "forecast_28_" + args.variant + ".csv")
