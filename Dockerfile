# Base image https://hub.docker.com/u/danielebaccega
FROM danielebaccega/prophet-forecasting

## Copy files
COPY forecasting_Prophet.R /home/docker/prophet-forecasting/forecasting_Prophet.R
COPY common_functions.R /home/docker/prophet-forecasting/common_functions.R

## Run the script
CMD Rscript /home/docker/prophet-forecasting/forecasting_Prophet.R