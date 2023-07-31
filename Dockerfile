# Base image https://hub.docker.com/u/danielebaccega
FROM danielebaccega/prophet
LABEL maintainer="Daniele Baccega <daniele.baccega@unito.it>"

## Copy files
COPY forecasting_Prophet.R .
COPY common_functions.R .

## Run the script
CMD Rscript forecasting_Prophet.R