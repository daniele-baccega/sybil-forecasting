# Base image https://hub.docker.com/u/danielebaccega
FROM danielebaccega/sybil
LABEL maintainer="Daniele Baccega <daniele.baccega@unito.it>"

## Copy files
COPY Sybil.R .
COPY common_functions.R .
COPY plots.R .
COPY prepare_data_Coronasurveys.R .
COPY prepare_data_USA.R .
COPY ExampleOnItaly.R .
COPY datasets .

## Run the script
CMD Rscript ExampleOnItaly.R
