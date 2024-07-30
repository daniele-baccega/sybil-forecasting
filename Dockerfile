# Base image https://hub.docker.com/u/danielebaccega
FROM danielebaccega/sybil
LABEL maintainer="Daniele Baccega <daniele.baccega@unito.it>"

## Copy files
COPY Sybil.R .
COPY common_functions.R .
COPY plots.R .
COPY prepare_data_Italy_VaccinationFixedRecoveryRateAndCoronasurveys.R .
COPY ExampleOnItaly_VaccinationFixedRecoveryRateAndCoronasurveys.R .
COPY NeuralProphet.sh .
COPY NeuralProphet.py .
COPY datasets .

## Run the script
CMD Rscript ExampleOnItaly_VaccinationFixedRecoveryRateAndCoronasurveys.R