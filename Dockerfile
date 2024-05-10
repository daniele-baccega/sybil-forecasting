# Base image https://hub.docker.com/u/danielebaccega
FROM danielebaccega/sybil
LABEL maintainer="Daniele Baccega <daniele.baccega@unito.it>"

## Copy files
COPY reproduce.R .
COPY Sybil.R .
COPY common_functions.R .
COPY plots.R .
COPY plots_ascending_scenarios.R .
COPY neural_prophet.sh .
COPY neural_prophet.py .
COPY lstm.sh .
COPY lstm.py .
COPY gru.sh .
COPY gru.py .

## Run the script
CMD Rscript reproduce.R