# Base image https://hub.docker.com/u/danielebaccega
FROM danielebaccega/sybil
LABEL maintainer="Daniele Baccega <daniele.baccega@unito.it>"

## Copy files
COPY reproduce.R .
COPY Sybil.R .
COPY common_functions.R .
COPY plots.R .
COPY datasets .

## Run the script
CMD Rscript reproduce.R