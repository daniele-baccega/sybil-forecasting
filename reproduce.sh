#!/bin/bash

unzip datasets/USA_2024-06-13.zip -d datasets
unzip datasets/USA_variants.zip -d datasets

docker build -t danielebaccega/reproduce-sybil .
docker run -it --user $UID:$UID --rm -v $(pwd):/home/docker/sybil-forecasting danielebaccega/reproduce-sybil