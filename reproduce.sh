#!/bin/bash

docker build -t danielebaccega/reproduce-sybil .
docker run -it --user $UID:$UID --rm -v $(pwd):/home/docker/sybil-forecasting danielebaccega/reproduce-sybil