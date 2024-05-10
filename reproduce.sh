#!/bin/bash

docker build -t danielebaccega/reproduce-sybil .
docker run -it --user $UID:$UID -e CONDA_PATH=/opt/conda/etc/profile.d/conda.sh --rm -v $(pwd):/home/docker/sybil-forecasting danielebaccega/reproduce-sybil