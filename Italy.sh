#!/bin/bash

docker build -t danielebaccega/reproduce-prophet .
docker run -it --user $UID:$UID --rm -v $(pwd):/home/docker/prophet-forecasting danielebaccega/reproduce-prophet