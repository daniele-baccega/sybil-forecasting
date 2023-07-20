#!/bin/bash

docker build -t danielebaccega/reproduce-prophet .
docker run -it --user 1001:1001 -v $(pwd):/home/docker/prophet-forecasting danielebaccega/reproduce-prophet