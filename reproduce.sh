#!/bin/bash

if [ ! -d aggregatesUMD ];
then
  git clone git@github.com:daniele-baccega/aggregatesUMD.git
fi

docker build -t danielebaccega/reproduce-sybil .
docker run -it --user $UID:$UID --rm -v $(pwd):/home/docker/sybil-forecasting danielebaccega/reproduce-sybil