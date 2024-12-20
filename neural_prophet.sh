#!/bin/bash

if [ -z "${CONDA_PATH}" ];
then
  CONDA_PATH=~/miniconda3/etc/profile.d/conda.sh
fi

source $CONDA_PATH
conda activate lstm-gru

python neural_prophet.py --directory $1 --variant $2
