#!/bin/bash

if [ -z "${CONDA_PATH}" ];
then
  CONDA_PATH=~/miniconda/etc/profile.d/conda.sh
fi

source $CONDA_PATH
conda activate lstm-gru

python NeuralProphet.py --directory $1 --suffix $2