#!/bin/bash

#SBATCH --partition=GPU-shared
#SBATCH -t 00:03:00
#SBATCH --gres=gpu:v100-32:1

./Preamble
