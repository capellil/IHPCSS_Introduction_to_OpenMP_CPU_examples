#!/bin/bash

#SBATCH -p RM-shared
#SBATCH -t 00:00:20
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -o output.txt

./bin/helloworld
