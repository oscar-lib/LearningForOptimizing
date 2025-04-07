#!/bin/bash
# Submission script for Lucia
#SBATCH --time=1-00:00:00 # days-hh:mm:ss
#
#SBATCH --ntasks=1
#SBATCH --gres="gpu:4"
#SBATCH --mem-per-cpu=1000 # megabytes
#SBATCH --partition=gpu
#
#SBATCH --mail-user=yannick.molinghen@ulb.be
#SBATCH --mail-type=ALL
#
#SBATCH --account=cooplrn

module purge
module load LIST_THE_MODULES_YOU_NEED_HERE 