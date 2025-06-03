#!/bin/bash
#
#SBATCH --job-name=pdptw-tuning
#SBATCH --output=logs/out.txt
#SBATCH --error=logs/err.txt
#
#SBATCH --time=24:00:00
#SBATCH --cpus-per-task=40
#SBATCH --mem-per-cpu=5G
#
#SBATCH --mail-user=yannick.molinghen@ulb.be
#SBATCH --mail-type=ALL
#SBATCH --partition=batch
#SBATCH --account=cooplrn
# source the configuration
source /gpfs/projects/shared/p_ariac_cetic/setup.sh
./irace_tuning.sh pdptw dqn all