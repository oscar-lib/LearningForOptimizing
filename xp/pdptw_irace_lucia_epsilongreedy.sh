#!/bin/bash
#
#SBATCH --job-name=irace_tuning_pdptw_e
#SBATCH --output=logs/irace_tuning_pdptw_epsilongreedy.out  # stdout log for the task
#SBATCH --error=logs/irace_tuning_pdptw_epsilongreedy.err   # stderr log for the task
#
#SBATCH --ntasks=1
#SBATCH --time=12:00:00
#SBATCH --cpus-per-task=15
#SBATCH --mem-per-cpu=16G

PROBLEM="pdptw"
ALGO="epsilongreedy"
TRAINING_SIZE="all"

# source the configuration
source /gpfs/projects/shared/p_ariac_cetic/setup.sh

PROJECT_DIR="/gpfs/projects/shared/p_ariac_cetic/LearningForOptimizing"

IRACE_DIR=$PROJECT_DIR/irace
DATA_DIR=$PROJECT_DIR/examples

SCENARIO=$IRACE_DIR/scenario-$PROBLEM-$ALGO.txt
TARGET_RUNNER=$IRACE_DIR/target-runner-$PROBLEM
TRAIN_INSTANCES="$DATA_DIR"/$PROBLEM/training$TRAINING_SIZE.txt

srun irace --parallel 15 -s "$SCENARIO" --target-runner "$TARGET_RUNNER" --train-instances-file "$TRAIN_INSTANCES"

