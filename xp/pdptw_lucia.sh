#!/bin/bash
#
#SBATCH --job-name=pdptw_experiments
#SBATCH --output=logs/pdptw_experiments_%A_%a.out  # stdout log for each array task
#SBATCH --error=logs/pdptw_experiments_%A_%a.err   # stderr log for each array task
#
#SBATCH --time=00:30:00
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=15G
#
# Number of tasks in the array:
# E.g. if your input file has 352 lines:
#SBATCH --array=0-351
#
# If you want to limit how many tasks run concurrently, use e.g. "--array=0-399%20"
# That means "up to 20 tasks at a time, out of the array of 400."

# source the configuration
source /gpfs/projects/shared/p_ariac_cetic/setup.sh

TIMEOUT=900  # Or get it from script env, or put it in the input file

PROJECT_DIR="/gpfs/projects/shared/p_ariac_cetic/LearningForOptimizing"
INPUT_FILE="${PROJECT_DIR}/examples/lucia_input/input_lucia_pdptw.txt"
RUN_SCRIPT="${PROJECT_DIR}/xp/pdptw_run_one_instance.sh"

# Figure out which line from INPUT_FILE this task should run:
LINE_NUM=$((SLURM_ARRAY_TASK_ID + 1))
LINE=$(sed -n "${LINE_NUM}p" "$INPUT_FILE")

# If each line is something like: "instancePath,banditType"
INSTANCE=$(echo "$LINE" | cut -d',' -f1)
BANDIT=$(echo "$LINE"    | cut -d',' -f2)

# "srun" is recommended by Slurm, but effectively this is one process.
# We'll append exactly one line of output to a shared CSV, with a lock to prevent concurrent writes:
srun "$RUN_SCRIPT" "$INSTANCE" "$BANDIT" "$TIMEOUT"

