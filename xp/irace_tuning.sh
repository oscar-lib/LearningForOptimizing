#run from anywhere, but do not move this file
#example run: ./irace_tuning.sh pdptw ucb
export PATH="$(Rscript -e "cat(paste0(system.file(package='irace', 'bin', mustWork=TRUE), ':'))" 2> /dev/null)${PATH}"
PROBLEM=pdptw # pdptw, csp
ALGO=dqn    # epsilongreedy, ucb
TRAINING_SIZE=all # 100, 200, ..., all
# if [ $# -lt 3 ]; then
#   echo 1>&2 "$0: not enough parameters: call script with <problem> <algorithm> <training_size>"
#   exit 2
# fi

. /gpfs/home/acad/ulb-qsec/yanneke/LearningForOptimizing/setup.sh

REPO_ROOT="$(dirname "$(dirname "$(realpath "$0")")")"
IRACE_DIR=$REPO_ROOT/irace
DATA_DIR=$REPO_ROOT/examples

DATA_DIRECTORY=$ROOT$PROBLEM-$ALGO-arena
SCENARIO=$IRACE_DIR/scenario-$PROBLEM-$ALGO.txt
TARGET_RUNNER=$IRACE_DIR/target-runner-$PROBLEM

if [ ! -d "$REPO_ROOT/$DATA_DIRECTORY" ]; then
  mkdir "$REPO_ROOT/$DATA_DIRECTORY"
fi

#copy .csv file if not there
CSV="$REPO_ROOT"/"$DATA_DIRECTORY"/bks/"$PROBLEM"_bks.csv
if [ ! -f "$CSV" ]; then
#  echo 1>&2 "$0: $CSV not found, copying from $REPO_ROOT/bks/"$PROBLEM"_bks.csv"
  if [ ! -d "$REPO_ROOT/$DATA_DIRECTORY"/bks ]; then
    mkdir "$REPO_ROOT/$DATA_DIRECTORY"/bks
  fi
  cp "$REPO_ROOT"/bks/"$PROBLEM"_bks.csv "$CSV"
fi

# sbt clean
# sbt assembly
irace -s "$SCENARIO" --target-runner "$TARGET_RUNNER" --parallel 20 --train-instances-file "$DATA_DIR"/$PROBLEM/training$TRAINING_SIZE.txt
