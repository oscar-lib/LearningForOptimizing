#run from anywhere, but do not move this file
#example run: ./irace_ucb_pdptw_tuning.sh pdptw ucb
PROBLEM=$1 # pdptw, ucb
ALGO=$2    # epsilongreedy, ucb
TRAINING_SIZE=$3
if [ $# -lt 3 ]; then
  echo 1>&2 "$0: not enough parameters: call script with <problem> <algorithm> <training_size>"
  exit 2
fi

REPO_ROOT="$(dirname "$(dirname "$(realpath "$0")")")"
IRACE_DIR=$REPO_ROOT/irace
DATA_DIR=$REPO_ROOT/examples

DATA_DIRECTORY=$ROOT/$PROBLEM-$ALGO-arena
SCENARIO=$IRACE_DIR/scenario-$PROBLEM-$ALGO.txt
TARGET_RUNNER=$IRACE_DIR/target-runner-$PROBLEM

if [ ! -d "$REPO_ROOT/$DATA_DIRECTORY" ]; then
  mkdir "$REPO_ROOT/$DATA_DIRECTORY"
fi

#sbt clean && sbt assembly
irace -s "$SCENARIO" --target-runner "$TARGET_RUNNER" --parallel 4 --train-instances-file "$DATA_DIR"/$PROBLEM/training$TRAINING_SIZE.txt

echo "Root: $REPO_ROOT"