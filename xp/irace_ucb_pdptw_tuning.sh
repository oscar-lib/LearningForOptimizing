#run from top level directory
DIRECTORY=pdptw-arena
if [ ! -d "$DIRECTORY" ]; then
  mkdir $DIRECTORY
fi
sbt clean && sbt assembly
irace --parallel 2 --train-instances-file examples/pdptw/training100.txt