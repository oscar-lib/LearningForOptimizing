#! /bin/bash

trap "echo 'Interrupted by user'; exit 130" SIGINT
sbt assembly


for seed in {0..10}
do
    echo "Running training with seed $seed"
    java -jar ./target/scala-2.13/learningforoptimizing-assembly-0.1.0-SNAPSHOT.jar solveInstance --input examples/csp/csp_100/16-81.txt --timeout=100 --bandit=dqn --problem=csp --debug --seed=$seed &
    python src/python/main.py -c=unix-socket -i=/tmp/pipes/unix-socket-0 -o=/tmp/pipes/p2s-0 -a=dqn --no-target  --clip=21 --batch-size=163 --lr=0.0001 --device=cuda:0 --disable-training-logs --seed=$seed
done
