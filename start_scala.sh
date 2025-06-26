#! /bin/bash

trap "echo 'Interrupted by user'; exit 130" SIGINT
# sbt assembly


while IFS= read -r line; do
    if [[ $line == examples/csp/csp_100/carseq_100_8_20* ]]; then
        echo "Processing ${line}" | tee -a "training.log"
        java -jar ./target/scala-2.13/learningforoptimizing-assembly-0.1.0-SNAPSHOT.jar solveInstance \
            --input="${line}" \
            --problem=csp  \
            --timeout=300 \
            --bandit=dqn \
            --debug \
            --accept=all >> "training.log"
    fi
done < examples/csp/trainingall.txt
