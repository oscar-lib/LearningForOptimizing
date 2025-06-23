#! /bin/bash

trap "echo 'Interrupted by user'; exit 130" SIGINT

while IFS= read -r line; do
    if [[ $line == csp_100* ]]; then
        echo "Processing $line"
        java -jar ./target/scala-2.13/learningforoptimizing-assembly-0.1.0-SNAPSHOT.jar \
            solveInstance \
            --input="${line}" \
            --problem=csp
    fi
done < examples/csp/trainingall.txt

