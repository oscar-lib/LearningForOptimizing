#! /bin/bash

trap "echo 'Interrupted by user'; exit 130" SIGINT
# sbt assembly


while IFS= read -r line; do
    # if [[ $line == examples/csp/csp_100/carseq_100_8_20* ]]; then
        echo "Training for $line"
        python src/python/main.py \
            -c=pipe \
            -i=pipes/s2p-0 \
            -o=pipes/p2s-0 \
            -a=dqn \
            --clip=2 \
            --ddqn=true \
            --batch-size=32 \
            --lr=0.0005 \
            --device=auto \
            --save-to="${line}.model" &
        sleep 1
        java -jar ./target/scala-2.13/learningforoptimizing-assembly-0.1.0-SNAPSHOT.jar solveInstance \
            --input="${line}" \
            --problem=csp  \
            --timeout=300 \
            --bandit=dqn \
            --debug \
            --accept=all >> "training.log"

        
        echo "Test without training for $line"
        python src/python/main.py \
            -c=pipe \
            -i=pipes/s2p-0 \
            -o=pipes/p2s-0 \
            -a=dqn \
            --clip=2 \
            --ddqn=true \
            --batch-size=32 \
            --lr=0.0005 \
            --device=auto \
            --load-from="${line}.model" \
            --no-train &
        sleep 1
        java -jar ./target/scala-2.13/learningforoptimizing-assembly-0.1.0-SNAPSHOT.jar solveInstance \
            --input="${line}" \
            --problem=csp  \
            --timeout=300 \
            --bandit=dqn \
            --debug \
            --accept=strict-improvement >> "test.log"

        echo "Test with training for $line"
        python src/python/main.py \
            -c=pipe \
            -i=pipes/s2p-0 \
            -o=pipes/p2s-0 \
            -a=dqn \
            --clip=2 \
            --ddqn=true \
            --batch-size=32 \
            --lr=0.0005 \
            --device=auto \
            --load-from="${line}.model" &
        sleep 1
        java -jar ./target/scala-2.13/learningforoptimizing-assembly-0.1.0-SNAPSHOT.jar solveInstance \
            --input="${line}" \
            --problem=csp  \
            --timeout=300 \
            --bandit=dqn \
            --debug \
            --accept=strict-improvement >> "test_with_training.log"
        exit
    # fi
done < examples/csp/trainingall.txt
