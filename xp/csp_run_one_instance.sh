#!/bin/bash
# solves one CSP instance
# usage: ./script [instance] [bandit] [reward] [timeout]
# example: ./xp/csp_run_one_instance.sh examples/csp/csp_100/sas-documentation.txt epsilongreedy r1 1
instance=$1
bandit=$2
reward=$3
timeout=$4

par_string=""
bandit_param="$bandit"
# Results of the irace fine tuning
if [ "$bandit" = "epsilongreedy" ] && [ "$reward" = "r1" ]; then
    par_string=" -lr 0.0479 -mfw 0.7958 -ew 0.3777 -sw 0.4522 -e 0.8495 "
elif [ "$bandit" = "epsilongreedy" ] && [ "$reward" = "r2" ]; then
    par_string=" -lr 0.7446 -e 0.2997 "
elif [ "$bandit" = "ucb" ] && [ "$reward" = "r1" ]; then
    par_string=" -lr 0.2879 -mfw 0.8656 -ew 0.344 -sw 0.0752 -c 3.1753 "
elif [ "$bandit" = "ucb" ] && [ "$reward" = "r2" ]; then
    par_string=" -lr 0.8245 -c 3.5174 "
elif [ "$bandit" = "dqn" ]; then
    par_string=" --learningRate 0.005 --epsilon 0.1 --batchSize 128 --ddqn true --clipping 5.0"
elif [ "$bandit" = "dqn-pretrained-csp-100" ]; then
  bandit_param="dqn"
  par_string=" --epsilon 0.0 --noTrain --accept=strict-improvement --loadFrom=saved_dqn_csp_100"
elif [ "$bandit" = "dqn-pretrained-csp-200" ]; then
  bandit_param="dqn"
  par_string=" --epsilon 0.0 --noTrain --accept=strict-improvement --loadFrom=saved_dqn_csp_200"
elif [ "$bandit" = "dqn-pretrained-csp-300" ]; then
  bandit_param="dqn"
  par_string=" --epsilon 0.0 --noTrain --accept=strict-improvement --loadFrom=saved_dqn_csp_300 --device=cuda:0"
else
    echo "Unknown bandit type: $bandit"
    exit 1
fi

launch_solver="java -jar ./target/scala-2.13/learningforoptimizing-assembly-0.1.0-SNAPSHOT.jar solveInstance"
output=`$launch_solver --problem csp --input ${instance} --timeout ${timeout} --bandit ${bandit} --verbosity 1 --reward ${reward} ${par_string}`
# post process to extract only the relevant information
objective=$(echo "$output" | grep 'bestObj' | awk -F'=' '{print $2}')
solOverTime=$(echo "$output" | grep 'solOverTime' | awk -F'=' '{print $2}')
integralPrimalGap=$(echo "$output" | grep 'integralPrimalGap' | awk -F'=' '{print $2}')
# prints the relevant informations
echo "$instance,$bandit,$reward,$timeout,$objective,$solOverTime"
