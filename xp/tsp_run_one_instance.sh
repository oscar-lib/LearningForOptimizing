#!/bin/bash
# solves one TSP instance
# usage: ./script [instance] [bandit] [reward] [timeout]
# example: ./xp/tsp_run_one_instance.sh examples/tsp/tsplib/att48.tsp epsilongreedy r1 5
instance=$1
bandit=$2
reward=$3
timeout=$4

par_string=""
# Results of the irace fine tuning
if [ "$bandit" = "epsilongreedy" ] && [ "$reward" = "r1" ]; then
    par_string=" -lr 0.1409 -mfw 0.5918 -ew 0.3395 -sw 0.0484 -e 0.1061 "
elif [ "$bandit" = "epsilongreedy" ] && [ "$reward" = "r2" ]; then
    par_string=" -lr 0.028 -e 0.0418 "
elif [ "$bandit" = "ucb" ] && [ "$reward" = "r1" ]; then
    par_string=" -lr 0.3817 -mfw 0.9166 -ew 0.7434 -sw 0.5745 -c 0.2618 "
elif [ "$bandit" = "ucb" ] && [ "$reward" = "r2" ]; then
    par_string=" -lr 0.1141 -c 0.8494 "
fi

launch_solver="java -jar ./target/scala-2.13/learningforoptimizing-assembly-0.1.0-SNAPSHOT.jar solveInstance"
output=`$launch_solver --problem tsp --input ${instance} --timeout ${timeout} --bandit ${bandit} --reward ${reward} --verbosity 0 ${par_string}`
# post process to extract only the relevant information
unroutedNodes=$(echo "$output" | grep 'Unrouted nodes' | awk -F': ' '{print $2}')
travelLength=$(echo "$output" | grep 'Tour length' | awk -F': ' '{print $2}')
solOverTime=$(echo "$output" | grep 'solOverTime' | awk -F'=' '{print $2}')
integralPrimalGap=$(echo "$output" | grep 'integralPrimalGap' | awk -F'=' '{print $2}')
# prints the relevant informations
echo "$instance,$bandit,$reward,$timeout,$unroutedNodes,$travelLength,$solOverTime,$integralPrimalGap"
