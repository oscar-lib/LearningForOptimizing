#!/bin/bash
# solves one CSP instance
# usage: ./script [instance] [bandit] [reward] [timeout]
# example: ./xp/csp_run_one_instance.sh examples/csp/csp_100/sas-documentation.txt epsilongreedy 1
instance=$1
bandit=$2
reward=$3
timeout=$4

par_string=""
# Results of the irace fine tuning
if [ "$bandit" = "epsilongreedy" ] && [ "$reward" = "r1" ]; then
    par_string=" -lr 0.5655 -mfw 0.332 -ew 0.3393 -sw 0.1208 -e 0.2334 "
elif [ "$bandit" = "epsilongreedy" ] && [ "$reward" = "r2" ]; then
    par_string=" -lr 0.0973 -mfw 0.3333 -ew 0.0852 -sw 0.0343 -c 2.9671 "
elif [ "$bandit" = "ucb" ] && [ "$reward" = "r1" ]; then
    par_string=" -lr 0.0973 -mfw 0.3333 -ew 0.0852 -sw 0.0343 -c 2.9671 "
elif [ "$bandit" = "ucb" ] && [ "$reward" = "r2" ]; then
    par_string=" -lr 0.0973 -mfw 0.3333 -ew 0.0852 -sw 0.0343 -c 2.9671 "
fi

launch_solver="java -jar ./target/scala-2.13/learningforoptimizing-assembly-0.1.0-SNAPSHOT.jar solveInstance"
output=`$launch_solver --problem csp --input ${instance} --timeout ${timeout} --bandit ${bandit} --verbosity 1 --reward ${reward} ${par_string}`
# post process to extract only the relevant information
objective=$(echo "$output" | grep 'bestObj' | awk -F'=' '{print $2}')
solOverTime=$(echo "$output" | grep 'solOverTime' | awk -F'=' '{print $2}')
# prints the relevant informations
echo "$instance,$bandit,$reward,$timeout,$objective,$solOverTime"
