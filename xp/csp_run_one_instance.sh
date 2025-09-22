#!/bin/bash
# solves one CSP instance
# usage: ./script [instance] [bandit] [reward] [timeout]
# example: ./xp/csp_run_one_instance.sh examples/csp/csp_100/sas-documentation.txt epsilongreedy r1 1
instance=$1
bandit=$2
reward=$3
timeout=$4

par_string=""
# Results of the irace fine tuning
if [ "$bandit" = "epsilongreedy" ] && [ "$reward" = "r1" ]; then
    par_string=" -lr 0.0978 -mfw 0.3624 -ew 0.0591 -sw 0.7699 -e 0.5199 "
elif [ "$bandit" = "epsilongreedy" ] && [ "$reward" = "r2" ]; then
    par_string=" -lr 0.8294 -e 0.4052 "
elif [ "$bandit" = "ucb" ] && [ "$reward" = "r1" ]; then
    par_string=" -lr 0.5904 -mfw 0.2387 -ew 0.5773 -sw 0.1954 -c 1.4807 "
elif [ "$bandit" = "ucb" ] && [ "$reward" = "r2" ]; then
    par_string=" -lr 0.1378 -c 4.2808 "
fi

launch_solver="java -jar ./target/scala-2.13/learningforoptimizing-assembly-0.1.0-SNAPSHOT.jar solveInstance"
output=`$launch_solver --problem csp --input ${instance} --timeout ${timeout} --bandit ${bandit} --verbosity 1 --reward ${reward} ${par_string}`
# post process to extract only the relevant information
objective=$(echo "$output" | grep 'bestObj' | awk -F'=' '{print $2}')
solOverTime=$(echo "$output" | grep 'solOverTime' | awk -F'=' '{print $2}')
# prints the relevant informations
echo "$instance,$bandit,$reward,$timeout,$objective,$solOverTime"
