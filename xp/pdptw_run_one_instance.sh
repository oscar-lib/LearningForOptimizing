#!/bin/bash
# solves one instance
# usage: ./script [instance] [bandit] [reward] [timeout]
# example: ./xp/pdptw_run_one_instance.sh examples/pdptw/pdptw_100/lc101.txt epsilongreedy r1 1
instance=$1
bandit=$2
reward=$3
timeout=$4

PREFIX="/gpfs/home/acad/ulb-qsec/yanneke/LearningForOptimizing"

. ${PREFIX}/setup.sh

par_string=""

# Results of the irace fine tuning
if [ "$bandit" = "epsilongreedy" ] && [ "$reward" = "r1" ]; then
    par_string=" -lr 0.5309 -mfw 0.755 -ew 0.606 -sw 0.3539 -e 0.1484 "
elif [ "$bandit" = "epsilongreedy" ] && [ "$reward" = "r2" ]; then
    par_string=" -lr 0.0973 -mfw 0.3333 -ew 0.0852 -sw 0.0343 -c 2.9671 "
elif [ "$bandit" = "ucb" ] && [ "$reward" = "r1" ]; then
    par_string=" -lr 0.6412 -mfw 0.2131 -ew 0.3275 -sw 0.2214 -c 3.723 "
elif [ "$bandit" = "ucb" ] && [ "$reward" = "r2" ]; then
    par_string=" -lr 0.0973 -mfw 0.3333 -ew 0.0852 -sw 0.0343 -c 2.9671 "
elif [ "$bandit" = "dqn" ]; then
  par_string=" --learningRate 0.0167 --epsilon 0.0644 --batchSize 117 --ddqn false --clipping 10.0"
  #par_string=" --learningRate 0.005 --epsilon 0.1 --batchSize 128 --ddqn true --clipping 5.0" # Previous best parameters
else
  echo "usage: ./script [instance] [bandit] [reward] [timeout]"
  echo "example: ./xp/pdptw_run_one_instance.sh examples/pdptw/pdptw_100/lc101.txt epsilongreedy r1 1"
  exit 1
fi


launch_solver="java -jar ${PREFIX}/target/scala-2.13/learningforoptimizing-assembly-0.1.0-SNAPSHOT.jar solveInstance"
output=`$launch_solver --problem pdptw --input ${instance} --timeout ${timeout} --bandit ${bandit} --verbosity 1 --reward ${reward} ${par_string}`
# post process to extract only the relevant information
unroutedNodes=$(echo "$output" | grep 'Unrouted nodes' | awk -F': ' '{print $2}')
nVehicles=$(echo "$output" | grep 'Number of used vehicles' | awk -F': ' '{print $2}')
travelLength=$(echo "$output" | grep 'travelLength' | awk -F': ' '{print $2}')
objective=$(echo "$output" | grep 'bestObj' | awk -F'=' '{print $2}')
integralPrimalGap=$(echo "$output" | grep 'integralPrimalGap' | awk -F'=' '{print $2}')
solOverTime=$(echo "$output" | grep 'solOverTime' | awk -F'=' '{print $2}')
# prints the relevant informations
echo "$instance,$bandit,$reward,$timeout,$unroutedNodes,$nVehicles,$travelLength,$objective,$integralPrimalGap,$solOverTime"

