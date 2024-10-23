# solves one instance
# usage: ./script [instance] [bandit] [timeout]
# example: ./xp/pdptw_run_one_instance.sh examples/pdptw/pdptw_100/lc101.txt epsilongreedy 1
instance=$1
bandit=$2
timeout=$3

par_string=""

if [ "$bandit" = "ucb" ]; then
  par_string="-c 1.3439 -sw 0.9358 -ew 0.9836 -mfw 0.1684 -lr 0.9155"
elif [ "$bandit" = "epsilongreedy" ]; then
  par_string="-lr 0.5048 -mfw 0.3134 -ew 0.2538 -sw 0.6768 -e 0.0524"
elif [ "$bandit" = "dqn" ]; then
  par_string=" --learningRate 0.005 --epsilon 0.1 --batchSize 128 --ddqn true --clipping 5.0"
else
  echo "usage: ./script [instance] [bandit] [timeout]"
  echo "example: ./xp/pdptw_run_one_instance.sh examples/pdptw/pdptw_100/lc101.txt epsilongreedy 1"
  exit 1
fi






launch_solver="java -jar ./target/scala-2.13/learningforoptimizing-assembly-0.1.0-SNAPSHOT.jar solveInstance"
output=`$launch_solver --problem pdptw --input ${instance} --timeout ${timeout} --bandit ${bandit} --verbosity 1 ${par_string}`
# post process to extract only the relevant information
unroutedNodes=$(echo "$output" | grep 'Unrouted nodes' | awk -F': ' '{print $2}')
nVehicles=$(echo "$output" | grep 'Number of used vehicles' | awk -F': ' '{print $2}')
travelLength=$(echo "$output" | grep 'travelLength' | awk -F': ' '{print $2}')
objective=$(echo "$output" | grep 'bestObj' | awk -F'=' '{print $2}')
integralPrimalGap=$(echo "$output" | grep 'integralPrimalGap' | awk -F'=' '{print $2}')
solOverTime=$(echo "$output" | grep 'solOverTime' | awk -F'=' '{print $2}')
# prints the relevant informations
echo "$instance,$bandit,$timeout,$unroutedNodes,$nVehicles,$travelLength,$objective,$integralPrimalGap,$solOverTime"

