# solves one instance
# usage: ./script [instance] [bandit] [timeout]
# example: ./xp/run_one_instance.sh bench/LC1_2_1.txt epsilongreedy 1
params=$*
root="$(dirname "$(dirname "$(realpath "$0")")")"
launch_solver="java -jar $root/target/scala-2.13/learningforoptimizing-assembly-0.1.0-SNAPSHOT.jar solveInstance"
output=`$launch_solver ${params}`
# post process to extract only the relevant information
unroutedNodes=$(echo "$output" | grep 'Unrouted nodes' | awk -F': ' '{print $2}')
nVehicles=$(echo "$output" | grep 'Number of used vehicles' | awk -F': ' '{print $2}')
travelLength=$(echo "$output" | grep 'Total route length' | awk -F': ' '{print $2}')
objective=$(echo "$output" | grep 'bestObj' | awk -F'=' '{print $2}')
integralPrimalGap=$(echo "$output" | grep 'integralPrimalGap' | awk -F'=' '{print $2}')
# prints the relevant informations
#echo "$instance,$bandit,$timeout,$unroutedNodes,$nVehicles,$travelLength,$objective"
echo "$objective"
