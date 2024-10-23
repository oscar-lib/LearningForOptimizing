# solves one CSP instance
# usage: ./script [instance] [bandit] [timeout]
# example: ./xp/csp_run_one_instance.sh examples/csp/csp_100/sas-documentation.txt epsilongreedy 1
instance=$1
bandit=$2
timeout=$3

par_string=""
# Results of the irace fine tuning
if [ "$bandit" = "epsilongreedy" ]; then
    par_string=" -lr 0.5655 -mfw 0.332 -ew 0.3393 -sw 0.1208 -e 0.2334 "
elif [ "$bandit" = "ucb" ]; then
    par_string=" -lr 0.0973 -mfw 0.3333 -ew 0.0852 -sw 0.0343 -c 2.9671 "
fi

launch_solver="java -jar ./target/scala-2.13/learningforoptimizing-assembly-0.1.0-SNAPSHOT.jar solveInstance"
output=`$launch_solver --problem csp --input ${instance} --timeout ${timeout} --bandit ${bandit} --verbosity 1 ${par_string}`
# post process to extract only the relevant information
objective=$(echo "$output" | grep 'bestObj' | awk -F'=' '{print $2}')
integralPrimalGap=$(echo "$output" | grep 'integralPrimalGap' | awk -F'=' '{print $2}')
# prints the relevant informations
echo "$instance,$bandit,$timeout,$objective,$integralPrimalGap"
