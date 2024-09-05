# solves one CSP instance
# usage: ./script [instance] [bandit] [timeout]
# example: ./xp/csp_run_one_instance.sh examples/csp/csp_100/sas-documentation.txt epsilongreedy 1
instance=$1
bandit=$2
timeout=$3
launch_solver="java -jar ./target/scala-2.13/learningforoptimizing-assembly-0.1.0-SNAPSHOT.jar solveInstance"
output=`$launch_solver --problem csp --input ${instance} --timeout ${timeout} --bandit ${bandit} --verbosity 0`
# post process to extract only the relevant information
objective=$(echo "$output" | grep 'bestObj' | awk -F'=' '{print $2}')
integralPrimalGap=$(echo "$output" | grep 'integralPrimalGap' | awk -F'=' '{print $2}')
# prints the relevant informations
echo "$instance,$bandit,$timeout,$objective,$integralPrimalGap"
