# run the experiments in parallel, using as many CPU cores as available
# supposed to be called at the root of the project
declare -a BanditType=("epsilonGreedy" "bestSlopeFirst" "bandit" "random" "banditrollingaverage" "banditaftermove")
timeout=300  # timeout in seconds
nRuns=2   # number of time an instance is run (to take randomness into account)
myDate=`printf '%(%Y-%m-%d_%H_%M_%S)T\n' -1`
filename="xp/${myDate}_results.csv"

# first creates the executable to the project
sbt clean
sbt assembly
# creates the file so that the header is present
echo "instance,bandit,timeout,unroutedNodes,nVehicles,travelLength,objective" > $filename
run_script="./xp/run_one_instance.sh"

# loop through the configurations
for bandit in "${BanditType[@]}"
do
  for i in $(seq 1 $nRuns)
  do
    # extract the instances from the data folder and run the solver
    find bench/ -type f | parallel -j 4 $run_script {} $bandit $timeout >> $filename
  done
done
