#!/bin/bash
# run the experiments in parallel, using as many CPU cores as available
# supposed to be called at the root of the project

# ------ parameters for the run -------
declare -a BanditType=("dqn-pretrained-csp-300")
timeout=300  # timeout in seconds
nRuns=10   # number of time an instance is run (to take randomness into account)
nParallel=1  # number of parallel run (should be <= number of threads on the machine, but small enough to fit in memory)
run_script="./xp/csp_run_one_instance.sh"  # executable for running the experiments
# path to the file where the instances to run are written
# each line in this file should be the full path to an instance to run
instances="examples/csp/testing-300.txt"

myDate=`printf '%(%Y-%m-%d_%H_%M_%S)T\n' -1`
commitId=`git rev-parse --short HEAD`
outFilename="results/csp_${myDate}_${commitId}_results.csv"  # where the results will be written

# ------ compilation + input preparation -------
# compile the project
echo "compiling..."
#sbt clean
# sbt assembly
echo "compilation done"
echo "running experiments on $nParallel core(s)"
# creates the file so that the header is present
echo "instance,bandit,timeout,objective,solOverTime,integralPrimalGap" > $outFilename


inputFile="csp_parallel_input"  # where the input data will be written for this experiment
if [ -e $inputFile ]; then
  echo "removing previous input file $inputFile"
  rm -f $inputFile
fi
for (( i=1; i<=$nRuns; i++ ))  # one line per solver
do
  for bandit in "${BanditType[@]}"
  do
    # extracts the instances from the data folder
    cat $instances | sed "s/$/,$bandit/"  >> $inputFile
  done
done

# ------ actually run the solver, delay each job by 5 seconds to allow the GPUs to be assigned properly  -------
# --delay 5.0 
cat $inputFile | parallel -j $nParallel --colsep ',' $run_script {1} {2} $timeout >> $outFilename
echo "experiments have been run"
rm -f $inputFile
