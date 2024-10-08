# LearningForOptimizing

This repository contains the code for (some) bandit learning algorithms for local search. 
Those algorithms choose dynamically a neighborhood to use when exploring the search space.

## Compiling the project

You can compile on the command line by using `sbt` with the command:

```
sbt clean
sbt assembly
```

This creates a JAR executable located at `./target/scala-2.13/learningforoptimizing-assembly-X.X.X-SNAPSHOT.jar`
(replace `X-X-X` with the version of the project).

This executable is needed for running the experiments.

## Run experiments

The scripts for running the experiments can be found in the `xp` folder.
They can be run by following the instructions specified in this section, provided that you have the following requirements installed on your machine:

- `sbt`
- java >= 11
- `nohup` if you want to run the experiments overnight

In case of doubt about your configuration, you can always execute the commands from the next subsection to see if the solver runs as expected.

### Running one instance

```
./xp/run_one_instance INSTANCE BANDIT TIMEOUT
```

Where `INSTANCE`, `BANDIT`, `TIMEOUT` are the path to an instance, the name of the bandit algorithm to use and the timeout (in seconds), respectively.
For instance the command 

```
./xp/run_one_instance.sh examples/pdptw/pdptw_100/lc101.txt epsilongreedy 2
```

Gives an output similar to: 

```
examples/pdptw/pdptw_100/lc101.txt,epsilongreedy,2,0,30,3996.112,212000000000
```

Giving the name of the instance, the name of the bandit, the timeout, the number of unrouted nodes, the number of vehicles, the travel length and the objective

### Running a bunch of instances / configurations (for benchmarking purposes)

```
./xp/run_parallel
```

This first creates a file `parallel_input` and uses it to launch the experiments in parallel. 
This file will be automatically deleted by the end of the benchmark: do not touch it!

This command creates a file `YYYY-MM-DD_HH_MM_SS_HASH_results.csv`, 
with `YYYY-MM-DD_HH_MM_SS` corresponding to the date at which the experiment started, and `HASH` to the id of the latest commit.
The content of the file is the run performed.

To modify the settings used in the runs (such as timeout, number of cores, number of runs, ...), go directly within the `run_parallel` and modify the parameters used (starting at line 5).

### Running experiments overnight

You can run the commands specified before for a long time by letting them run in the background. To do so, use the command

```
nohup ./xp/run_parallel &
```

then press `CTRL+C` and the process will continue to run in the background. 
You can achieve the same results by playing with the command `screen` if `nohup` isn't installed on your machine.
