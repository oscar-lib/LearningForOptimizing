#! /bin/bash

trap "echo 'Interrupted by user'; exit 130" SIGINT
sbt assembly

sleep 1h 30m
python run_experiments.py