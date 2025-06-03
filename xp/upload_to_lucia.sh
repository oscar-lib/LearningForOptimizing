#! /bin/bash

LUCIA_USERNAME="yanneke"
LUCIA_HOST="lucia"
FILE_SRC="./target/scala-2.13/learningforoptimizing-assembly-0.1.0-SNAPSHOT.jar"
FILE_DST="/gpfs/projects/shared/p_ariac_cetic/LearningForOptimizing/${FILE_SRC}"
scp ${FILE_SRC} ${LUCIA_HOST}:${FILE_DST}