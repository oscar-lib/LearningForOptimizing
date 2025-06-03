# miniconda, used for R installation
export PATH="/gpfs/projects/shared/p_ariac_cetic/miniconda/bin:$PATH"
# load Conda shell functions for non-interactive shells
source /gpfs/projects/shared/p_ariac_cetic/miniconda/etc/profile.d/conda.sh
# irace installed through miniconda
export IRACE_HOME="/gpfs/projects/shared/p_ariac_cetic/miniconda/envs/myr-env/lib/R/library/irace"
export PATH=${IRACE_HOME}/bin/:$PATH
# activate the conda R environment, so that irace can be found
conda activate myr-env
# path to java 24
export PATH="/gpfs/projects/shared/p_ariac_cetic/jdk-24/bin:$PATH"
export JAVA_HOME="/gpfs/projects/shared/p_ariac_cetic/jdk-24"
# sbt for compiling scala code
export PATH="/gpfs/projects/shared/p_ariac_cetic/sbt/bin:$PATH"