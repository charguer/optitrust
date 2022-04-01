#!/bin/bash

# Usage:  ./run.sh ${TARGET}
#  where ${TARGET} is either a C filename from the folder simulations/
#  or the name of a binary in the 3d_runs/run1 folder (either with or without the full path)

# OPTIONS:  a SEED value can be passed, e.g. SEED=0 ./run.sh ${TARGET} ; it should not already be defined in parameters_3d.txt
# OPTIONS:  a COMP value can be passed, e.g. COMP=gcc ./run.sh ${TARGET}

TARGET=$1
TARGET=`basename ${TARGET}`
BASENAME="${TARGET%%.*}"
EXTENSION="${TARGET##*.}"

if [ ${EXTENSION} = "c" ]; then
  BINARY="${BASENAME}.out"
else
  BINARY="${TARGET}"
fi

#HOSTNAME=`hostname`

# Home path for Pic-Vert.
cd ..
PICVERT_HOME=$(pwd)

###################################
#        Your configuration       #
###################################

# All your architecture and setup parameters have to be modified in $PICVERT_HOME/your_configuration.sh
source $PICVERT_HOME/your_configuration.sh

# read compiler name from environment variable, else from your_configuration.sh
COMPILER="${compiler}"
if [ ! -z "${COMP}" ]; then
  COMPILER="${COMP}"
fi

REPORTBINDINGS=""
if [ ! -z "${BIND}" ]; then
  REPORTBINDINGS="--report-bindings"
fi

CPULIST="${CPULIST}"
if [ -z ${CPULIST} ]; then
  echo "need to specify CPULIST"
  exit 1
fi


###################################
#              run                #
###################################

# VALGRIND SUPPORT (sudo apt install valgrind)
VALGRIND=
# Uncomment the line below to activate debugging using valgrind, and deactivate mpirun
#VALGRIND="valgrind --track-origins=yes"

# JEMALLOC SUPPORT (check during a run using "lsof | grep malloc")
JEMALLOC=
JEMALLOC="export LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libjemalloc.so"

# read nb threads from environment variable, else from your_configuration.sh
NBTHREADS="${nb_threads}"
if [ ! -z "${CORES}" ]; then
  NBTHREADS="${CORES}"
fi

RUNINFOS="Run ./${BINARY} NBTHREADS=${NBTHREADS}"
if [ ! -z "${VALGRIND}" ]; then
  RUNINFOS+=", with VALGRIND"
fi
if [ ! -z "${JEMALLOC}" ]; then
  RUNINFOS+=", with JEMALLOC"
fi

if [ ! -z "${SEED}" ]; then
  RUNINFOS+=", SEED=${SEED}"
fi

echo "${RUNINFOS}"


run_one() {
  id_run=$1

  if [ "${COMPILER}" = "gcc" ]; then
    #Threads-to-cores binding.
    export OMP_PLACES=cores
    export OMP_PROC_BIND=close

  elif [ "${COMPILER}" = "icc" ]; then
    source /opt/intel/oneapi/setvars.sh > /dev/null
    export LD_LIBRARY_PATH=$INTEL_OPENMP_DYNAMIC_LIBRARY_PATH:$LD_LIBRARY_PATH
    #Threads-to-cores binding.
    export KMP_AFFINITY=granularity=fine,compact,1,0,verbose
  else
    echo "invalid compiler parameter: ${COMPILER}."
    exit 1
  fi

  cd $PICVERT_HOME/3d_runs/run${id_run}

  # Make sure to copy up to date version of parameters
  cp $PICVERT_HOME/scripts/parameters_3d.txt .
  # include in the parameters the seed value, if provided
  if [ ! -z "${SEED}" ]; then
    echo "seed=${SEED};" >> parameters_3d.txt
  fi

  export OMP_NUM_THREADS=${NBTHREADS}
  ${JEMALLOC}
  COMMAND="./${BASENAME}.out ./parameters_3d.txt | tee ./std_output_run${id_run}.txt"
  if [ "${VALGRIND}" = "" ]; then
    if [ "${COMPILER}" = "gcc" ]; then
      export OMP_NUM_THREADS=$NBTHREADS
      echo ">>> OMP_NUM_THREADS=${OMP_NUM_THREADS} OMP_PLACES=${OMP_PLACES} OMP_PROC_BIND=${OMP_PROC_BIND}"
      CMDFILE="__cmd.sh"
      # echo ">>> mpirun ${REPORTBINDINGS} -np $nb_sockets ${COMMAND} > ${CMDFILE}"
      # echo "mpirun ${REPORTBINDINGS} -np $nb_sockets ${COMMAND}" > ${CMDFILE}

      # echo "${COMMAND}" > ${CMDFILE}
      # echo "taskset --cpu-list ${CPULIST} ./${CMDFILE}"
      # chmod +x ${CMDFILE}
      # taskset --cpu-list ${CPULIST} ./${CMDFILE}
      stdbuf -i0 -o0 -e0  taskset --cpu-list ${CPULIST} ./${BASENAME}.out ./parameters_3d.txt
    elif [ "${COMPILER}" = "icc" ]; then
      mpiexec.hydra -n $nb_sockets ${COMMAND}
    else
      echo "invalid compiler parameter: ${COMPILER}."
      exit 1
    fi

  else
    ${VALGRIND} ${COMMAND}
  fi
}
# LATER: remove -q to see the depreciation warnings
# --cpus-per-proc ${NBTHREADS} | sed 's/MCW rank 0 is not bound//'

###################################
#     Launching the software      #
###################################
for run_id in $(seq 1 $nb_runs)
do
    run_one $run_id
done



