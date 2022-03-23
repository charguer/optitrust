#!/bin/bash

# Usage:  ./run.sh ${TARGET}
#  where ${TARGET} is either a C filename from the folder simulations/
#  or the name of a binary in the 3d_runs/run1 folder (either with or without the full path)

TARGET=$1
TARGET=`basename ${TARGET}`
BASENAME="${TARGET%%.*}"
EXTENSION="${TARGET##*.}"

if [ ${EXTENSION} = "c" ]; then
  BINARY="${BASENAME}.out"
else
  BINARY="${TARGET}"
fi

# Home path for Pic-Vert.
cd ..
PICVERT_HOME=$(pwd)


###################################
#        Your configuration       #
###################################

# All your architecture and setup parameters have to be modified in $PICVERT_HOME/your_configuration.sh
source $PICVERT_HOME/your_configuration.sh

###################################
#              run                #
###################################

# VALGRIND SUPPORT (sudo apt install valgrind)
VALGRIND=
# Uncomment the line below to activate debugging using valgrind, and deactivate mpirun
#VALGRIND="valgrind --track-origins=yes"

# JEMALLOC SUPPORT (check during a run using "lsof | grep malloc")
JEMALLOC=
JEMALLOC="export LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libjemalloc.so.1"

# read nb threads from environment variable, est from your_configuration.sh
NBTHREADS="${nb_threads}"
if [ ! -z "${P}" ]; then
  NBTHREADS="${P}"
fi

RUNINFOS="Run ./${BINARY} NBTHREADS=${NBTHREADS}"
if [ ! -z "${VALGRIND}" ]; then
  RUNINFOS+=", with VALGRIND"
fi
if [ ! -z "${JEMALLOC}" ]; then
  RUNINFOS+=", with JEMALLOC"
fi
echo "${RUNINFOS}"

run_one() {
  id_run=$1

  if [ "${compiler}" = "gcc" ]; then
    #Threads-to-cores binding.
    export OMP_PLACES=cores
    export OMP_PROC_BIND=close
  elif [ "${compiler}" = "icc" ]; then
    export LD_LIBRARY_PATH=$INTEL_OPENMP_DYNAMIC_LIBRARY_PATH:$LD_LIBRARY_PATH
    #Threads-to-cores binding.
    export KMP_AFFINITY=granularity=fine,compact,1,0,verbose
  else
    echo "invalid compiler parameter: ${compiler}."
    exit 1
  fi

  cd $PICVERT_HOME/3d_runs/run${id_run}
  export OMP_NUM_THREADS=$nb_threads
  ${JEMALLOC}
  COMMAND="./${BASENAME}.out ./parameters_3d.txt | tee ./std_output_run${id_run}.txt"
  if [ "${VALGRIND}" = "" ]; then
    if [ "${compiler}" = "gcc" ]; then
      mpirun -q --report-bindings --cpus-per-proc $nb_threads -np $nb_sockets ${COMMAND}   
    
    elif [ "${compiler}" = "icc" ]; then
      mpiexec.hydra -n $nb_sockets ${COMMAND}   
    else 
      echo "invalid compiler parameter: ${compiler}."
      exit 1
    fi

  else
    ${VALGRIND} ${COMMAND}
  fi
}
# LATER: remove -q to see the depreciation warnings


###################################
#     Launching the software      #
###################################
for run_id in $(seq 1 $nb_runs)
do
    run_one $run_id
done

