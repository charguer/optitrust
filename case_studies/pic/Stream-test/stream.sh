#!/bin/bash

# Usage: ./stream.sh compiler size nbcores

# Example:   COMP=gcc SIZE=20000000 CORES=4 CPULIST="0,1,2,3" ./stream.sh
#
# - COMP is the compiler
# - SIZE is the number of array cells per core, to use for the benchmark; e.g. `20000000` (20 million)
# - SIZEM is like size but in millions (eg 20)
# - CORES is the number of openmp threads to use
# - CPULIST see ../results/README.md for explanations
#
# Script must be executed in the current directory


if [ -z "${COMP}" ]; then
  COMP="gcc"
fi

if [ ! -z "${SIZEM}" ]; then
  SIZE=$((SIZEM * 1000 * 1000))
fi

if [ -z "${SIZE}" ]; then
  SIZE=20000000
fi

if [ -z "${CORES}" ]; then
  CORES=$(grep ^cpu\\scores /proc/cpuinfo | uniq | awk '{print $4}')
fi

echo "COMP=${COMP} SIZE=${SIZE} CORES=${CORES} ./stream.sh"

STREAM_ARRAY_SIZE=$((CORES * SIZE))
COMPILER=${COMP}

###############################################################
# Use local folder

STREAMDIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd ${STREAMDIR}

###############################################################
# Compilation & environment variables

COMPILEARGS="-DSTREAM_ARRAY_SIZE=${STREAM_ARRAY_SIZE} -O3 -march=native -mcmodel=medium stream.c -o stream.out"

if [ "${COMPILER}" = "gcc" ]; then
  gcc -fopenmp ${COMPILEARGS}
  export OMP_PLACES=cores
  export OMP_PROC_BIND=close
  export OMP_NUM_THREADS=${CORES}
  # | tee ./std_output.txt

elif [ "${COMPILER}" = "icc" ]; then
  export LD_LIBRARY_PATH=$INTEL_OPENMP_DYNAMIC_LIBRARY_PATH:$LD_LIBRARY_PATH
  source /opt/intel/oneapi/setvars.sh > /dev/null
  icc -qopenmp ${COMPILEARGS}
  export KMP_AFFINITY=granularity=fine,compact,1,0,verbose
  export OMP_NUM_THREADS=${CORES}

else
  echo "invalid compiler parameter: ${COMPILER}."
  exit 1
fi


###############################################################
# Execution

if [ -z "${CPULIST}" ]; then
  echo "Warning: running stream.sh without taskset and CPULIST"
  ./stream.out
else
  taskset --cpu-list ${CPULIST} ./stream.out
fi


