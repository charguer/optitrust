#!/bin/bash

# Script to execute call ./check.sh on multiple runs with multiple parameters

# Usage:  ./checks.sh ${TARGET1} ${TARGET2}
#  where ${TARGET1} and ${TARGET2} are filenames from the folder simulations/

# Run parameters are set by calling   ../results/bench.sh params
# in particular, use FAST=1 or FAST=2 for fast runs (default is FAST=1)

# The environment variable RUNS=2 to perform more than 1 run
# The environment variable P=2 to specify 2 cores for the parallel run
#   (the default value for P is $nb_threads from ../your_configuration.sh)
# The environment variable NOSEQ=1 can be used to skip sequential runs
# The environment variable NOPAR=1 can be used to skip parallel runs
# The environment variable SEED=42 to provide the seed 42 (default is to use the RUN_ID)
# The environment variable DRY=1 can be used to make dry runs
# The environment variable STEPS=100 to provide the number of steps (default is 5)
# The environment variable NB=100 to use 100 million particles (default is 1)



# Example usage:
#   DRY=1 NOSEQ=1 NB=1 STEPS=1000 RUNS=20 ./checks.sh pic_barsamian.c pic_optimized_checker.c
#   cat parameters_3d.txt
#   NOSEQ=1 NB=1 STEPS=1000 RUNS=20 ./checks.sh pic_barsamian.c pic_optimized_checker.c


#--------------------------------------------------------------------------------

TARGET1=$1
TARGET2=$2

if [ -z "${RUNS}" ]; then
  RUNS="1"
fi

if [ -z "${FAST}" ]; then
  FAST="1"
fi

if [ -z "${NB}" ]; then
  NB="1"
fi

if [ -z "${STEPS}" ]; then
  STEPS="5"
fi

# disable sequential run if parallel run is requested with P=1
if [ "${P}" = 1 ]; then
  NOSEQ="1"
fi

# if P is not provided, read it from your_configuration
if [ -z "${P}" ]; then
  P=`source ../your_configuration.sh; echo $nb_threads`
fi

DIR=`pwd`

LOGFILE="checks.log"

# Clear log file
> ${LOGFILE}

#--------------------------------------------------------------------------------

cd ../results
NB=${NB} FAST=${FAST} STEPS=${STEPS} ./bench.sh params
cd ${DIR}

#--------------------------------------------------------------------------------
# Run

for ((RUN=0; RUN<RUNS; RUN++)); do
  if [ ! -z "${SEED}" ]; then
    RUNSEED=${SEED}
  else
    RUNSEED=${RUN}
  fi
  COMMAND="./check.sh ${TARGET1} ${TARGET2}"
  OUTFILE="checks.log"

  if [ -z "${NOSEQ}" ]; then
    echo "P=1 SEED=${RUNSEED} ${COMMAND}"
    if [ -z ${DRY} ]; then
      P=1 SEED=${RUNSEED} ${COMMAND} | tee -a ${LOGFILE} || echo "Failure in run"
    fi
  fi
  if [ -z "${NOPAR}" ]; then
    echo "P=${P} SEED=${RUNSEED} ${COMMAND}"
    if [ -z ${DRY} ]; then
       P=${P} SEED=${RUNSEED} ${COMMAND} | tee -a ${LOGFILE} || echo "Failure in run"
    fi
  fi
done

#--------------------------------------------------------------------------------
# Summary

echo "================Summary for checks.sh==================="
cat ${LOGFILE} | grep "^Maximal dist pos relative to area width"
