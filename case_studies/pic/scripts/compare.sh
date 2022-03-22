#!/bin/bash

# Script to execute compile.sh and run.sh

# Usage:  ./check.sh ${TARGET1} ${TARGET2}
#  where ${TARGET} is a C filename from the folder simulations/
#  where ${CHECKER_OUTFILE} is an optional filename argument,
#        where to output particles in the final state

TARGET1=$1
TARGET2=$2

if [ -z ${TARGET1} ]; then
  TARGET1="pic_barsamian.c"
fi
if [ -z ${TARGET2} ]; then
  TARGET2="pic_optimized.c"
fi


DIR_ROOT=..
DIR_OUTFILE=${DIR_ROOT}/3d_runs/run1/
BINARY1="${DIR_OUTFILE}`basename ${TARGET1} .c`.out"
BINARY2="${DIR_OUTFILE}`basename ${TARGET2} .c`.out"

echo "====Compilation===="
make -j3 ${BINARY1} ${BINARY2} params || exit 1
echo "====./run.sh ${TARGET1}===="
./run.sh ${TARGET1}
OUT=$?
if [ ${OUT} -ne 0 ];then
  echo "Error: ${TARGET1} crashed"  #>> /dev/stderr
  #exit 1
fi
echo "====./run.sh ${TARGET2}===="
./run.sh ${TARGET2}
OUT=$?
if [ ${OUT} -ne 0 ];then
  echo "Error: ${TARGET2} crashed"  #>> /dev/stderr
  #exit 1
fi

