#!/bin/bash

# Script to execute compile.sh and run.sh

# Usage:  ./check_steps.sh ${BASENAME}
#  where ${BASENAME} is e.g., pic_demo, to check all "pic_demo_*_out.c"
#  files against "pic_demo_0_out.c"
#  If  ${BASENAME} is not provided, then "pic_demo" is used.

BASENAME=$1

if [ -z ${BASENAME} ]; then
  BASENAME="pic_demo"
fi

DIR_ROOT=..
DIR_OUTFILE=${DIR_ROOT}/3d_runs/run1/

NBTHREADS=`source ${DIR_ROOT}/your_configuration.sh; echo $nb_threads`
if [[ ${NBTHREADS} < 2 ]]; then
  echo "ERROR: you should use multiple threads in your_configuration.sh to run the checker"
  exit 1
fi


SOURCES=`ls ${DIR_ROOT}/simulations/${BASENAME}_*_out.c`

TARGET1="${BASENAME}_00_out.c"
BINARY1="${DIR_OUTFILE}`basename ${TARGET1} .c`_checker.out"


#CHECKER_OUTFILE1="`basename ${TARGET1} .c`.res"
CHECKER_OUTFILE1="`basename ${TARGET1} .c`_checker.res"

# Faster compilation
export COMPOPT="-O1"

echo "+++++++++++++++++++CLEANUP+++++++++++++++++++"
make clean

echo "+++++++++++++++++++REFERENCE+++++++++++++++++++"
make -j3 checker.out ${BINARY1} params || exit 1
# make checker.out params
# ./compile.sh ${TARGET1} 1
./run.sh ${BINARY1}
mv ${DIR_OUTFILE}/particles.res ${DIR_OUTFILE}/${CHECKER_OUTFILE1}
OUT=$?
if [ ${OUT} -ne 0 ];then
  echo "Error: ${TARGET1} crashed"  #>> /dev/stderr
  #exit 1
fi


for SOURCE in ${SOURCES}; do
  echo "+++++++++++++++++++CHECK FOR ${SOURCE}+++++++++++++++++++"
  TARGET2=`basename ${SOURCE}`
  BINARY2="${DIR_OUTFILE}`basename ${TARGET2} .c`_checker.out"
  #CHECKER_OUTFILE2="`basename ${TARGET2} .c`.res"
  CHECKER_OUTFILE2="`basename ${TARGET2} .c`_checker.res"
  make ${BINARY2}
  # ./compile.sh ${TARGET2} 1
  OUT=$?
  if [ ${OUT} -ne 0 ]; then
    echo "FAILURE"  #>> /dev/stderr
    exit 1
  fi
  echo "./run.sh ${TARGET2}"
  ./run.sh ${BINARY2}
  mv ${DIR_OUTFILE}/particles.res ${DIR_OUTFILE}/${CHECKER_OUTFILE2}
  OUT=$?
  if [ ${OUT} -ne 0 ];then
    echo "Error: ${TARGET2} crashed"  #>> /dev/stderr
  fi
  echo "====Comparison: ./checker.out ${DIR_OUTFILE}/${CHECKER_OUTFILE1} ${DIR_OUTFILE}/${CHECKER_OUTFILE2}===="
  ./checker.out  ${DIR_OUTFILE}/${CHECKER_OUTFILE1} ${DIR_OUTFILE}/${CHECKER_OUTFILE2}

done
