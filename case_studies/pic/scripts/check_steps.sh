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

SOURCES=`ls ${DIR_ROOT}/simulations/${BASENAME}_*_out.c`

TARGET1="${BASENAME}_00_out.c"
BINARY1="${DIR_OUTFILE}`basename ${TARGET1} .c`.out"
CHECKER_OUTFILE1="`basename ${TARGET1} .c`.res"

echo "+++++++++++++++++++REFERENCE+++++++++++++++++++"
make -j3 checker.out ${BINARY1} params || exit 1
./run.sh ${TARGET1}
mv ${DIR_OUTFILE}/particles.res ${DIR_OUTFILE}/${CHECKER_OUTFILE1}
OUT=$?
if [ ${OUT} -ne 0 ];then
  echo "Error: ${TARGET1} crashed"  #>> /dev/stderr
  #exit 1
fi


for SOURCE in ${SOURCES}; do
  echo "+++++++++++++++++++CHECK FOR ${SOURCE}+++++++++++++++++++"
  TARGET2=`basename ${SOURCE}`
  BINARY2="${DIR_OUTFILE}`basename ${TARGET2} .c`.out"
  CHECKER_OUTFILE2="`basename ${TARGET2} .c`.res"
  make ${BINARY2}
  OUT=$?
  if [ ${OUT} -ne 0 ]; then
    echo "FAILURE"  #>> /dev/stderr
    exit 1
  fi
  echo "./run.sh ${TARGET2}"
  ./run.sh ${TARGET2}
  mv ${DIR_OUTFILE}/particles.res ${DIR_OUTFILE}/${CHECKER_OUTFILE2}
  OUT=$?
  if [ ${OUT} -ne 0 ];then
    echo "Error: ${TARGET2} crashed"  #>> /dev/stderr
  fi
  echo "====Comparison: ./checker.out ${DIR_OUTFILE}/${CHECKER_OUTFILE1} ${DIR_OUTFILE}/${CHECKER_OUTFILE2}===="
  ./checker.out  ${DIR_OUTFILE}/${CHECKER_OUTFILE1} ${DIR_OUTFILE}/${CHECKER_OUTFILE2}

done
