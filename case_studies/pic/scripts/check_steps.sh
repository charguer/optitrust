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

for SOURCE in ${SOURCES}; do
  echo "+++++++++++++++++++CHECK FOR ${SOURCE}+++++++++++++++++++"
  TARGET1="${BASENAME}_0_out.c"
  TARGET2=`basename ${SOURCE}`
  ./check.sh ${TARGET1} ${TARGET2}
  OUT=$?
  if [ ${OUT} -ne 0 ]; then
    echo "FAILURE"  #>> /dev/stderr
    exit 1
  fi

done
