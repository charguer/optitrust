#!/bin/bash

# Script to execute compile.sh and run.sh

# Usage:  ./check.sh ${TARGET1} ${TARGET2}
#  where ${TARGET1} and ${TARGET2} are filenames from the folder simulations/

TARGET1=$1
TARGET2=$2

if [ -z ${TARGET1} ]; then
  TARGET1="pic_barsamian.c"
fi
if [ -z ${TARGET2} ]; then
  TARGET2="pic_optimized_checker.c"
fi


DIR_ROOT=..
DIR_OUTFILE=${DIR_ROOT}/3d_runs/run1/
BINARY1="${DIR_OUTFILE}`basename ${TARGET1} .c`_checker.out"
BINARY2="${DIR_OUTFILE}`basename ${TARGET2} .c`_checker.out"
CHECKER_OUTFILE1="`basename ${TARGET1} .c`_checker.res"
CHECKER_OUTFILE2="`basename ${TARGET2} .c`_checker.res"

rm -f ${DIR_OUTFILE}/${CHECKER_OUTFILE1} ${DIR_OUTFILE}/${CHECKER_OUTFILE2}

echo "====Compilation===="
# Note: the make command builds all the binaries, instead of the two binaries
# requested, but for now it is equivalent
echo "make -j3 checker.out ${BINARY1} ${BINARY2} params"
make -j3 checker.out ${BINARY1} ${BINARY2} params || exit 1
echo "====./run.sh ${TARGET1}===="
./run.sh ${BINARY1}
mv ${DIR_OUTFILE}/particles.res ${DIR_OUTFILE}/${CHECKER_OUTFILE1}
OUT=$?
if [ ${OUT} -ne 0 ];then
  echo "Error: ${BINARY1} crashed"  #>> /dev/stderr
  #exit 1
fi
echo "====./run.sh ${BINARY2}===="
./run.sh ${BINARY2}
mv ${DIR_OUTFILE}/particles.res ${DIR_OUTFILE}/${CHECKER_OUTFILE2}
OUT=$?
if [ ${OUT} -ne 0 ];then
  echo "Error: ${BINARY2} crashed"  #>> /dev/stderr
  #exit 1
fi
CMD="./checker.out ${DIR_OUTFILE}/${CHECKER_OUTFILE1} ${DIR_OUTFILE}/${CHECKER_OUTFILE2}"
echo "====${CMD}===="
${CMD}

