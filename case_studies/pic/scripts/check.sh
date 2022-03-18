#!/bin/bash

# Script to execute compile.sh and run.sh

# Usage:  ./check.sh ${TARGET1} ${TARGET2}
#  where ${TARGET} is a C filename from the folder simulations/
#  where ${CHECKER_OUTFILE} is an optional filename argument,
#        where to output particles in the final state

TARGET1=$1
TARGET2=$2

if [ -z ${TARGET1} ]; then
  TARGET1="pic_demo_00_out.c"
fi
if [ -z ${TARGET2} ]; then
  TARGET2="pic_optimized.c"
fi


DIR_ROOT=..
DIR_OUTFILE=${DIR_ROOT}/3d_runs/run1/
BINARY1="${DIR_OUTFILE}`basename ${TARGET1} .c`.out"
BINARY2="${DIR_OUTFILE}`basename ${TARGET2} .c`.out"
CHECKER_OUTFILE1="`basename ${TARGET1} .c`.res"
CHECKER_OUTFILE2="`basename ${TARGET2} .c`.res"


#rm -f ${CHECKER_OUTFILE1} ${CHECKER_OUTFILE2}
rm -f ${DIR_OUTFILE}/${CHECKER_OUTFILE1} ${DIR_OUTFILE}/${CHECKER_OUTFILE2}
# echo "./checker.out  ${DIR_OUTFILE}/${CHECKER_OUTFILE1} ${DIR_OUTFILE}/${CHECKER_OUTFILE2}"

echo "====Compilation===="
# Note: the make command builds all the binaries, instead of the two binaries
# requested, but for now it is equivalent
make -j3 checker.out ${BINARY1} ${BINARY2} params || exit 1
# make checker.out
#./compile.sh ${TARGET1} ${CHECKER_OUTFILE1} || exit 1
#./compile.sh ${TARGET2} ${CHECKER_OUTFILE2} || exit 1
echo "====./run.sh ${TARGET1}===="
./run.sh ${TARGET1}
mv ${DIR_OUTFILE}/particles.res ${DIR_OUTFILE}/${CHECKER_OUTFILE1}
OUT=$?
if [ ${OUT} -ne 0 ];then
  echo "Error: ${TARGET1} crashed"  #>> /dev/stderr
  #exit 1
fi
echo "====./run.sh ${TARGET2}===="
./run.sh ${TARGET2}
mv ${DIR_OUTFILE}/particles.res ${DIR_OUTFILE}/${CHECKER_OUTFILE2}
OUT=$?
if [ ${OUT} -ne 0 ];then
  echo "Error: ${TARGET2} crashed"  #>> /dev/stderr
  #exit 1
fi
echo "====Comparison: ./checker.out  ${DIR_OUTFILE}/${CHECKER_OUTFILE1} ${DIR_OUTFILE}/${CHECKER_OUTFILE2}===="
./checker.out  ${DIR_OUTFILE}/${CHECKER_OUTFILE1} ${DIR_OUTFILE}/${CHECKER_OUTFILE2}
#ln -f -s ${DIR_OUTFILE}/${CHECKER_OUTFILE1} ${CHECKER_OUTFILE1}
#ln -f -s ${DIR_OUTFILE}/${CHECKER_OUTFILE2} ${CHECKER_OUTFILE2}
#./checker.out ${CHECKER_OUTFILE1} ${CHECKER_OUTFILE2}


