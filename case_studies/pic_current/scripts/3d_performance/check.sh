#!/bin/bash

# Script to execute compile.sh and run.sh

# Usage:  ./check.sh ${TARGET1} ${TARGET2}
#  where ${TARGET} is a C filename from the folder simulations/
#  where ${CHECKER_OUTFILE} is an optional filename argument,
#        where to output particles in the final state

TARGET1=$1
TARGET2=$2
CHECKER_OUTFILE1="`basename ${TARGET1} .c`.res"
CHECKER_OUTFILE2="`basename ${TARGET2} .c`.res"

echo "====Compilation===="
make checker.out
./compile.sh ${TARGET1} ${CHECKER_OUTFILE1}
./compile.sh ${TARGET2} ${CHECKER_OUTFILE2}
echo "====Execution===="
./run.sh ${TARGET1}
./run.sh ${TARGET2}
echo "====Comparison===="
ln -f -s ../../3d_runs/run1/${CHECKER_OUTFILE1} ${CHECKER_OUTFILE1}
ln -f -s ../../3d_runs/run1/${CHECKER_OUTFILE2} ${CHECKER_OUTFILE2}
./checker.out ${CHECKER_OUTFILE1} ${CHECKER_OUTFILE2}


