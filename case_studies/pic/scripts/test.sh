#!/bin/bash

# Script to execute compile.sh and run.sh

# Usage:  ./test.sh ${TARGET} [${CHECKER_OUTFILE}]
#  where ${TARGET} is a C filename from the folder simulations/
#  where ${CHECKER_OUTFILE} is an optional filename argument,
#        where to output particles in the final state

TARGET=$1
CHECKER_OUTFILE=$2

./compile.sh ${TARGET} ${CHECKER_OUTFILE} && ./run.sh ${TARGET}
