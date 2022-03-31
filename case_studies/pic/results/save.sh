#!/bin/bash

# usage: ./save.sh [machinename]

MACHINE=$1

if [ -z "${MACHINE}" ]; then
  MACHINE=`hostname`
fi

TIME=$(date '+%Y-%m-%d_%H-%M-%S');

cp ${MACHINE}/output.txt saved/${MACHINE}_${TIME}.txt
