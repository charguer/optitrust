#!/bin/bash

# usage: ./save.sh [machinename]

MACHINE=$1

if [ -z "${MACHINE}" ]; then
  MACHINE=`hostname`
fi

TIME=$(date '+%Y-%m-%d_%H-%M-%S');

TARGET="saved/${MACHINE}_${TIME}.txt"
cp ${MACHINE}/output.txt ${TARGET}

git add ${TARGET}

echo "Generated ${TARGET} and added it to git"
