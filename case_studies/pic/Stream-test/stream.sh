#!/bin/bash

# Usage: ./stream compiler size
# - compiler is the name of the compiler to use, `gcc` or `icc`
# - size is the number of array cells per core, to use for the benchmark; e.g. `20000000` (20 million)

COMPILER=$1
SIZE=$2

if [ -z "${COMPILER}" ] || [ -z "${SIZE}" ] ; then
  echo "Stream: missing arguments"
  exit 1
fi

./a_compile_Stream.sh ${COMPILER} ${SIZE} || (echo "Stream: compilation failure"; exit 1)
./b_run_Stream.sh
