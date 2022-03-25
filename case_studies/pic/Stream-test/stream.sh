#!/bin/bash

# Usage: ./stream.sh compiler size nbcores
# - compiler is the name of the compiler to use, `gcc` or `icc`
# - size is the number of array cells per core, to use for the benchmark; e.g. `20000000` (20 million)
# - nbcores is the number of openmp threads to use

# Example:   ./stream.sh gcc 2000000 2

COMPILER=$1
SIZE=$2
NBCORES=$3

if [ -z "${COMPILER}" ] || [ -z "${SIZE}" ] || [ -z "${NBCORES}" ] ; then
  echo "Stream: missing arguments"
  exit 1
fi

STREAMDIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

${STREAMDIR}/a_compile_Stream.sh ${COMPILER} ${SIZE} || (echo "Stream: compilation failure"; exit 1)
${STREAMDIR}/b_run_Stream.sh ${COMPILER} ${NBCORES}
