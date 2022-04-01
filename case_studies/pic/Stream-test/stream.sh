#!/bin/bash

# Usage: ./stream.sh compiler size nbcores

# Example:   COMP=gcc SIZE=2000000 CORES=2 CPULIST="0,1,2,3" ./stream.sh
#
# - SIZE is the number of array cells per core, to use for the benchmark; e.g. `20000000` (20 million)
# - CORES is the number of openmp threads to use
# - CPULIST see ../results/README.md for explanations

if [ -z "${COMP}" ] || [ -z "${SIZE}" ] || [ -z "${CORES}" ] || [ -z "${CPULIST}" ] ; then
  echo "Stream: missing arguments"
  exit 1
fi

STREAMDIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

${STREAMDIR}/a_compile_Stream.sh ${COMP} ${SIZE} || (echo "Stream: compilation failure"; exit 1)

CMDFILE="${STREAMDIR}/__cmd.sh"
echo "${STREAMDIR}/b_run_Stream.sh ${COMP} ${CORES}" > ${CMDFILE}
echo "taskset --cpu-list ${CPULIST} ${CMDFILE}"
chmod +x ${CMDFILE}
echo "taskset --cpu-list ${CPULIST} ${CMDFILE}"
taskset --cpu-list ${CPULIST} ${CMDFILE}

