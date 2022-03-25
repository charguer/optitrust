#!/bin/bash

# Execute PIC benchmarks.

# usage: ./bench.sh [action]
#
# where action can be one of: clean, params, hard, run

# The environment variable MACHINE can be used to use config_${MACHINE}.sh
#   instead of config_`hostname`.sh
# The environment variable FAST=1 can be used to request a smaller simulation
# The environment variable NOSEQ=1 can be used to skip sequential runs
# The environment variable NOPAR=1 can be used to skip parallel runs
# The environment variable PROG=pic_demo.c can be used to benchmark only one code
# The environment variable COMP=gcc can be used to benchmark only one compiler

# Example:  FAST=1 ./bench.sh
# Example:  FAST=1 COMP=gcc PROG=pic_demo.c ./bench.sh run


ACTION=$1

if [ -z "${ACTION}" ]; then
  ACTION="all"
fi

if [ -z "${MACHINE}" ]; then
  MACHINE=`hostname`
fi

CURDIR=`pwd`
ROOTDIR=".."
SCRIPTDIR="${ROOTDIR}/scripts"
STREAMDIR="${ROOTDIR}/Stream-test"
MACHINEDIR="${MACHINE}"

# use 20 million (array cells per core) for the stream test
STREAMSIZE="20000000"

# use 2 million particles for a fast run
FASTNBPARTICLES="2000000"

PROGRAMS="pic_demo.c pic_optimized.c pic_barsamian.c pic_barsamian_malloc.c"

#--------------------------------------------------------------------------------
# Load machine configuration

# binds the variables $nb_cores, $compilers, and $nb_particles

CONFIGMACHINE="config_${MACHINE}.sh"
if [ ! -f ${CONFIGMACHINE} ]; then
  echo "${CONFIGMACHINE} not found"
fi
source ${CONFIGMACHINE}


#--------------------------------------------------------------------------------
# Create folder, and clear it if needed

mkdir -p ${MACHINEDIR}

if [ "${ACTION}" = "all" ] || [ "${ACTION}" = "clean" ]; then
  rm -f ${MACHINE}/*
  echo "Cleaned folder ${MACHINE}/"
fi

#--------------------------------------------------------------------------------
# Hardware topology

if [ "${ACTION}" = "all" ] || [ "${ACTION}" = "hard" ]; then

  cat /proc/cpuinfo > ${MACHINEDIR}/cpuinfo.txt
  lstopo-no-graphics > ${MACHINEDIR}/lstopo.txt \
    || (echo "warning: lstopo not available, try apt-get install hwloc")
  lstopo --of pdf > ${MACHINEDIR}/lstopo.pdf || (echo "")
  for COMPILER in ${compilers}; do
    OUTPUT="${MACHINEDIR}/stream_${COMPILER}_p${nb_cores}.txt"
    ${STREAMDIR}/stream.sh ${COMPILER} ${STREAMSIZE} ${nb_cores} > ${OUTPUT}
    echo "Generated ${OUTPUT}"
  done

fi

#--------------------------------------------------------------------------------
# Parameters (updated if action=run, to allow updating FAST=1 on the command line)

if [ "${ACTION}" = "all" ] || [ "${ACTION}" = "run" ] || [ "${ACTION}" = "params" ]; then

  CONFIGFILE="${ROOTDIR}/your_configuration.sh"
  cp template_your_configuration.sh ${CONFIGFILE}
  echo "nb_threads=${nb_cores}" >> ${CONFIGFILE}
  DEFAULTCOMPILER="${compilers%% *}"
  echo "compiler=\"${DEFAULTCOMPILER}\"" >> ${CONFIGFILE}
  echo "Generated ${CONFIGFILE}       with nb_cores=${nb_cores}"

  PARAMSFILE="${SCRIPTDIR}/parameters_3d.txt"
  if [ -z "${FAST}" ]; then
    PARAMSTEMPLATE="template_parameters_3d.txt"
    NBPARTICLES="$nb_particles"
  else
    PARAMSTEMPLATE="template_parameters_3d_fast.txt"
    NBPARTICLES="${FASTNBPARTICLES}"
  fi
  cp ${PARAMSTEMPLATE} ${PARAMSFILE}
  echo "nb_particles = ${NBPARTICLES};" >> ${PARAMSFILE}
  echo "Generated ${PARAMSFILE}   with nb_particles=${NBPARTICLES}"

fi

#--------------------------------------------------------------------------------
# Runs

cd ${SCRIPTDIR}

if [ "${ACTION}" = "all" ] || [ "${ACTION}" = "run" ]; then

  for COMPILER in ${compilers}; do
    if [ ! -z "${COMP}" ] && [ "${COMPILER}" != "${COMP}" ]; then
      continue;
    fi
    for PROGRAM in ${PROGRAMS}; do
      if [ ! -z "${PROG}" ] && [ "${PROGRAM}" != "${PROG}" ]; then
        continue;
      fi
      BASENAME="${PROGRAM%.*}"
      COMP=${COMPILER} ${SCRIPTDIR}/compile.sh ${PROGRAM}
      OUT=$?
      if [ ${OUT} -ne 0 ];then
        echo "Error: could not compile the program ${PROGRAM} using ${COMPILER}"
        exit 1
      fi
      # LATER:RUNCMD TO FACTORIZE
      if [ -z "${NOSEQ}" ]; then
        OUTFILE="${MACHINEDIR}/${BASENAME}_${COMPILER}_p1.txt"
        echo "P=1 ./run.sh ${PROGRAM} > ${OUTFILE}"
        P=1 COMP=${COMPILER} ./run.sh ${PROGRAM} | tee ${CURDIR}/${OUTFILE} || echo "Failure in run"
        # for quiet output:
        # ./run.sh ${PROGRAM} > ${CURDIR}/${OUTFILE} || echo "Failure in run"
      fi
      if [ -z "${NOPAR}" ]; then
        OUTFILE="${MACHINEDIR}/${BASENAME}_${COMPILER}_p${nb_cores}.txt"
        echo "P=${nb_cores} ./run.sh ${PROGRAM} > ${OUTFILE}"
        P=${nb_cores} COMP=${COMPILER} ./run.sh ${PROGRAM} | tee ${CURDIR}/${OUTFILE} || echo "Failure in run"
      fi
    done
  done

fi

#--------------------------------------------------------------------------------
# Summary

cd ${CURDIR}

if [ "${ACTION}" = "all" ] || [ "${ACTION}" = "summary" ]; then

  echo "====Summary : Exectime / Throughput / Program / Compiler / Cores ====="
  for FILE in ${MACHINEDIR}/pic_*.txt; do
    # RES=$(sed '/^\(Throughput\)/!d' ${FILE})
    THROUGHPUT=$(cat ${FILE} | grep ^Throughput* | awk '{print $2}')
    EXECTIME=$(cat ${FILE} | grep ^Exectime* | awk '{print $2}')

    echo -e "${EXECTIME}\t${THROUGHPUT}\t${FILE}"
  done
fi
