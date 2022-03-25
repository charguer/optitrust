#!/bin/bash

# usage: ./bench.sh  machine action
# - where machine is the machine name, for which config_machine.sh must be available

# The environment variable FAST=1 can be used to request a smaller simulation
# The environment variable NOSEQ=1 can be used to skip sequential runs

MACHINE=$1
ACTION=$2



if [ -z "${MACHINE}" ]; then
  echo "Bench: missing machine name"
  exit 1
fi

if [ -z "${ACTION}" ]; then
  ACTION="all"
fi

ROOTDIR=".."
SCRIPTDIR="${ROOTDIR}/scripts "
MACHINEDIR="${MACHINE}"

# use 20 million (array cells per core) for the stream test
STREAMSIZE="20000000"

PROGS="pic_demo.c pic_optimized.c pic_barsamian.c pic_barsamian_malloc.c"

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
  echo "Clear folder ${MACHINE}/"
fi

#--------------------------------------------------------------------------------
# Parameters

if [ "${ACTION}" = "all" ] || [ "${ACTION}" = "params" ]; then

  CONFIGFILE="${ROOTDIR}/your_configuration.sh"
  cp template_your_configuration.sh ${CONFIGFILE}
  echo "nb_threads=${nb_threads}" >> ${CONFIGFILE}
  echo "Generated ${CONFIGFILE}"

  PARAMSFILE="${SCRIPTDIR}/parameters_3d.txt"
  if [ ! -z "${FAST}" ]; then
    PARAMSTEMPLATE="template_parameters_3d.txt"
  else
    PARAMSTEMPLATE="template_parameters_3d_fast.txt"
  fi
  cp template_parameters_3d.txt ${PARAMSFILE}
  echo "nb_particles = ${nb_particles};" >> ${PARAMSFILE}
  echo "Generated ${PARAMSFILE}"

fi

#--------------------------------------------------------------------------------
# Hardware topology

if [ "${ACTION}" = "all" ] || [ "${ACTION}" = "hard" ]; then

  cat /proc/cpuinfo > ${MACHINEDIR}/cpuinfo.txt
  lstopo-no-graphics > ${MACHINEDIR}/lstopo.txt \
    || (echo "warning: lstopo not available, try apt-get install hwloc")
  lstopo --of pdf > ${MACHINEDIR}/lstopo.pdf || (echo "")
  for COMPILER in ${compilers}; do
    ${ROOTDIR}/Stream-test/stream.sh ${COMPILER} ${STREAMSIZE} > ${MACHINEDIR}/stream_${COMPILER}.txt
  done

fi

#--------------------------------------------------------------------------------
# Sequential runs

if [ "${ACTION}" = "all" ] || [ "${ACTION}" = "run" ]; then

  for COMPILER in ${compilers}; do
    for PROG in ${PROGS}; do
      COMP=${COMPILER} ${SCRIPTDIR}/compile.sh ${PROG}
      OUT=$?
      if [ ${OUT} -ne 0 ];then
        echo "Error: could not compile the program ${PROG} using ${COMPILER}"
        exit 1
      fi
      if [ -z "${NOSEQ}" ]; then
        ./run.sh ${PROG} > ${MACHINEDIR}/${PROG}_${COMPILER}_p1.txt || echo "Failure in  ./run.sh ${PROG}, using ${COMPILER}"
      fi
      P=${nb_cores} ./run.sh ${PROG} >  ${MACHINEDIR}/${PROG}_${COMPILER}_p${nb_cores}.txt || echo "Failure in P=${nb_cores} ./run.sh ${PROG}, using ${COMPILER}"
    done
  done

fi

