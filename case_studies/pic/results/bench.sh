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
# The environment variable COMP=gcc can be used to benchmark only one compiler (here gcc)
# The environment variable DRY=1 can be used to make dry runs
# The environment variable NB=100 to use 100 million particles
# The environment variable RUNS=2 to perform more than 1 run
# The environment variable SEED=42 to provide the seed 42 (default is to use the RUN_ID)

# Example:  FAST=1 ./bench.sh
# Example:  FAST=1 COMP=gcc PROG=pic_demo.c ./bench.sh run

# Example COMP=gcc PROG=pic_barsamian.c ./bench.sh run
# Example NOPAR=1 COMP=gcc PROG=pic_demo.c ./bench.sh run

# Example to evaluate noise:
#    RUNS=8 SEED=3 PROG=pic_optimized.c FAST=2 NOSEQ=1 ./bench.sh
# Example to evaluate seed effect:
#    RUNS=8 PROG=pic_optimized.c FAST=2 NOSEQ=1 ./bench.sh


ACTION=$1

if [ -z "${ACTION}" ]; then
  ACTION="all"
fi

if [ -z "${MACHINE}" ]; then
  MACHINE=`hostname`
fi

if [ -z "${RUNS}" ]; then
  RUNS="1"
fi



CURDIR=`pwd`
ROOTDIR=".."
SCRIPTDIR="${ROOTDIR}/scripts"
STREAMDIR="${ROOTDIR}/Stream-test"
MACHINEDIR="${MACHINE}"

# use 10 million (array cells per core) for the stream test
STREAMSIZE="10000000"

# use 1 million particles for a fast run
FASTNBPARTICLES="1000000"

# use 10 million particles for a mid-size run
MIDNBPARTICLES="10000000"

PROGRAMS="pic_optimized.c pic_barsamian.c pic_barsamian_malloc.c"
if [ ! -z "${PROG}" ]; then
  PROGRAMS=${PROG}
fi


#--------------------------------------------------------------------------------
# Load machine configuration

# binds the variables $nb_cores, $compilers, and $nb_particles

CONFIGMACHINE="config_${MACHINE}.sh"
if [ ! -f ${CONFIGMACHINE} ]; then
  echo "${CONFIGMACHINE} not found"
fi
source ${CONFIGMACHINE}

COMPILERS="${compilers}"
if [ ! -z "${COMP}" ]; then
  COMPILERS="${COMP}"
fi

NBCORES="${nb_cores}"
if [ ! -z "${P}" ]; then
  NBCORES="${P}"
fi

#--------------------------------------------------------------------------------
# Create folder, and clear it if needed

mkdir -p ${MACHINEDIR}

if [ "${ACTION}" = "all" ] || [ "${ACTION}" = "clean" ]; then
  rm -f ${MACHINE}/*
  echo "Cleaned folder ${MACHINE}/"
fi

#--------------------------------------------------------------------------------
# Hardware topology (not performed by default)

if [ "${ACTION}" = "hard" ]; then

  cat /proc/cpuinfo > ${MACHINEDIR}/cpuinfo.txt
  lstopo-no-graphics > ${MACHINEDIR}/lstopo.txt \
    || (echo "warning: lstopo not available, try apt-get install hwloc")
  lstopo --of pdf > ${MACHINEDIR}/lstopo.pdf || (echo "")
  for COMPILER in ${compilers}; do
    OUTPUT="${MACHINEDIR}/stream_${COMPILER}_p${NBCORES}.txt"
    ${STREAMDIR}/stream.sh ${COMPILER} ${STREAMSIZE} ${NBCORES} > ${OUTPUT}
    echo "Generated ${OUTPUT}"
  done

fi

#--------------------------------------------------------------------------------
# Parameters (updated if action=run, to allow updating FAST=1 on the command line)

if [ "${ACTION}" = "all" ] || [ "${ACTION}" = "run" ] || [ "${ACTION}" = "params" ]; then

  CONFIGFILE="${ROOTDIR}/your_configuration.sh"
  cp template_your_configuration.sh ${CONFIGFILE}
  echo "nb_threads=${NBCORES}" >> ${CONFIGFILE}
  DEFAULTCOMPILER="${compilers%% *}"
  echo "compiler=\"${DEFAULTCOMPILER}\"" >> ${CONFIGFILE}
  echo "Generated ${CONFIGFILE}       with nb_cores=${NBCORES}"

  PARAMSFILE="${SCRIPTDIR}/parameters_3d.txt"
  if [ -z "${FAST}" ]; then
    PARAMSTEMPLATE="template_parameters_3d.txt"
    NBPARTICLES="$nb_particles"
  elif [ "${FAST}" = "1" ]; then
    PARAMSTEMPLATE="template_parameters_3d_fast.txt"
    NBPARTICLES="${FASTNBPARTICLES}"
  elif [ "${FAST}" = "2" ]; then
    PARAMSTEMPLATE="template_parameters_3d_mid.txt"
    NBPARTICLES="${MIDNBPARTICLES}"
  else
    echo "Invalid value for parameter FAST"
    exit 1
  fi
  if [ ! -z "${NB}" ]; then
    NBPARTICLES=$((${NB} * 1000 * 1000))
  fi

  cp ${PARAMSTEMPLATE} ${PARAMSFILE}
  echo "nb_particles = ${NBPARTICLES};" >> ${PARAMSFILE}
  echo "Generated ${PARAMSFILE}   with nb_particles=${NBPARTICLES}"

fi

#--------------------------------------------------------------------------------
# Runs

cd ${SCRIPTDIR}

if [ "${ACTION}" = "all" ] || [ "${ACTION}" = "run" ]; then
  for COMPILER in ${COMPILERS}; do
    for PROGRAM in ${PROGRAMS}; do
      BASENAME="${PROGRAM%.*}"
      if [ -z ${DRY} ]; then
        COMP=${COMPILER} ${SCRIPTDIR}/compile.sh ${PROGRAM}
      fi
      OUT=$?
      if [ ${OUT} -ne 0 ];then
        echo "Error: could not compile the program ${PROGRAM} using ${COMPILER}"
        exit 1
      fi
      # LATER:RUNCMD TO FACTORIZE
      for ((RUN=0; RUN<RUNS; RUN++)); do
        if [ ! -z "${SEED}" ]; then
          RUNSEED=${SEED}
        else
          RUNSEED=${RUN}
        fi

        if [ -z "${NOSEQ}" ]; then
          OUTFILE="${MACHINEDIR}/${BASENAME}_${COMPILER}_p1_seed${RUNSEED}_run${RUN}.txt"
          echo "P=1 SEED=${RUNSEED}./run.sh ${PROGRAM} > ${OUTFILE}"
          if [ -z ${DRY} ]; then
            P=1 COMP=${COMPILER} SEED=${RUNSEED} ./run.sh ${PROGRAM} | tee ${CURDIR}/${OUTFILE} || echo "Failure in run"
          fi
          # for quiet output:
          # ./run.sh ${PROGRAM} > ${CURDIR}/${OUTFILE} || echo "Failure in run"
        fi
        if [ -z "${NOPAR}" ]; then
          OUTFILE="${MACHINEDIR}/${BASENAME}_${COMPILER}_p${NBCORES}_seed${RUNSEED}_run${RUN}.txt"
          echo "P=${NBCORES} SEED=${RUNSEED} ./run.sh ${PROGRAM} > ${OUTFILE}"
          if [ -z ${DRY} ]; then
            P=${NBCORES} COMP=${COMPILER} SEED=${RUNSEED} ./run.sh ${PROGRAM} | tee ${CURDIR}/${OUTFILE} || echo "Failure in run"
          fi
        fi
      done
    done
  done

fi

#--------------------------------------------------------------------------------
# Summary

cd ${CURDIR}


if [ "${ACTION}" = "all" ] || [ "${ACTION}" = "summary" ] || [ "${ACTION}" = "run" ]; then
  if [ ! -z ${DRY} ]; then
    echo "====No summary in dry run mode===="
  else
    FINAL_RES="${MACHINEDIR}/output.txt"
    echo "====Summary : Exectime / Throughput / Program / Compiler / Cores =====" | tee >> ${FINAL_RES}
    for FILE in ${MACHINEDIR}/pic_*.txt; do
      # RES=$(sed '/^\(Throughput\)/!d' ${FILE})
      THROUGHPUT=$(cat ${FILE} | grep ^Throughput* | awk '{print $2}')
      EXECTIME=$(cat ${FILE} | grep ^Exectime* | awk '{print $2}')

      echo -e "${EXECTIME}\t${THROUGHPUT}\t${FILE}" | tee >> ${FINAL_RES}
    done
    CAT_OUTPUT=`cat ${FINAL_RES}`
  fi
fi
