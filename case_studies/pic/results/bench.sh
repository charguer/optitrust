#!/bin/bash

# Execute PIC benchmarks.

# usage: ./bench.sh [action]
#
# where action can be one of: clean, params, hard, run

# The environment variable PROG="pic_demo.c pic_barsamian.c" to specify programs
# The environment variable CORES=4 to provide the number of cores
# The environment variable GRID=32 to specify grid size
# The environment variable COMP="gcc icc" to specify compilers
# The environment variable NB=100 to use 100 million particles
# The environment variable RUNS=2 to perform more than 1 run
# The environment variable STEPS=100 to provide the number of steps
# The environment variable SEED=42 to provide the seed 42 (default is to use the RUN_ID)
# The environment variable CPULIST="0,2" to specify processing units to use

# The environment variable DRY=1 can be used to make dry runs


# DEPRECATED
# The environment variable NOSEQ=1 can be used to skip sequential runs
# The environment variable NOPAR=1 can be used to skip parallel runs


# See the bottom of README.md for examples

ACTION=$1
if [ -z "${ACTION}" ]; then
  ACTION="all"
fi

if [ -z "${MACHINE}" ]; then
  MACHINE=`hostname`
fi

if [ "${ACTION}" != "summary" ]; then

  if [ -z "${COMP}" ]; then
    echo "must provide COMP, the list of compilers, space separated"
    exit 1
  fi
  COMPILERS="${COMP}"

  if [ -z "${CORES}" ]; then
    echo "must provide CORES, the number of cores to use"
    exit 1
  fi
  NBCORES="${CORES}"

  if [ "${ACTION}" != "hard" ]; then

    if [ -z "${RUNS}" ]; then
      RUNS="1"
    fi

    if [ -z "${STEPS}" ]; then
      echo "must provide STEPS, number of simulation steps"
      exit 1
    fi

    if [ -z "${NB}" ]; then
      echo "must provide NB, number of particles in millions"
      exit 1
    fi
    NBPARTICLES=$((${NB} * 1000 * 1000))

    if [ -z "${GRID}" ]; then
      echo "must provide GRID, number of cells in each dimension of the grid"
      exit 1
    fi

    if [ -z "${PROG}" ]; then
      echo "must provide PROG, the list of programs, space separated"
      exit 1
    fi
    PROGRAMS=${PROG}

    if [ -z "${CPULIST}" ]; then
      echo "must provide CPULIST, a coma-separated list with as many values as the argument CORES"
      exit 1
    fi

  fi

fi

CURDIR=`pwd`
ROOTDIR=".."
SCRIPTDIR="${ROOTDIR}/scripts"
STREAMDIR="${ROOTDIR}/Stream-test"
MACHINEDIR="${MACHINE}"

# use 10 million (array cells per core) for the stream test
STREAMSIZEMILLION="10"
STREAMSIZE=$((STREAMSIZEMILLION * 1000 * 1000))


#--------------------------------------------------------------------------------
# Load machine configuration

# DEPREACATED
# binds the variables $nb_cores, $compilers, and $nb_particles

# CONFIGMACHINE="config_${MACHINE}.sh"
# if [ ! -f ${CONFIGMACHINE} ]; then
#  echo "${CONFIGMACHINE} not found"
# fi
# source ${CONFIGMACHINE}

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

  VERSION="${MACHINEDIR}/versions.txt"
  uname -mrs > ${VERSION}
  echo "------------------------------" >> ${VERSION}
  gcc --version >> ${VERSION}
  echo "------------------------------" >> ${VERSION}
  mpirun --version >> ${VERSION}
  cat /proc/cpuinfo > ${MACHINEDIR}/cpuinfo.txt
  lstopo-no-graphics > ${MACHINEDIR}/lstopo.txt \
    || (echo "warning: lstopo not available, try apt-get install hwloc")
  lstopo --of pdf > ${MACHINEDIR}/lstopo.pdf || (echo "")
  for COMPILER in ${COMPILERS}; do
    OUTPUT="${MACHINEDIR}/stream_${COMPILER}_cores${NBCORES}_size${STREAMSIZEMILLION}.txt"
    COMP=${COMPILER} SIZE=${STREAMSIZE} CORES=${NBCORES} CPULIST=${CPULIST} ${STREAMDIR}/stream.sh > ${OUTPUT}
    echo "Generated ${OUTPUT}"
  done

fi

#--------------------------------------------------------------------------------
# Parameters (updated if action=run, to allow updating FAST=1 on the command line)

if [ "${ACTION}" = "all" ] || [ "${ACTION}" = "run" ] || [ "${ACTION}" = "params" ]; then

  CONFIGFILE="${ROOTDIR}/your_configuration.sh"
  cp template_your_configuration.sh ${CONFIGFILE}
  echo "nb_threads=${NBCORES}" >> ${CONFIGFILE}
  echo "cpulist=\"${CPULIST}\"" >> ${CONFIGFILE}
  DEFAULTCOMPILER="${COMPILERS%% *}"
  echo "compiler=\"${DEFAULTCOMPILER}\"" >> ${CONFIGFILE}
  echo "Generated ${CONFIGFILE}       with nb_cores=${NBCORES}"

  PARAMSTEMPLATE="template_parameters_3d.txt"
  PARAMSFILE="${SCRIPTDIR}/parameters_3d.txt"
  cp ${PARAMSTEMPLATE} ${PARAMSFILE}
  echo "nb_particles = ${NBPARTICLES};" >> ${PARAMSFILE}
  echo "nb_iterations = ${STEPS};" >> ${PARAMSFILE}
  echo "nb_cells_x = ${GRID};" >> ${PARAMSFILE}
  echo "nb_cells_y = ${GRID};" >> ${PARAMSFILE}
  echo "nb_cells_z = ${GRID};" >> ${PARAMSFILE}
  echo "Generated ${PARAMSFILE}   with nb_particles=${NBPARTICLES},nb_iterations=${STEPS},nb_cells_{x,y,z}=${GRID}"

fi

#--------------------------------------------------------------------------------
# Runs

cd ${SCRIPTDIR}

if [ "${ACTION}" = "all" ] || [ "${ACTION}" = "run" ]; then
  for COMPILER in ${COMPILERS}; do

    echo "****************** COMPILATION ******************************"
    for PROGRAM in ${PROGRAMS}; do
      if [ -z ${DRY} ]; then
        COMP=${COMPILER} ${SCRIPTDIR}/compile.sh ${PROGRAM}
      fi
      OUT=$?
      if [ ${OUT} -ne 0 ];then
        echo "Error: could not compile the program ${PROGRAM} using ${COMPILER}"
        exit 1
      fi
    done

    for ((RUN=0; RUN<RUNS; RUN++)); do
      echo "****************** RUN ${RUN} ******************************"
      for PROGRAM in ${PROGRAMS}; do
        BASENAME="${PROGRAM%.*}"
        # LATER:RUNCMD TO FACTORIZE
        if [ ! -z "${SEED}" ]; then
          RUNSEED=${SEED}
        else
          RUNSEED=${RUN}
        fi
        OUTFILE="${MACHINEDIR}/results_cores${NBCORES}_${COMPILER}_grid${GRID}_nb${NB}_steps${STEPS}_seed${RUNSEED}_run${RUN}_${BASENAME}.txt"
        echo "P=${NBCORES} SEED=${RUNSEED} ./run.sh ${PROGRAM} > ${OUTFILE}"
        if [ -z ${DRY} ]; then
          CORES=${NBCORES} COMP=${COMPILER} SEED=${RUNSEED} ./run.sh ${PROGRAM} | tee ${CURDIR}/${OUTFILE} || echo "Failure in run"
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
    for FILE in ${MACHINEDIR}/results_*.txt; do
      # RES=$(sed '/^\(Throughput\)/!d' ${FILE})
      THROUGHPUT=$(cat ${FILE} | grep ^Throughput* | awk '{print $2}')
      EXECTIME=$(cat ${FILE} | grep ^Exectime* | awk '{print $2}')

      echo -e "${EXECTIME}\t${THROUGHPUT}\t${FILE}" | tee >> ${FINAL_RES}
    done
    CAT_OUTPUT=`cat ${FINAL_RES}`
    echo "${CAT_OUTPUT}"
  fi
fi

