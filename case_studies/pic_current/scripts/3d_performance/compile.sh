#!/bin/bash

# Usage:  ./compile.sh ${TARGET} [${CHECKER_OUTFILE}]
#  where ${TARGET} is a C filename from the folder simulations/
#  where ${CHECKER_OUTFILE} is an optional filename argument,
#        where to output particles in the final state;
#        If ${CHECKER_OUTFILE} is the string "1", then the filename
#        is automatically generated from the target

TARGET=$1
BASENAME=`basename ${TARGET} .c`
CHECKER_OUTFILE=$2

if [ "$CHECKER_OUTFILE" == "1" ]; then
  CHECKER_OUTFILE="${BASENAME}.res"
fi

#Home path for Pic-Vert.
cd ../..
PICVERT_HOME=$(pwd)


###################################
#        Your configuration       #
###################################
# All your architecture and setup parameters have to be modified in $PICVERT_HOME/your_configuration.sh
source $PICVERT_HOME/your_configuration.sh

###################################
#      Simulation parameters      #
###################################
# Please check the maximum memory on one NUMA node of your architecture, then open Pic-Vert/memory/memory-3d.ods and check the resulting maximum number of particles possible, then change the line "nb_particles" in parameters_3d.txt accordingly.
# As example, 2 billions is a maximum for 96 GB of memory.

# Other things may be modified:
# Modify the compile scripts : modify the -DCHUNK_SIZE= for a different chunk size
# Modify other parameters    : modify the other parameters in parameters_3d.txt


# Default value for CHUNK_SIZE, if not defined in your_configuration.sh
if [ -z "$CHUNK_SIZE" ]; then
  CHUNK_SIZE=256
fi

###################################
#              compilation        #
###################################

# possible additional flags -DPRINTPARAMS -DDEBUG_CHECKER -DDEBUG_CHARGE -DDEBUG_FIELD -DDEBUG_ACCEL -DDEBUG_CREATION
# more flags: -DPRINTPERF -DPRINTSTEPS

DEBUGFLAGS="-DDEBUG_ACCEL "

if [ ! -z "$CHECKER_OUTFILE" ]; then
  CHECKER="-DCHECKER=$CHECKER_OUTFILE ${DEBUGFLAGS}"
fi

EXTRA_BARSAMIAN=
if [ "${TARGET}" = "pic_barsamian.c" ]; then
    EXTRA_BARSAMIAN="$PICVERT_HOME/src/particle_type_concurrent_chunkbags_of_soa_3d.c"
fi

COMPILE_ARGS="-I$PICVERT_HOME/include $PICVERT_HOME/src/matrix_functions.c $PICVERT_HOME/src/meshes.c $PICVERT_HOME/src/output.c $PICVERT_HOME/src/parameter_reader.c $PICVERT_HOME/src/random.c $PICVERT_HOME/src/space_filling_curves.c $PICVERT_HOME/src/diagnostics.c $PICVERT_HOME/src/fields.c $PICVERT_HOME/src/initial_distributions.c $EXTRA_BARSAMIAN  $PICVERT_HOME/src/poisson_solvers.c $PICVERT_HOME/src/rho.c $PICVERT_HOME/simulations/${BASENAME}.c -DSPARE_LOC_OPTIMIZED -DOMP_TILE_SIZE=2 -DCHUNK_SIZE=$CHUNK_SIZE $CHECKER -lfftw3 -lm -O3  -march=native -std=gnu11"


# Depending on your version, you may change "export OMPI_CC=icc\n  mpicc" by just "mpiicc".
compile_one() {
  id_run=$1
  cd $PICVERT_HOME/3d_runs
  mkdir -p run${id_run}
  cp $PICVERT_HOME/scripts/3d_performance/parameters_3d.txt run${id_run}/
  if [ "${compiler}" = "gcc" ]; then
    export OMPI_CC=gcc
    mpicc ${COMPILE_ARGS} -fopenmp -o run${id_run}/${BASENAME}.out
  elif [ "${compiler}" = "icc" ]; then
    export OMPI_CC=icc
    mpicc ${COMPILE_ARGS} -qopenmp -o run${id_run}/${BASENAME}.out
  else
    echo "invalid compiler parameter: ${compiler}."
    exit 1
  fi
}

###################################
#     Compiling the software      #
###################################

# LATER: we could avoid compiling the program once per run!
cd $PICVERT_HOME
mkdir -p 3d_runs
for run_id in $(seq 1 $nb_runs)
do
    compile_one $run_id
done

