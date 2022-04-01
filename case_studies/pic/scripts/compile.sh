#!/bin/bash

# Usage:  ./compile.sh ${TARGET} [${CHECKER_OUTFILE}]
#  where ${TARGET} is a C filename from the folder simulations/
#  where ${CHECKER_OUTFILE} is an optional filename argument,
#        where to output particles in the final state;
#        If ${CHECKER_OUTFILE} is the string "1", then the filename
#        is automatically generated from the target

# Note: this script also takes care of copying the "parameters_3d.txt" file
# into the run folder
# Option VERBOSE=1

TARGET=$1
# BASENAME=`basename ${TARGET} .c`
BASENAME="${TARGET%%.*}"
EXTENSION="${TARGET##*.}"
CHECKER_OUTFILE=$2
echo $CHECKER_OUTFILE

if [ "$CHECKER_OUTFILE" == "1" ]; then
  CHECKER_OUTFILE="${BASENAME}.res"
fi

#Home path for Pic-Vert.
cd ..
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

# read compiler name from environment variable, else from your_configuration.sh
COMPILER="${compiler}"
if [ ! -z "${COMP}" ]; then
  COMPILER="${COMP}"
fi


# uses VECTINFOS from the environment, see vectinfo.sh

# possible additional flags -DPRINTPARAMS -DDEBUG_CHECKER -DDEBUG_CHARGE -DDEBUG_FIELD -DDEBUG_ACCEL -DDEBUG_CREATION
# more flags: -DPRINTPERF -DPRINTSTEPS

DEBUGFLAGS=""
# DEBUGFLAGS="-g -DSKIPLEAPFROG"
# DEBUGFLAGS="-g -DDEBUG_ITER"
# DEBUGFLAGS="-g"
DEBUGFLAGS="-DDEBUG_SEED"

# -ftree-vectorize
PERFFLAGS=" -DPRINTPERF -DPRINTSTEPS"

BINARY="${BASENAME}.out"

if [ ! -z "$CHECKER_OUTFILE" ]; then
  CHECKER=" -DCHECKER=$CHECKER_OUTFILE "
  BINARY="${BASENAME}_checker.out"
fi

OPTIMLEVEL="-O3"
if [ ! -z "$COMPOPT" ]; then
  OPTIMLEVEL="$COMPOPT"
fi

COMPILEINFOS="Build ${BINARY} with ${COMPILER} ${OPTIMLEVEL} ${DEBUGFLAGS} ${PERFFLAGS} ${CHECKER}"

echo ${COMPILEINFOS}

EXTRA_SPECIFIC_PRE=

# Barsiamian's code
BARSAMIAN_COMMON="$PICVERT_HOME/src/particle_type_concurrent_chunkbags_of_soa_3d.c"
if [ "${TARGET}" = "pic_barsamian.c" ]; then
    EXTRA_SPECIFIC_PRE="${BARSAMIAN_COMMON} -DPOSTYPEDOUBLE -DSTDCHUNKALLOC"
elif [ "${TARGET}" = "pic_barsamian_freelist.c" ]; then
    EXTRA_SPECIFIC_PRE="${BARSAMIAN_COMMON} -DPOSTYPEDOUBLE"
elif [ "${TARGET}" = "pic_barsamian_single.c" ]; then
    EXTRA_SPECIFIC_PRE="${BARSAMIAN_COMMON} -DSTDCHUNKALLOC"
elif [ "${TARGET}" = "pic_barsamian_freelist_single.c" ]; then
    EXTRA_SPECIFIC_PRE="${BARSAMIAN_COMMON}"

# Compilation of naive code (pic_demo.c)
elif [ "${TARGET}" = "pic_demo.c" ]; then
    EXTRA_SPECIFIC_PRE=" $PICVERT_HOME/src/optitrust.c $PICVERT_HOME/src/particle.c $PICVERT_HOME/src/bag_atomics.c $PICVERT_HOME/src/bag.c $PICVERT_HOME/simulations/pic_demo_aux.c"
else
# Compilation of a version of pic_demo.c produced by OptiTrust
    EXTRA_SPECIFIC_PRE="  $PICVERT_HOME/src/optitrust.c $PICVERT_HOME/src/bag_atomics.c  $PICVERT_HOME/simulations/pic_demo_aux.c"
fi

if [ "${EXTENSION}" = "c" ]; then
  CSTANDARD="-std=gnu11"
else
  CSTANDARD="-xc++"
  #-std=gnu++11
fi


COMPILE_ARGS="-I$PICVERT_HOME/include -I $PICVERT_HOME/simulations $PICVERT_HOME/src/matrix_functions.c $PICVERT_HOME/src/meshes.c $PICVERT_HOME/src/output.c $PICVERT_HOME/src/parameter_reader.c $PICVERT_HOME/src/random.c $PICVERT_HOME/src/space_filling_curves.c $PICVERT_HOME/src/diagnostics.c $PICVERT_HOME/src/fields.c $PICVERT_HOME/src/initial_distributions.c $PICVERT_HOME/src/poisson_solvers.c $PICVERT_HOME/src/rho.c  $EXTRA_SPECIFIC_PRE  $PICVERT_HOME/simulations/${BASENAME}.${EXTENSION} $EXTRA_SPECIFIC_POST -DSPARE_LOC_OPTIMIZED -DOMP_TILE_SIZE=2 -DCHUNK_SIZE=$CHUNK_SIZE $CHECKER $DEBUGFLAGS $PERFFLAGS -lfftw3 -lm ${OPTIMLEVEL} ${VECTINFOS} -march=native ${CSTANDARD} "

if [ ! -z "${VERBOSE}" ]; then
  echo "EXTRA_SPECIFIC_PRE=${EXTRA_SPECIFIC_PRE}"
  echo "gcc ${COMPILE_ARGS}"
fi

# Depending on your version, you may change "export OMPI_CC=icc\n  mpicc" by just "mpiicc".
compile_one() {
  id_run=$1
  cd $PICVERT_HOME/3d_runs
  mkdir -p run${id_run}

  # copy of parameters is redundant with that done in run.sh but nevermind
  cp $PICVERT_HOME/scripts/parameters_3d.txt run${id_run}/
  if [ "${COMPILER}" = "gcc" ]; then
    export OMPI_CC=gcc
    mpicc ${COMPILE_ARGS} -fopenmp -o run${id_run}/${BINARY}
  elif [ "${COMPILER}" = "icc" ]; then
    #export OMPI_CC=/opt/intel/oneapi/compiler/latest/linux/bin/intel64/icc
    source /opt/intel/oneapi/setvars.sh > /dev/null
    #-qopt-report=vec -qopt-report=1
    mpiicc ${COMPILE_ARGS} -qopenmp -o run${id_run}/${BASENAME}.out
  else
    echo "invalid compiler parameter: ${COMPILER}."
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

