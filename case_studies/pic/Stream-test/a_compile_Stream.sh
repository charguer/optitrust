#!/bin/bash

# Usage: ./a_compile_Stream.sh compiler size
# - compiler is the name of the compiler to use, gcc or icc
# - size is the number of array cells per core, to use for the benchmark

COMPILER=$1
SIZE=$2


###################################
#        Your configuration       #
###################################
#Home path for Pic-Vert.
cd ..
PICVERT_HOME=$(pwd)

#All your architecture and setup parameters have to be modified in $PICVERT_HOME/your_configuration.sh
source $PICVERT_HOME/your_configuration.sh


###################################
#              gcc                #
###################################

compile_gcc() {
  gcc -DSTREAM_ARRAY_SIZE=$(expr $nb_threads \* $SIZE) -O3 -fopenmp -march=native -mcmodel=medium $PICVERT_HOME/Stream-test/stream.c -o stream.out
}

###################################
#              icc                #
###################################
compile_icc() {
  source /opt/intel/oneapi/setvars.sh > /dev/null 
  icc -DSTREAM_ARRAY_SIZE=$(expr $nb_threads \* $SIZE) -O3 -qopenmp -march=native -mcmodel=medium $PICVERT_HOME/Stream-test/stream.c -o stream.out
}

###################################
#     Compiling the software      #
###################################
cd $PICVERT_HOME
mkdir -p Stream_runs
cd Stream_runs
compile_$COMPILER

