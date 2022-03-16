#!/bin/bash

#Home path for Pic-Vert.
cd ..
PICVERT_HOME=$(pwd)

###################################
#        Your configuration       #
###################################
#All your architecture and setup parameters have to be modified in $PICVERT_HOME/your_configuration.sh
source $PICVERT_HOME/your_configuration.sh

###################################
#      Simulation parameters      #
###################################
#Modify the compile scripts : modify the -DSTREAM_ARRAY_SIZE= if you want a different number of cells. Here given for 20M cells per core.

###################################
#              gcc                #
###################################

SIZE=200000000
compile_gcc() {
  gcc -DSTREAM_ARRAY_SIZE=$(expr $nb_threads \* $SIZE) -O3 -fopenmp -march=native -mcmodel=medium $PICVERT_HOME/Stream-test/stream.c -o stream.out
}

###################################
#              icc                #
###################################
compile_icc() {
  icc -DSTREAM_ARRAY_SIZE=$(expr $nb_threads \* $SIZE) -O3 -qopenmp -march=native -mcmodel=medium $PICVERT_HOME/Stream-test/stream.c -o stream.out
}

###################################
#     Compiling the software      #
###################################
cd $PICVERT_HOME
mkdir -p Stream_runs
cd Stream_runs
compile_$compiler

