#!/bin/bash

#Home path for Pic-Vert.
cd ../..
PICVERT_HOME=$(pwd)

###################################
#        Your configuration       #
###################################
#All your architecture and setup parameters have to be modified in $PICVERT_HOME/your_configuration.sh
source $PICVERT_HOME/your_configuration.sh

###################################
#      Simulation parameters      #
###################################
#Please check the maximum memory on one NUMA node of your architecture, then open Pic-Vert/memory/memory-3d.ods and check the resulting maximum number of particles possible, then change the line "nb_particles" in parameters_3d.txt accordingly.
#As example, 2 billions is a maximum for 96 GB of memory.

#Other things may be modified:
#Modify the compile scripts : modify the -DCHUNK_SIZE= for a different chunk size
#Modify other parameters    : modify the other parameters in parameters_3d.txt

###################################
#              gcc                #
###################################
compile_gcc() {
  cd $PICVERT_HOME/3d_runs
  mkdir -p run$1
  cp $PICVERT_HOME/scripts/3d_performance/parameters_3d.txt run$1/
  export OMPI_CC=gcc
  mpicc -I$PICVERT_HOME/include $PICVERT_HOME/src/matrix_functions.c $PICVERT_HOME/src/meshes.c $PICVERT_HOME/src/output.c $PICVERT_HOME/src/parameter_reader.c $PICVERT_HOME/src/random.c $PICVERT_HOME/src/space_filling_curves.c $PICVERT_HOME/src/diagnostics.c $PICVERT_HOME/src/fields.c $PICVERT_HOME/src/initial_distributions.c $PICVERT_HOME/src/particle_type_concurrent_chunkbags_of_soa_3d.c $PICVERT_HOME/src/poisson_solvers.c $PICVERT_HOME/src/rho.c $PICVERT_HOME/simulations/sim3d_aocosoa.c -DSPARE_LOC_OPTIMIZED -DOMP_TILE_SIZE=2 -DCHUNK_SIZE=256 -lfftw3 -lm -O3 -fopenmp -march=native -std=gnu11 -o run$1/aocosoa_3d.out
}

###################################
#              icc                #
###################################
compile_icc() {
  cd $PICVERT_HOME/3d_runs
  mkdir -p run$1
  cp $PICVERT_HOME/scripts/3d_performance/parameters_3d.txt run$1/
  #Depending on your version, you may change "export OMPI_CC=icc\n  mpicc" by just "mpiicc".
  export OMPI_CC=icc
  mpicc -I$PICVERT_HOME/include $PICVERT_HOME/src/matrix_functions.c $PICVERT_HOME/src/meshes.c $PICVERT_HOME/src/output.c $PICVERT_HOME/src/parameter_reader.c $PICVERT_HOME/src/random.c $PICVERT_HOME/src/space_filling_curves.c $PICVERT_HOME/src/diagnostics.c $PICVERT_HOME/src/fields.c $PICVERT_HOME/src/initial_distributions.c $PICVERT_HOME/src/particle_type_concurrent_chunkbags_of_soa_3d.c $PICVERT_HOME/src/poisson_solvers.c $PICVERT_HOME/src/rho.c $PICVERT_HOME/simulations/sim3d_aocosoa.c -DSPARE_LOC_OPTIMIZED -DOMP_TILE_SIZE=2 -DCHUNK_SIZE=256 -lfftw3 -lm -O3 -qopenmp -march=native -std=gnu11 -o run$1/aocosoa_3d.out
}

###################################
#     Compiling the software      #
###################################
cd $PICVERT_HOME
mkdir -p 3d_runs
for run_id in $(seq 1 $nb_runs)
do
    compile_$compiler $run_id
done

