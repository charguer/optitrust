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
#Please check the maximum memory on one NUMA node of your architecture, then open Pic-Vert/memory/memory-3d.ods and check the resulting maximum number of particles possible, then change the following line accordingly.
#As example, 2 billions is a maximum for 96 GB of memory.
nb_particles=1000000000

#For quick runs, please lower the number of iterations of the simulation.
nb_iterations=100

#Other things may be modified:
#Modify the compile scripts : modify the -DCHUNK_SIZE= for a different chunk size
#Modify the source code     : modify other simulation parameters directly inside $PICVERT_HOME/simulations/instrum_sim3d_aocosoa.c

###################################
#              gcc                #
###################################
compile_gcc() {
  cd $PICVERT_HOME/3d_heat_runs
  mkdir -p proportion$1-drift$2-vth$3
  export OMPI_CC=gcc
  mpicc -I$PICVERT_HOME/include $PICVERT_HOME/src/matrix_functions.c $PICVERT_HOME/src/meshes.c $PICVERT_HOME/src/output.c $PICVERT_HOME/src/parameter_reader.c $PICVERT_HOME/src/random.c $PICVERT_HOME/src/space_filling_curves.c $PICVERT_HOME/src/diagnostics.c $PICVERT_HOME/src/fields.c $PICVERT_HOME/src/initial_distributions.c $PICVERT_HOME/src/particle_type_concurrent_chunkbags_of_soa_3d.c $PICVERT_HOME/src/poisson_solvers.c $PICVERT_HOME/src/rho.c $PICVERT_HOME/simulations/sim3d_aocosoa.c -DSPARE_LOC_OPTIMIZED -DOMP_TILE_SIZE=2 -DNCX=64 -DNCY=64 -DNCZ=64 -DDELTA_T=0.02 -DINITIAL_DISTRIBUTION=DRIFT_VELOCITIES_3D -DPROPORTION_FAST_PARTICLES=$1 -DDRIFT_VELOCITY=$2 -DTHERMAL_SPEED=$3 -DNB_PARTICLE=$nb_particles -DNB_ITER=$nb_iterations -DCHUNK_SIZE=256 -lfftw3 -lm -O3 -fopenmp -march=native -std=gnu11 -o proportion$1-drift$2-vth$3/aocosoa_3d.out
}

###################################
#              icc                #
###################################
compile_icc() {
  cd $PICVERT_HOME/3d_heat_runs
  mkdir -p proportion$1-drift$2-vth$3
  #Depending on your version, you may change "export OMPI_CC=icc\n  mpicc" by just "mpiicc".
  export OMPI_CC=icc
  mpicc -I$PICVERT_HOME/include $PICVERT_HOME/src/matrix_functions.c $PICVERT_HOME/src/meshes.c $PICVERT_HOME/src/output.c $PICVERT_HOME/src/parameter_reader.c $PICVERT_HOME/src/random.c $PICVERT_HOME/src/space_filling_curves.c $PICVERT_HOME/src/diagnostics.c $PICVERT_HOME/src/fields.c $PICVERT_HOME/src/initial_distributions.c $PICVERT_HOME/src/particle_type_concurrent_chunkbags_of_soa_3d.c $PICVERT_HOME/src/poisson_solvers.c $PICVERT_HOME/src/rho.c $PICVERT_HOME/simulations/sim3d_aocosoa.c -DSPARE_LOC_OPTIMIZED -DOMP_TILE_SIZE=2 -DNCX=64 -DNCY=64 -DNCZ=64 -DDELTA_T=0.02 -DINITIAL_DISTRIBUTION=DRIFT_VELOCITIES_3D -DPROPORTION_FAST_PARTICLES=$1 -DDRIFT_VELOCITY=$2 -DTHERMAL_SPEED=$3 -DNB_PARTICLE=$nb_particles -DNB_ITER=$nb_iterations -DCHUNK_SIZE=256 -lfftw3 -lm -O3 -qopenmp -march=native -std=gnu11 -o proportion$1-drift$2-vth$3/aocosoa_3d.out
}

###################################
#     Compiling the software      #
###################################
cd $PICVERT_HOME
mkdir -p 3d_heat_runs
compile_$compiler 0.00 0.00 0.339
compile_$compiler 0.02 11.0 0.126
compile_$compiler 0.02 13.0 0.178
compile_$compiler 0.02 15.0 0.234
compile_$compiler 0.02 17.0 0.287
compile_$compiler 0.02 20.0 0.355
compile_$compiler 0.02 22.0 0.3585

