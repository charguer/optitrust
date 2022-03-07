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
#              gcc                #
###################################
run_gcc() {
  #Threads-to-cores binding.
  export OMP_PLACES=cores
  export OMP_PROC_BIND=close
  
  export OMP_NUM_THREADS=$nb_threads
  ./stream.out | tee ./std_output.txt
}

###################################
#              icc                #
###################################
export LD_LIBRARY_PATH=$INTEL_OPENMP_DYNAMIC_LIBRARY_PATH:$LD_LIBRARY_PATH
run_icc() {
  #Threads-to-cores binding.
  export KMP_AFFINITY=granularity=fine,compact,1,0,verbose
  
  export OMP_NUM_THREADS=$nb_threads
  ./stream.out | tee ./std_output.txt
}

###################################
#     Launching the software      #
###################################
cd $PICVERT_HOME/Stream_runs
run_$compiler $nb_threads

