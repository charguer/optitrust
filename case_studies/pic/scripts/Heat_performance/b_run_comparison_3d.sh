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
#              gcc                #
###################################
run_gcc() {
  #Threads-to-cores binding.
  export OMP_PLACES=cores
  export OMP_PROC_BIND=close

  cd $PICVERT_HOME/3d_heat_runs/proportion$1-drift$2-vth$3
  export OMP_NUM_THREADS=$nb_threads
  mpirun --report-bindings --cpus-per-proc $nb_threads -np $nb_sockets ./aocosoa_3d.out | tee ./std_output.txt
}

###################################
#              icc                #
###################################
export LD_LIBRARY_PATH=$INTEL_OPENMP_DYNAMIC_LIBRARY_PATH:$LD_LIBRARY_PATH
run_icc() {
  #Threads-to-cores binding.
  export KMP_AFFINITY=granularity=fine,compact,1,0,verbose

  cd $PICVERT_HOME/3d_heat_runs/proportion$1-drift$2-vth$3
  export OMP_NUM_THREADS=$nb_threads
  mpirun --report-bindings --cpus-per-proc $nb_threads -np $nb_sockets ./aocosoa_3d.out | tee ./std_output.txt
}

###################################
#     Launching the software      #
###################################
run_$compiler 0.00 0.00 0.339
run_$compiler 0.02 11.0 0.126
run_$compiler 0.02 13.0 0.178
run_$compiler 0.02 15.0 0.234
run_$compiler 0.02 17.0 0.287
run_$compiler 0.02 20.0 0.355
run_$compiler 0.02 22.0 0.3585

