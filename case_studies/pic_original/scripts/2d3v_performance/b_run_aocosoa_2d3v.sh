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

  cd $PICVERT_HOME/2d3v_runs/run$1
  export OMP_NUM_THREADS=$nb_threads
  mpirun --report-bindings --cpus-per-proc $nb_threads -np $nb_sockets ./aocosoa_2d3v.out ./parameters_2d3v.txt | tee ./std_output_run$1.txt
}

###################################
#              icc                #
###################################
export LD_LIBRARY_PATH=$INTEL_OPENMP_DYNAMIC_LIBRARY_PATH:$LD_LIBRARY_PATH
run_icc() {
  #Threads-to-cores binding.
  export KMP_AFFINITY=granularity=fine,compact,1,0,verbose

  cd $PICVERT_HOME/2d3v_runs/run$1
  export OMP_NUM_THREADS=$nb_threads
  mpirun --report-bindings --cpus-per-proc $nb_threads -np $nb_sockets ./aocosoa_2d3v.out ./parameters_2d3v.txt | tee ./std_output_run$1.txt
}

###################################
#     Launching the software      #
###################################
for run_id in $(seq 1 $nb_runs)
do
    run_$compiler $run_id
done

