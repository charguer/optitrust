#!/bin/bash

# Fix the number of MPI nodes to 1
nb_sockets=1

# Fix the number of runs
nb_runs=1

# paths for compiling pic_barsamian
HDF5_HEADER_PATH=/usr/include/hdf5/openmpi
HDF5_LIBRARY_PATH=/usr/lib/x86_64-linux-gnu/hdf5/openmpi

# To be added to the template:
# nb_threads=2
