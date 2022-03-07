#!/bin/bash

###################################
#        Your configuration       #
###################################
#Choose the number of similar runs you want to execute (for performance tests)
nb_runs=1
#Choose between icc or gcc
#compiler=icc
compiler=gcc

#icc only: dynamic library path for libiomp5.so (find your right path by typing "locate libiomp5.so" in a terminal, maybe preceded by "updatedb")
# INTEL_OPENMP_DYNAMIC_LIBRARY_PATH=/opt/intel/compilers_and_libraries_2017.0.098/linux/compiler/lib/intel64_lin/

#Choose the number of threads depending on your architecture. Use as many threads as there are cores on each socket of your processor.
#Here assuming a processor with two sockets of 10 cores each.
# nb_threads=10
nb_threads=1
# nb_sockets=2
nb_sockets=1

#Include path for hdf5.h (find your right path by typing "locate hdf5.h" in a terminal, maybe preceded by "updatedb")
HDF5_HEADER_PATH=/usr/include/hdf5/openmpi

#Library path for libhdf5.a (find your right path by typing "locate libhdf5.a" in a terminal, maybe preceded by "updatedb")
HDF5_LIBRARY_PATH=/usr/lib/x86_64-linux-gnu/hdf5/openmpi

#You might have to also add library paths for fftw and zlib.

