#!/bin/sh

export TVM_NUM_THREADS=1 && export OMP_NUM_THREADS=1 && export MKL_NUM_THREADS=1 && taskset -a -c 0 make bench_demo
