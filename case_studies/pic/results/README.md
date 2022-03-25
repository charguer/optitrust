
# Contents of this folder

This folder contains the experimental results for the PIC case study.

In a rush:
```
# Gather machine dependent parameters
./autoconfig.sh
# Gather machine topology information
./bench.sh hard
# Small-scale simulation
FAST=1 ./bench.sh
# Large-scale simulation
./bench.sh
```

There is one folder per machine used, named after the hostname.
Each folder contains the following data.

The file `config_HOSTNAME.sh` needs to exist, to describe the machine parameters
(nb_cores, compilers, nb_particles).
This file can be generated using `./autoconfig.sh foo`, and then possibly modified by hand.

Execute `./bench.sh action` to run only a subset of the benchmark actions.
Execute `FAST=1 ./bench.sh` to use smaller parameters for faster simulation.

The contents of result folder HOSTNAME will be cleared, unless a specific action
is provided to the bench script.


Here is an overview of how it works.


## Information about the hardware

- output of `cat /proc/cpuinfo`
- output of `lstopo`, both in text and image format (requires hwloc)
- performance of the Stream benchmark (both each compiler)


## Parameters used for the evaluation in `config_foo.sh`

- `nb_cores` stores the number of cores available on the hardware
- `compilers` stores the list of available compilers on the hardware (e.g., "gcc icc")
- `nb_particles` storing the number of particles close to the maximum that the RAM can accomodate

This file is used to generate `your_configuration.sh` and `parameters_3d.txt`
which the compilation and execution scripts do rely on.

To determine `$nb_particles`, use the script `./particles.sh`, providing as argument
the number of GB of RAM available, e.g. `./particles.sh 64`. The script takes a 10%
margin to avoid memory swapping. During a run, make sure to watch the RAM usage using `htop`
and kill the process if it comes too close to the maximal RAM available.


## Benchmark results

We store performance figures for:
- pic_demo (unoptimized sequential code)
- pic_optimized
- pic_barsamian (the original code)
- pic_barsamian_malloc (the original code, without manual memory management of chunks)

For each program, we consider the performance both with two compilers: GCC and ICC.

For the parallel code, we consider both sequential runs and runs with the maximal
number of hardware threads (excluding hyperthreading).

The naming pattern is, e.g., `pic_optimized_gcc_p2.txt` for a `gcc` compiled
program executed using 2 cores.
