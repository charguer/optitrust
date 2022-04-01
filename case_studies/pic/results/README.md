
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


## Experiments

To figure out what CPULIST to use, do:
```
sudo apt-get install hwloc
lstopo
# This shows each socket, with each core, with 1 or 2 PUs (processing unit) depending on hyperthreading
# Read the label of the first PU of each core, only for the first socket, and write them in the list
# e.g. CORES=4 CPULIST="0,1,2,3"
# On NUMA machines make sure to use only one socket.
```

Keep in mind that:

- The number of parallel tasks is GRID^3/8/8  (because 8 phases, with blocs of size 8)
  for GRID=32; this gives 512 tasks. The number of tasks should be at least 10*CORES
  for decent parallelism.

- The number of particles should be at least GRID^3 * 256 * 10,
  to have on average 10 chunks per cell.
  For GRID=32, this gives 84 million particles.
  For GRID=64, this gives 670 million particles.
  because CHUNK_SIZE=256, and the code is optimized for processing several chunks per cell.

- The number of time steps should be at least 20, to tame cache warm-up artefacts;
  Larger values make simulations more realistic, but don't fundamentally change the results.

- The space needed to represent the grid is GRID^3*256*36*4 (factor 4 for spare chunks)
  (factor 36 is the size of a particle with single-precision positions---48 with double precision).
  For GRID=32, this gives 1.2GB. For GRID=64, this gives 10GB.

- The space needed to represent the particles is NB*36 (MB) (36 bytes per particle)
  (ignoring the chunk size headers). For NB=100 (million particles), this gives 3.6GB.
  Round it up to account for chunk overheads, and the auxiliary data structures.

Then:
```
#--------------Laptop

# To get hardware info
CORES=4 CPULIST="0,1,2,3" COMP=gcc ./bench.sh hard

# for a fast run just to check everything is ok
CORES=4 CPULIST="0,1,2,3" COMP=gcc GRID=8 NB=2 STEPS=20 RUNS=2 PROG="pic_barsamian.c pic_optimized.c" ./bench.sh

# to check variance for a fixed seed (add SEED=0 to the front)
SEED=0 CORES=4 CPULIST="0,1,2,3" COMP=gcc GRID=32 NB=20 STEPS=20 RUNS=3 PROG="pic_barsamian.c pic_optimized.c" ./bench.sh

# to check the speedups
CORES=1 CPULIST="1" COMP=gcc GRID=8 NB=5 STEPS=20 RUNS=3 PROG="pic_barsamian_single.c pic_optimized_single.c" ./bench.sh
CORES=4 CPULIST="0,1,2,3" COMP=gcc GRID=8 NB=5 STEPS=20 RUNS=3 PROG="pic_barsamian_single.c pic_optimized_single.c" ./bench.sh


# for a mid-size run
CORES=4 CPULIST="0,1,2,3" COMP=gcc GRID=32 NB=20 STEPS=20 RUNS=3 PROG="pic_barsamian_single.c pic_optimized_single.c" ./bench.sh
CORES=4 CPULIST="0,1,2,3" COMP=gcc GRID=32 NB=20 STEPS=20 RUNS=1 PROG="pic_barsamian_single.c pic_optimized_single.c pic_barsamian_freelist_single.c" ./bench.sh

# for a large-size run
CORES=4 CPULIST="0,1,2,3" COMP=gcc GRID=32 NB=100 STEPS=100 RUNS=3 PROG="pic_barsamian_single.c pic_optimized_single.c pic_barsamian_freelist_single.c" ./bench.sh

# for a large-size sequential run
CORES=1 CPULIST="1" COMP=gcc GRID=8  NB=100 STEPS=100 RUNS=1 PROG="pic_barsamian_single.c pic_optimized_single.c" ./bench.sh
CORES=1 CPULIST="1" COMP=gcc GRID=32 NB=100 STEPS=100 RUNS=1 PROG="pic_barsamian_single.c pic_optimized_single.c" ./bench.sh


#--------------Server

# for a mid-size run
CORES=10 CPULIST="3,7,11,15,19,23,27,31,35,39" COMP=gcc GRID=32 NB=20 STEPS=20 RUNS=3 PROG="pic_barsamian_single.c pic_optimized_single.c" ./bench.sh

# for a large-size run
CORES=10 CPULIST="3,7,11,15,19,23,27,31,35,39" COMP=gcc GRID=32 NB=200 STEPS=100 RUNS=3 PROG="pic_barsamian_single.c pic_optimized_single.c pic_barsamian_freelist_single.c" ./bench.sh

# for one single core run, to see the speedup
CORES=1 CPULIST="1" COMP=gcc GRID=32 NB=100 STEPS=100 RUNS=3 PROG="pic_barsamian_single.c pic_optimized_single.c pic_barsamian_freelist_single.c" ./bench.sh

# for a huge-size run on a big server (not needed to make more steps)
CORES=10 CPULIST="3,7,11,15,19,23,27,31,35,39" COMP=gcc GRID=64 NB=500 STEPS=50 RUNS=1 PROG="pic_barsamian_single.c pic_optimized_single.c pic_barsamian_freelist_single.c" ./bench.sh
```

## Discussion

Barsamian's results:
- 740 million particles per second using 24 cores
- 30.8 million particles per second per core
- The theoretical peak advertised by the manufacturer is 127.99 GB/s.
- The Stream benchmark [15] provides the measure 98.2 GB/s
- The memory bandwidth achieved is 53.6 GB/s.
  The bandwidth is obtained by multiplying the size of a particle, 36 bytes
  (plus 64/256=0.25 bytes to account for chunk headers, we ignore this)
  multiplied by the number of particle processed per second (740 million),
  and multiplied by a factor 2 (one read plus one write).

// Intel Xeon Platinum 8160 @ 2.1 GHz (Skylake),
// with 96 GB of RAM, 6 memory channels, and 24 cores



- teraram on 10 cores: The Stream benchmark [15] provides the measure 22.2 GB/s


# eliminate warning => find any relevant network interfaces
# --mca btl_base_warn_component_unused 0


# Debugging:


```
nb_particles = 10000000;
nb_iterations = 100;
nb_cells_x = 8;
nb_cells_y = 8;
nb_cells_z = 8;
```

```
cd pic/scripts
CORES=1 CPULIST="1"       COMP=gcc ./run.sh pic_barsamian.c
CORES=4 CPULIST="0,1,2,3" COMP=gcc ./run.sh pic_barsamian.c

cd pic/3d_runs/run1
OMP_NUM_THREADS=4 ./pic_barsamian.out parameters_3d.txt
OMP_NUM_THREADS=4 taskset --cpu-list 0,1,2,3 ./pic_barsamian.out parameters_3d.txt

```