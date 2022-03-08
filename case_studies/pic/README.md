# Relevant files


Unoptimized code, used as input to the OptiTrust script:

```
	include/optitrust.h
	include/mymacros.h
	include/particle.h
	include/bag.h
	include/bag_atomics.h
	src/particle.c
	src/bag.c
   src/bag_atomics.c
	simulations/pic_demo_aux.h
	simulations/pic_demo_aux.c
	simulations/pic_demo.h
	simulations/pic_demo.c
```

Original code optimized by hand:

```
	simulation/pic_barsamian.c
```



# Installation of dependencies

The command to install the compiler libraries

```
sudo apt-get install libopenmpi-dev openmpi-bin libhdf5-openmpi-dev libfftw3-dev
```

Then check the existence of the paths:
```
ls /usr/include/hdf5/openmpi
ls /usr/lib/x86_64-linux-gnu/hdf5/openmpi
```

Versions known to work

```
   gcc 5.5.0-12
   openmpi-common 1.10.2
   libhdf5-openmpi 10_1.8.16
   libfftw3-long3 3.3.4
```

# Check your machine peak performance

```
  cd Stream-test
   ./a_compile_Stream.sh
   ./b_run_Stream.sh
  cd ..
```

Example output (meaning that memcopy is about 30GB/s):
```
Array size = 80000000 (elements), Offset = 0 (elements)
Memory per array = 610.4 MiB (= 0.6 GiB).
Total memory required = 1831.1 MiB (= 1.8 GiB).
-------------------------------------------------------------
Function    Best Rate MB/s  Avg time     Min time     Max time
Copy:           29229.1     0.047055     0.043792     0.048286
Scale:          18802.5     0.077192     0.068076     0.081039
Add:            18946.7     0.116318     0.101337     0.121007
Triad:          20905.0     0.113370     0.091844     0.121651
```


# Configuration file

To check the architecture of your machine, execute `sudo lshw -class memory`

To configure your architecture, execute (and possibly edit the resulting file):
```
cp configuration_small.sh your_configuration.sh
```

To configure the benchmark parameters, execute (and possibly edit the resulting file):

```
 cd scripts/
 cp parameters_3d_small.txt parameters_3d.txt
# or
 cp parameters_3d_medium.txt parameters_3d.txt
# or
 cp parameters_3d_large.txt parameters_3d.txt
```

Don't try the "huge" parameters, it will blow up the machine.
Already with 100 million particles, you need 10GB of RAM.

Make sure to recompile whenever you change the parameter file.


# Compilation

In the folder `scripts/`, execute:

```
./compile.sh pic_barsamian.c
# or
./compile.sh pic_demo.c
# or
make -j2 progs
```

This should create the binary and a copy of the parameters file in the folder:
```
   ls ../../3d_runs/run1
```
--- TODO: if we should use a distinct folder for the results of pic_demo.out,
   please modify the compile and the run script accordingly.

--- TODO: why do we need to compile the program once per run? could we just copy?

# Execution

First, open `htop` is an auxiliary terminal, to visualize CPU utilization.

Then, in the folder `scripts/`, execute:

```
./run.sh pic_barsamian.c
# or
./run.sh pic_demo.c
```

Ignore the warnings (we'll fix them later)
 ```
   Deprecated:  --cpus-per-proc, -cpus-per-proc, --cpus-per-rank, -cpus-per-rank
   Replacement: --map-by <obj>:PE=N, default <obj>=NUMA
 ```

Then wait a little bit.

At the end, the program output information about parameters and execution time on stdout, and also create output files:
```
   ls ../../3d_runs/run1
```

Note that there is an initialization phase that runs on a single core, then there is the multicore execution.

Example output for the "medium" parameters:
```
   Execution time (total)    : 5.06378 s
   Array of particles update : 4.72076 s (93.2261%)
   - Including particle loop : 4.43227 s (87.5289%)
   - Including append        : 0.288494 s (5.69721%)
   Reduction of rho          : 0.0230181 s (0.454564%)
   Poisson solver            : 0.214542 s (4.2368%)
   Nb. particles / s : 3.94962e+07
   Time / particle / iteration : 23.6038 ns
```

Example output for the "large" parameters:

```
Execution time (total)    : 22.1576 s
Array of particles update : 21.8239 s (98.4938%)
- Including particle loop : 21.5378 s (97.2026%)
- Including append        : 0.286097 s (1.29119%)
Reduction of rho          : 0.022112 s (0.0997944%)
Poisson solver            : 0.210075 s (0.948096%)
Nb. particles / s : 9.02625e+07
Time / particle / iteration : 10.9119 ns
```

Interesting is to compute the "Nb. particles / s / core" by dividing the value of "Nb. particles / s" (here ~90million) by the number of cores used (here 4), giving 22.5 million particles processed by core per second. (This is to be compared with 30 million reported in the paper, on a high-performance hardware; note that using more particles helps improving the figure).


# Combined script for compiling and executing

In the folder `/script`, `test.sh` invokes `compile.sh` then `run.sh` on the argument.

```
./test.sh pic_barsamian.c
# or
./test.sh pic_demo.c
```


# Correctness checker

In the folder `/script`, `check.sh` measures the distance (absolute and relative, for positions and speeds)
between the final state of particles associated with two simulations.

```
./check.sh pic_barsamian.c pic_demo.c
```

To achieve this, the simulations are compiled using a flag `CHECKER`,
that adds an identifier information to every particle, and dumps the
state of all particles in a file (in binary format) after the last step.
The program `checker.c` implements a comparison function for two such
binary output files. This program is compiled by the `check.sh` script.




