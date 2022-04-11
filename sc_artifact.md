
This document explains how to reproduce results form the paper submission entitled
"OptiTrust: an Interactive Framework for Source-to-Source Transformations"


# Part 1: Reproduce performance comparison

## Objectives

The paper does not sell performance results. It describes a tool, called
OptiTrust, that can be used for producing a high-performance code via a
transformation script, rather than by writing optimized code by hand.
As a sanity check for our case study, we verified that the performance
of our generated code is comparable or better than the performance of
the corresponding manually optimized code, which we call the baseline,
and comes from Barsamian et al. [30].

**The purpose of the artifact evaluation should thus be
to assess whether our generated code is at least as performant as its
baseline.** The fact that we discovered additional optimizations is
nice, but it is not the matter of our paper. We would have been
perfectly happy with producing code with exactly the same performance
as the baseline.

We ran experiments with the same parameters as Barsamian et al.,
however these simulations run for hours. However, the claim that our code
is at least as performant as its baseline can be checked on mid-size
simulations that run for just a few minutes.

The benchmark results which we obtained on our test machines suggest
that the most critical loop from our code runs faster than in the baseline.
In the performance evaluation section of the paper, we explain the source of
the performance improvement. Essentially, our code loads the particle data
before a conditional, whereas the baseline code loads the data after
the conditional, in each of the two branches.

The more particles are involved in the simulation, the more visible the
performance difference will appear. Reviewers will need to consider
sufficiently large values of the number of particles for reproducing the
performance gains that we report in the paper. However, smaller number
of particles could suffice to verify our claim that our code is at
least as performant as the baseline.

**Reference [30]:**
Y. Barsamian, A. Charguéraud, S. A. Hirstoaga, and M. Mehrenberger,
“Efficient strict-binning particle-in-cell algorithm for multi-core SIMD
processors,” in 24th International Conference on Parallel and Dis-
   tributed Computing (Euro-Par), ser. Lecture Notes in Computer Science,
   vol. 11014. Springer, Cham, 2018, pp. 749–763

## Investment

We expect that some basic performance results can be reproduce in 1 hour.
About 15 minutes to read the present documentation, 15 minutes to install
the required software, then performing 3 runs of less than 10 minutes each.

Full-scale experiments can take several hours to run, but can be executed
in the background, on a multicore server.

Disclaimer: Running in a virtual machine might have impact on performance,
and on the ability to pin the physical cores being used (with `taskset`).


## Dependency installation


### Installation of singularity

```
  sudo apt-get update
  sudo apt-get install -y build-essential libssl-dev uuid-dev libgpgme11-dev \
    squashfs-tools libseccomp-dev wget pkg-config git cryptsetup debootstrap

```

### Installation of go lang

```
  wget https://dl.google.com/go/go1.13.linux-amd64.tar.gz

  sudo tar --directory=/usr/local -xzvf go1.13.linux-amd64.tar.gz

  export PATH=/usr/local/go/bin:$PATH


```

### Installation of singularity

```
  wget https://github.com/singularityware/singularity/releases/download/v3.5.3/singularity-3.5.3.tar.gz

  tar -xzvf singularity-3.5.3.tar.gz

  cd singularity

  ./mconfig

  cd builddir

  make

  sudo make install

```


### Installation of Intel openAPI toolkit (for the ICC compiler)

(Note: does not work on Ubuntu 16.04, which is too old.)

```
   wget -O- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB \
   | gpg --dearmor | sudo tee /usr/share/keyrings/oneapi-archive-keyring.gpg > /dev/null

   echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] \
   https://apt.repos.intel.com/oneapi all main" | sudo tee /etc/apt/sources.list.d/oneAPI.list

   sudo apt update

   sudo apt upgrade

   sudo apt install intel-basekit

   # optional
   sudo apt install intel-hpckit
```

### Installation of OpenMPI (only for GCC compiler, which is not needed to reproduce results)

```
   sudo apt-get install libopenmpi-dev openmpi-bin
```

### Installation of FFTW (used for the Poison solver, which is part of the PIC simulation)

```
   sudo apt-get install libfftw3-dev
```

### Installation of jemalloc (for fast multi-thread allocation)

```
   sudo apt install libjemalloc-dev
```

### Installation of hwloc (strongly recommanded to check your machine topology)

The package `hwloc` provides the command `lstopo` used later on.

```
   sudo apt install hwloc
```


## Benchmarking using a cached copy of our output program:

First of all, run:
```
   # Save the path to the root of the OptiTrust directory
   export OPTITRUST=`pwd`

   cd ${OPTITRUST}/case_studies/pic/results

   ./update_by_copy.sh
```

The effect is to copy our cached file `${OPTITRUST}/demo/pic_demo_single_exp.cpp`
into the file `${OPTITRUST}/case_studies/pic/simulations`.

## Figure out the IDs of the cores that should be assigned to the execution

### Objectives

The goal is to perform the execution by assigning each OMP thread to one
fixed physical core, ignoring hyperthreading processing units.

To that end, we use `lstopo` to view the machine architecture, and we can
read on the image the identifier of the first processing unit associated
with each core, from only one NUMA node (in case there are several).

The goal is to produce a list, such as `CPULIST="0,1,2,3"`.

Note: we certainly don't want to simulate more particles than what we can fit in the RAM. During the simulation, make sure to use `htop` to keep track of the memory consumption.

Note: for multi-socket machines we use only the memory dedicated to one socket, as the intention of the original code is to have separate MPI processes for each socket.


### Using lstopo to find the CPULIST to use

On a laptop with 4 physical cores and 32GB of ram, we invoke on the command line:

```
   lstopo
```

Here is a sample output:

![Numa architecture \label{numa}](sc_artifact_lstopo.png){width=60% height=50%}

In Figure \ref{numa}, we can read the ids of the processing units (PU)
that we want to use: `CPULIST="0,1,2,3"`.


## Selection of the grid size and number of particles

The size of the simulation grid is controlled by the parameter `GRID`,
which denotes the size of each of the sides of the grid.
E.g. `GRID=32` or `GRID=64`.

The number of particles simulated is controlled by the parameter `NB`,
which denotes the number of million particles.
E.g. `NB=200` or `NB=2000`.


The simulation parameters may be bounded by the capabilities of your hardware.

- If you have < 32GB of RAM, you can use only GRID=32.
- If you have >= 32GB of RAM, you should use GRID=64.

- If you have 16GB or 32GB of RAM, you should use NB=200.
- If you have 96GB of RAM or more, you can use NB=1000 or NB=2000.

The combinations of paramters used in the paper are:

- GRID=32 NB=200
- GRID=64 NB=200
- GRID=64 NB=2000


## Execution of the benchmark script

Runs are executed from the `results` folder:
```
   cd ${OPTITRUST}/case_studies/pic/results
```

An example command line, just to make sure that everything runs fine.

```
   CORES=4 CPULIST="0,2,4,6" COMP=icc GRID=8 NB=2 STEPS=10 RUNS=2 \
    PROG="pic_barsamian_single.c pic_optimized_single.c" ./bench.sh
```

where:

- `CPULIST` is the list determined above
- `CORES` is the number of the physical cores, it should match the length of `CPULIST`.
- `COMP` is the compiler argument, use `icc` (or `gcc` if you don't have `icc`).
- `STEPS` is the number of steps performed in the simulation, use `100`.
- `RUNS` is the number of runs to perform, use `10` (or fewer if time is limited).
- `SEED` if you want to use the same random seed for every run, as opposed to using a different one for each run (e.g. `SEED=0`).


Note that our code, like Barsamian's code, initializes particles using a single core.
Overall, the initialization process generally takes the same order of magnitude of
time as the parallel processing of the particles.


## Reproducing performance results from the paper (Figure 9)

We provide the three command lines which we used, for each of our 3 machines.
Make sure to adjust the CPULIST as explained previously.
You can adjust the number of runs.
You can also reduce the number of particles, if you RAM is limited.

As explained further on, throughput results can vary very significantly
from a machine to the next.
Larger number of particles lead to better throughput, because the time
needed to process the grid at each time step becomes relatively smaller.


Machine #3: Example command line for a 4-core laptop with 32GB RAM or more.
You can reduce to RUNS=3. Possibly set an arbitrary seed, e.g. SEED=42.
A run typically takes less than 10 minutes.

```
   CORES=4 CPULIST="0,2,4,6" COMP=icc GRID=64 NB=200 STEPS=100 RUNS=10 \
    PROG="pic_barsamian_single.c pic_optimized_single.c" ./bench.sh
```

Machine #2: Example command line for a 40-core server, with 4 NUMA nodes of 10 cores each.

```
   CORES=10 CPULIST="1,3,5,7,9,11,13,15,17,19" \
    COMP=icc GRID=32 NB=200 STEPS=100 RUNS=10 \
    PROG="pic_barsamian_single.c pic_optimized_single.c" ./bench.sh
```

Machine #1:
Example command line for a 36-core chip, with 2 NUMA nodes of 18 cores each.
With `NB=2000`, a single evaluation can take an hour.
With 10 runs it can take a full day.
You can reduce to RUNS=3. Possibly set an arbitrary seed, e.g. SEED=42.

```
   CORES=18 CPULIST="1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35" \
    COMP=icc GRID=64 NB=2000 STEPS=100 RUNS=10 \
    PROG="pic_barsamian_single.c pic_optimized_single.c" ./bench.sh
```


## Sample output

At the end, the bench script displays a summary of the results.
Each row contains four columns:

- the execution time (in seconds),
- the throughput (in million particles per second),
- the throughput per core (in million particles per second per core),
- the simulation arguments, and the program name: `pic_barsamian` is the baseline, `pic_optimized` is ours.


```

====Summary : Exectime / Throughput / ThroughputPerCore / Run =====
255.789	78.2	19.5 host/results_cores4_icc_grid64_nb200_steps100_seed0_run0_pic_barsamian_single.txt
220.433	90.7	22.7 host/results_cores4_icc_grid64_nb200_steps100_seed0_run0_pic_optimized_single.txt
264.873	75.5	18.9 host/results_cores4_icc_grid64_nb200_steps100_seed1_run1_pic_barsamian_single.txt
225.921	88.5	22.1 host/results_cores4_icc_grid64_nb200_steps100_seed1_run1_pic_optimized_single.txt
270.197	74.0	18.5 host/results_cores4_icc_grid64_nb200_steps100_seed2_run2_pic_barsamian_single.txt
229.249	87.2	21.8 host/results_cores4_icc_grid64_nb200_steps100_seed2_run2_pic_optimized_single.txt

```

On the sample results above, between the first two runs, the throughput increases from
78.2 to 90.7, a 16% improvement.

The execution time values are highly dependent on the hardware used. Likewise for the throughput values.
The relative difference between the two code might also vary, depending on the relative
performance of computation power vs memory bandwidth of the hardware.

For throughput per core, values in the range 10 to 30 million particles per second per core can be expected.
For the ratio Ours/Orig, that is, `pic_optimized/pic_barsamian`, values in the range 0% to 20% can be expected.

The claim of the paper is the ability to produce code that is equivalent or better
to the manually optimized code. Technically, even small negative values, e.g. -3%,
would not compromise the conclusions of the paper.


## Stream benchmark performance

The PIC simulation is memory bound. Hence, to interpret the performance, Barsamian et al.
compare the throughput of the program relative to the peak memory bandwidth that can be
achieved in practice. They report achieving 55% of that "practical peak".

Here is how to run the stream benchmark. It takes only a few seconds.

```
   cd ${OPTITRUST}/case_studies/pic/Stream-test
   COMP=icc SIZE=20000000 CORES=4 CPULIST="0,1,2,3" ./stream.sh
```

Sample output:

```
Function    Best Rate MB/s  Avg time     Min time     Max time
Copy:           24197.4     0.053340     0.052898     0.055104
Scale:          16835.2     0.076476     0.076031     0.077138
Add:            19053.1     0.101586     0.100771     0.103492
Triad:          18918.7     0.102154     0.101487     0.103861
```

Reading, e.g., the first value for "Add" gives 19GB/s, using 4-cores.
Consider, on the same machine, a simulation with 4 cores that achieves a
total throughput of 100 million particles per second. Because each particle
takes 36 bytes, and needs to be read and written at least once at each
step, the throughput is at least 72*100m = 7.2GB/s, which amounts to 38%
of the practical peak.

Note that here we only take into account memory bandwidth associated with
particle manipulation, and not that related to the manipulations on the grid.
To observe a higher percentage closer to 55%, one needs to run simulations
with 2 million particles, so that the time spent on manipulations of the
grid become negligeable.


# Part 2: Execution of the transformation scripts

# Introduction

In this second part, we explain how to install the tooling for generating,
using OptiTrust, our `pic_optimized` program, starting from the totally
unoptimized code, which corresponds to the file
`${OPTITRUST}/case_studies/pic/simulations/pic_demo.c`.

It takes about 30 minutes to install the required OCaml software.
Then, running the transformation script and checking its output should
take no more than 2 minutes.


We first list the OCaml packages required, and then detail further on the instructions
for installing those packages using opam, the package manager for OCaml.

- OCaml compiler (tested with version 4.12.0)
- dune (tested with version 2.9.1)
- clangml (https://gitlab.inria.fr/tmartine/clangml) (tested with version
  4.5.0)
- [pprint](https://github.com/fpottier/pprint) (tested with version 20220103)
- menhir and menhirLib (must be version 20210419, otherwise it won't work)

Besides, we also require:

- several system packages that are needed by the OCaml package (they are listed further on)
- gcc (tested with version 9.4.0, but any version should work)
- clang-format (tested with version 10.0.0, but any version should work)


# Installation steps

### Installation of system packages

```
   sudo apt-get install clang-format meld libclang-dev llvm-dev libomp-dev pkg-config zlib1g-dev
```

### Installation and configuration of opam (OCaml packet managers)

(For explanations, see https://opam.ocaml.org/doc/Install.html )

Unfortunately, this takes a little while (typically 15-20 minutes), because the opam
package manager compiles the OCaml compiler and all its packages from sources.

```
   sudo apt-get install opam
   opam init
   opam switch create 4.12.0
   opam pin add menhirLib 20210419
   opam pin add pprint 20220103
   opam install dune clangml pprint menhir menhirLib base64 ocamlbuild
```

### Compilation and installation of OptiTrust, and copy of the pic-demo source files

It is necessary to export the path to the OptiTrust folder.

```
   # From the root folder of OptiTrust
   export OPTITRUST=`pwd`
```

All the commands from the following steps are to be executed in the `/demo` folder.

```
   cd demo
```

To compile and install OptiTrust

```
   make init
```

### Execution of our transformation script

This command generates `pic_demo_single_out.cpp`, which should be the same
as `pic_demo_single_exp.cpp`, which used for benchmarking under the name
`pic_optimized_single.c` in the `pic/simulation/` folder.

```
   make optim_single
```

On a recent laptop, the command takes about 10 seconds to execute.

To check that the file produced matches the one used, you can use the command:

```
   diff --ignore-blank-lines --ignore-all-space pic_demo_single_out.cpp pic_demo_single_exp.cpp
```

### Generation of the HTML page to browse through the steps

```
   make trace
```

Then open the output file:

```
   chromium-browser pic_demo_trace.html
```

Use buttons to navigate in the big-steps, on in the small steps.


# Part 3: Interactive usage of OptiTrust

It will probably take about 20 minutes to install and configure VScode.
Feel free to skip this part, which is mostly about setting keybindings,
and has little scientific interest.


### Installation of chromium-browser

Our scripts assume this browser, which opens up pages very fast.

```
   sudo apt-get install chromium-browser
```


### Installation of VSCode

Click [here](https://go.microsoft.com/fwlink/?LinkID=760868 "Download vscode") file
and then move to the directory that contains the file and run:

```
   sudo dpkg -i code_something.deb
```

We used Visual Studio Code version 1.63.2, but any recent version should work.


### Configuration of VScode

We provide key bindings for Visual Studio Code (VSCode).

To execute a VSCode task, one may use a keyboard shortcut. This shortcut triggers a
task defined in `.vscode/tasks.json`, which is part of the repository, unlike
`keybindings.json`, where bindigs are defined, and which is a user-specific
configuration file.

In the root folder of OptiTrust, run `code . &` to open VSCode.
To edit the `keybindings.json` file from Visual Studio Code,
type `Ctrl + Shift + P` to access the command panel and then choose
"Preferences: Open Keyboard Shortcuts (JSON)". There, replace the empty
square braces `{}` with the following contents:

If you want to see just a few diffs, the key binding for "F6" is sufficient.

```
{
  {
    "key": "f6",
    "command": "workbench.action.tasks.runTask",
    "args": "View diff",
  },
  {
    "key": "alt+f6",
    "command": "workbench.action.tasks.runTask",
    "args": "View big step diff",
  },
  {
    "key": "shift+alt+f6",
    "command": "workbench.action.tasks.runTask",
    "args": "View trace",
  },
  {
    "key": "f7",
    "command": "workbench.action.tasks.runTask",
    "args": "Execute from intermediate state",
  },
  {
    "key": "alt+f7",
    "command": "workbench.action.tasks.runTask",
    "args": "Execute a big step from intermediate state",
  },
  {
    "key": "ctrl+f7",
    "command": "workbench.action.tasks.runTask",
    "args": "Save intermediate state",
  },
}
```

Note: on Ubuntu, `Alt+F6` is bound to a window moving operation,
it needs to be deactivated in the settings panel, shortcut tab,
'window' group of shortcuts.

### Launching the background task handler

We cannot rely directly on VSCode for executing our scripts,
because VSCode does not support well scripts that interact
with the window manager. For this reason, we use a separate
process, running in a terminal, that waits for queries that
are issued by the VSCode shortcut through an intermediate file.

```
   # In a fresh terminal, execute:
   ${OPTITRUST}/.vscode/watch.sh
```

This program will wait and print and execute the queries
that are sent from VScode.


### Working in VSCode and trying interactive shortcuts

Here is how to open our transformation script:

```
   cd ${OPTITRUST}
   code demo/pic_demo.ml &
```

### Instructions on how to use the script interactively

To execute a given step, select a line starting with '!!' among the
first few ones, type `F6` on this should open a browser with the diff.

To work on a selected big-step from the script, it is useful to
save a local checkpoint. To that end, select a line starting with
"bigstep", type `CTRL+F7`, this saves a checkpoint for that line.
Then, select a line starting with '!!' among the lines that follow
the selected big-step, and type `F7`, this should show the diff,
faster than using F6 which executes everything from the start.


# Part 4: Correctness checker

### Instructions on how to use the correctness checker

We provide means of comparing our output program
(using double precision positions) against the original program,
by checking that particle positions and speeds match.
The programs are compiled using a `ifdef` that adds identifiers
to every particles, and dumps their state at the end.

Here is an example command for testing using 4 cores.

```
   cd ${OPTITRUST}/demo
   CORES=4 CPULIST="0,2,4,6" COMP=icc make chk
```

Sample output:

```
   Maximal dist pos: 7.10543e-15
   Maximal dist pos relative to area width: 3.22974e-16
   Maximal dist speed: 1.35178e-15
   Maximal dist speed relative to maximal speed: 2.5524e-16
```

Simulation parameters are controlled `template_parameters_3d_checker.txt`,
where you can increase the grid size, the number of particles, or
the number iterations as desired.

Typical values for deviations in large simulations are in the order of 10^-13.

The source code for the checker may be found in the file:
```
   ${OPTITRUST}/case_studies/pic/scripts/checker.c
```


# Appendix: Reference of the machine used for the benchmark


## Compiler options

The compilation command is generated by `${OPTITRUST}/case_studies/pic/scripts/compile.sh`.

It includes, for programs compiled using icc:

```
    source /opt/intel/oneapi/setvars.sh > /dev/null
    OPTIONS="-lfftw3 -lm -O3 -march=native -std=gnu11"
    mpiicc ${OPTIONS} ${FILES} -qopenmp -qopt-report-phase=vec -qopt-report=1 -o theprogram.out
```

## Execution options

The execution command is generated by `${OPTITRUST}/case_studies/pic/scripts/run.sh`.

It includes, for programs compiled using icc:

```
   export LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libjemalloc.so
   source /opt/intel/oneapi/setvars.sh > /dev/null
   export LD_LIBRARY_PATH=$INTEL_OPENMP_DYNAMIC_LIBRARY_PATH:$LD_LIBRARY_PATH
   export KMP_AFFINITY=granularity=fine,compact,1,0,verbose
   taskset --cpu-list ${CPULIST} ./theprogram.out
```


## Machine (1), with a 18-core socket.

Machine #1 is a 36-core machine, with two sockets.
Each socket hosts a 18-core Intel Xeon Gold 6240 chip, running at 2.60GHz, with 96GB of RAM.

The system is CentOS Linux release 7.6.1810 (Core), 3.10.0-957.el7.x86_64
The version of ICC is icc version 19.0.4.243


## Machine (2), with a 10-core socket.

Machine #2 is a 20-core machines, with two sockets.
Each sockets hosts a 10-core Intel(R) Xeon(R) CPU E5-2650 v3 chip, running at 2.30GHz, with 16GB of RAM per socket.

The system is Ubuntu 18.04.6 LTS (binonic), 4.15.0-143-generic
The version of ICC is icc version 2021.5.0 (gcc version 11.1.0 compatibility).

## Machine (3), with a 4-core socket.

Machine #3 is a 4-core laptop, with a single socket.
The socket hosts a Intel(R) Core(TM) i7-8665U CPU, running at 1.90GHz, with 32GB of RAM.

The system is Ubuntu 20.04.4 LTS(focal), 5.4.0-80-generic
The version of ICC is 2021.5.0.

