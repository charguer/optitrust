
A clean PDF version of this document is available from:
http://www.chargueraud.org/research/2022/sc_optitrust/

This document explains how to reproduce results from the paper submission entitled
"OptiTrust: an Interactive Framework for Source-to-Source Transformations".

# Part 1: Reproduction of the performance comparison

## Objectives

The paper does not sell performance results. It describes a tool, called
OptiTrust, that can be used for producing a high-performance code via a
transformation script, rather than by writing optimized code by hand.
As a sanity check for our case study, we verified that the performance
of our generated code is comparable or better than the performance of
the corresponding manually optimized code, which we call the baseline,
and which comes from Barsamian et al. [30].

**The purpose of the artifact evaluation should thus be
to assess whether our generated code is at least as performant as its
baseline.**

We ran experiments with the same simulation parameters as Barsamian et al.
However, running those simulations take several hours.
The artifact reviewer may not have access to such a hardware, or may
not be ready to invest that much time. Fortunately, if needed, the claim that
our code is at least as performant as its baseline can be checked on a
modern laptop, using mid-size simulations that run for just a few minutes.

The benchmark results which we obtained on our test machines suggest
that the most critical loop from our code runs faster than in the baseline.
In the performance evaluation section of the paper, we explain the source of
the performance improvement. Essentially, our code loads the particle data
before a conditional, whereas the baseline code loads the data after
the conditional, in each of the two branches.
The fact that we discovered additional optimizations is
nice, but it is not the matter of our paper. We would have been
perfectly happy with producing code with exactly the same performance
as the baseline.

The more particles are involved in the simulation, the more visible the
performance difference will appear. Reviewers will need to consider
sufficiently large values of the number of particles for reproducing the
performance gains that we report in the paper. However, smaller number
of particles should suffice to verify our claim that our code is at
least as performant as the baseline.

**Reference [30]:**
Y. Barsamian, A. Charguéraud, S. A. Hirstoaga, and M. Mehrenberger,
“Efficient strict-binning particle-in-cell algorithm for multi-core SIMD
processors,” in 24th International Conference on Parallel and Dis-
   tributed Computing (Euro-Par), ser. Lecture Notes in Computer Science,
   vol. 11014. Springer, Cham, 2018, pp. 749–763


## Updated results for machine #3

For machine #3, the results that we reported in the submission are:

- Orig: 18.2 million particles per second per core,
- Ours: 21.2, which corresponds to a 16.5% improvement.

We realized after the deadline that for this machine we had not been using
the right `--cpu-list` argument to `taskset`. Indeed, a coauthor had been
confused by the new version of `lstopo` which, unlike previous versions,
displays to the the front for each processing unit the
logical identifier instead of the physical identifier. We re-ran the
experiment using the appropriate `--cpu-list` and obtained faster executions,
as expected. The updated results that we measured are:

- Orig: 24.5 million particles per second per core,
- Ours: 27.7, which corresponds to a 13.4% improvement.

Note that these throughput values are very close to the results from machine (1),
for which we report 23.2 and 27.6 million particles per second per core,
for Orig. and Ours, respectively, (for the same grid, but with more particles).

The fact that our initial throughput figures were lower does not affect
the conclusions of our experiments. Our optimized code remains at least as
performant as the baseline code. The fact that the relative gap is slighly
reduced (from 16.5% down to 13.4%) does not matter here. It can be explained
by the fact that when two programs run faster, the memory bus becomes more
of a bottleneck for both programs, hence the relative performance between
the two programes reduces.

Please feel free to share the above explanations with the paper reviewers,
if deemed appropriate.


## Investment

We expect that some basic performance results can be reproduced in less than
1 hour. About 15 minutes to read the present documentation, 15 minutes to
install the required software, then performing 3 runs of less than 10 minutes each.

We provide two ways of installing the required software: either using a
Singularity container, or by installing system packages. The first option
requires downloading a 4GB image. The second option involves a slighly
larger number of commands, but could be appealing especially if a good share
of the required software (ICC, openmpi, hwloc, jemalloc) is already available
on the reviewer's machine.

Full-scale experiments can take several hours to run, but can be executed
in the background, on a multicore server.
We do not expect a significant performance dropout when executing the programs
using the container shell, compared with not using a container.



## First option: dependency installation using Singularity

### Installation of Singularity using system packages

Try the following commands to install Go and Singularity.
If they don't work for you, you can get them from sources as detailed further below.

```
   sudo apt-get install golang-go

   sudo wget -O- http://neuro.debian.net/lists/xenial.us-ca.full \
    | sudo tee /etc/apt/sources.list.d/neurodebian.sources.list
   sudo apt-key adv --recv-keys --keyserver hkp://pool.sks-keyservers.net:80 0xA5D32F012649A5A9
   sudo apt-get update

   sudo apt-get install singularity-container
```


### Alternative: installation of Singularity from sources

Installation of system packages required for Singularity:

```
  sudo apt-get update
  sudo apt-get install build-essential libssl-dev uuid-dev libgpgme11-dev \
    squashfs-tools libseccomp-dev wget pkg-config git cryptsetup debootstrap
```

Installation of Go lang from sources:

```
  wget https://dl.google.com/go/go1.13.linux-amd64.tar.gz
  sudo tar --directory=/usr/local -xzvf go1.13.linux-amd64.tar.gz
  export PATH=/usr/local/go/bin:$PATH
```

Installation of Singularity from sources:

```
  wget https://github.com/singularityware/singularity/releases/download/v3.5.3/singularity-3.5.3.tar.gz
  tar -xzvf singularity-3.5.3.tar.gz
  cd singularity
  ./mconfig
  cd builddir
  make
  sudo make install
```


## Second option: dependency installation without Singularity

### Installation of ICC

ICC is provided by Intel openAPI toolkit.

```
   wget -O- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB \
   | gpg --dearmor | sudo tee /usr/share/keyrings/oneapi-archive-keyring.gpg > /dev/null

   echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] \
   https://apt.repos.intel.com/oneapi all main" | sudo tee /etc/apt/sources.list.d/oneAPI.list

   sudo apt update

   sudo apt install intel-hpckit
```

Note: the above commands do not work on Ubuntu 16.04, which is too old.

### Installation of FFTW

This library is used by the Poison solver, which is part of the case study.

```
   sudo apt-get install libfftw3-dev
```

### Installation of jemalloc

Jemalloc is an alternative to standard malloc, for fast multi-thread allocation.

```
   sudo apt install libjemalloc-dev
```

### Installation of hwloc

The package `hwloc` provides the command `lstopo` used later on
for viewing your machine topology, and gather the identifiers
of the cores to use for the execution.

```
   sudo apt install -y hwloc
```

### Optional: installation of OpenMPI

In case you are not able to install ICC, you can compile the code
using GCC. We observed, however, poorer performance when using GCC.

```
   sudo apt-get install libopenmpi-dev openmpi-bin
```


## Set up OptiTrust and the benchmark program

Make sure to execute all the commands using the same shell,
else you will need to rebind the environment variable OPTITRUST.

First of all, download and extract the archive that contains the source code
and the benchmark scripts:

```
   wget http://www.chargueraud.org/research/2022/sc_optitrust/optitrust.tar.gz
   tar -xzf optitrust.tar.gz
```

Then, set the environment variable for the rest of the experiments:

```
   cd optitrust
```

Then, in this `optitrust` folder, if you wish to use our Singularity image
that packages the necessary dependencies, download our singularity image,
and launch a shell for our singularity container.

```
   singularity pull library://begatim01/bench/optitrust.sif:latest
   singularity shell optitrust.sif_latest.sif
```

Regardless of whether you use Singularity or not, execute the following
instructions to set the environment variable, navigate to the benchmark
directory.

```
   export OPTITRUST=`pwd`
   cd ${OPTITRUST}/case_studies/pic/results
```

There remains to copy our cached file `${OPTITRUST}/demo/pic_demo_single_exp.cpp`
into the file `${OPTITRUST}/case_studies/pic/simulations`. This command allows
benchmarking our generated code independently of the process of generating
our code (this process is explained further on).

```
   ./update_by_copy.sh
```


## Figure out the IDs of the cores that should be assigned to the execution

### Objectives

The goal is to perform the execution by assigning each OpenMP thread to one
fixed physical core, ignoring hyper-threading processing units.

To that end, we use `lstopo` to view the machine architecture, and we can
read on the image the identifier of the first processing unit associated
with each core, from only one NUMA node (in case there are several).

The goal is to produce a list, such as `CPULIST="0,1,2,3"`.

**Note:** we certainly don't want to simulate more particles than what we can fit in the RAM. During the simulation, make sure to use `htop` to keep track of the memory consumption.

**Note:** for multi-socket machines we use only the memory dedicated to one socket, as the intention of the original code is to have separate MPI processes for each socket.


### Using lstopo to find the CPULIST to use

Execute the following command (in the Singularity shell, if you use it):

```
   lstopo -p
```

Figure \ref{numa} gives a sample output, produced on a laptop with 4 physical cores and 32GB of ram.
We are interested in picking the first processing units (PU) of each
physical core (the second one is associated with hyperthreading),
and consider only the cores associated with the first socket
(NUMA machines have more than one socket).
On the figure, we can read the ids of the PU
that we would use on our laptop: `CPULIST="0,1,2,3"`.

![Sample output of lstopo for a 4-core laptop; for this hardware, we use `CPULIST="0,1,2,3"`. \label{numa}](sc_artifact_lstopo.png){width=60% height=50%}


## Selection of the grid size and number of particles

The size of the simulation grid is controlled by the parameter `GRID`,
which denotes the size of each of the sides of the grid, e.g. `GRID=32` or `GRID=64`.

The number of particles simulated is controlled by the parameter `NB`,
which denotes the number of million particles, e.g. `NB=200` or `NB=2000`.


The simulation parameters may be bounded by the capabilities of your hardware.

- If you have < 32GB of RAM, you can use only GRID=32.
- If you have >= 32GB of RAM, you should use GRID=64.

- If you have 16GB or 32GB of RAM, you should use NB=200.
- If you have 96GB of RAM or more, you should be able to use NB=2000,
assuming that your current memory load is no more than a couple GB.

The combinations of paramters used in the paper are:

- for machine #1: GRID=64 NB=2000
- for machine #2: GRID=32 NB=200
- for machine #3: GRID=64 NB=200

At least one of these combination of parameters should suit your machine.
If you have an intermediate machine, we would recommand trying first
GRID=64 NB=200, then GRID=64 NB=500.


## Execution of the benchmark script

**Note:** if you are using the Singularity container, make sure to
use the Singularity shell set up previously.

First, reach the results folder, which contains our benchmarking script.

```
   cd ${OPTITRUST}/case_studies/pic/results
```

As example command line, just to make sure that everything runs fine, try:

```
   CORES=4 CPULIST="0,1,2,3" COMP=icc GRID=8 NB=2 STEPS=10 RUNS=2 \
    PROG="pic_barsamian_single.c pic_optimized_single.c" ./bench.sh
```

where:

- `CPULIST` is the list determined above
- `CORES` is the number of the physical cores, it should match the length of `CPULIST`.
- `COMP` is the compiler argument, use `icc` (or `gcc` if you don't have `icc`).
- `STEPS` is the number of steps performed in the simulation, use `100`.
- `RUNS` is the number of runs to perform, use `10` (or fewer if time is limited).
- `SEED` if you want to use the same random seed for every run, as opposed to using a different one for each run (e.g. `SEED=0`).


Remark: our code, like Barsamian's code, initializes particles using a single core.
Overall, the initialization process generally takes the same order of magnitude of
time as the parallel processing of the particles.


## Reproducing performance results from the paper (Figure 9)

We provide the three command lines which we used, for each of our 3 machines.
Make sure to adjust the CPULIST as explained previously.
You can adjust the number of runs.
You can also reduce the number of particles, if you RAM is limited.

Throughput results can vary very significantly from a machine to the next.
Larger number of particles lead to better throughput, because the time
needed to process the grid at each time step becomes relatively smaller.


Machine #3: Example command line for a 4-core laptop with 32GB RAM or more.
You can reduce to RUNS=3. Possibly set an arbitrary seed, e.g. SEED=42, if
you want to fix the initial distribution of particles, and measure the
variance of the execution time for a same simulation.
A run typically takes less than 10 minutes.

```
   CORES=4 CPULIST="0,1,2,3" COMP=icc GRID=64 NB=200 STEPS=100 RUNS=10 \
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
You can reduce to RUNS=3 or even RUNS=1.
Possibly set an arbitrary seed, e.g. SEED=42.

```
   CORES=18 CPULIST="1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35" \
    COMP=icc GRID=64 NB=2000 STEPS=100 RUNS=10 \
    PROG="pic_barsamian_single.c pic_optimized_single.c" ./bench.sh
```


## Sample output

During the runs, the programs output only the execution time and the raw throughput (million particles
per seconds), but not the throughput per core.

At the end, the bench script displays a summary of the results.
Each row contains four columns:

- the execution time (in seconds),
- the throughput (in million particles per second),
- the throughput per core (in million particles per second per core),
- the simulation arguments, and the program name: `pic_barsamian` is the baseline, `pic_optimized` is ours.


Here is an example summary:

```
====Summary : Exectime / Throughput / ThroughputPerCore / Run =====
189.376	105.6	26.4	beg/results_cores4_icc_grid64_nb200_steps100_seed0_run0_pic_barsamian_single.txt
170.756	117.1	29.2	beg/results_cores4_icc_grid64_nb200_steps100_seed0_run0_pic_optimized_single.txt
196.828	101.6	25.4	beg/results_cores4_icc_grid64_nb200_steps100_seed1_run1_pic_barsamian_single.txt
174.462	114.6	28.6	beg/results_cores4_icc_grid64_nb200_steps100_seed1_run1_pic_optimized_single.txt
205.451	97.3	24.3	beg/results_cores4_icc_grid64_nb200_steps100_seed2_run2_pic_barsamian_single.txt
177.209	112.9	28.2	beg/results_cores4_icc_grid64_nb200_steps100_seed2_run2_pic_optimized_single.txt

```

On the sample results above, between the first two runs, the throughput increases from
105.6 to 117.1, a 16% improvement.

The execution time values are highly dependent on the hardware used. Likewise for the throughput values.
The relative difference between the two code might also vary, depending on the relative
performance of computation power vs memory bandwidth of the hardware.

For throughput per core, values in the range 10 to 30 million particles per second per core can be expected.
For the ratio Ours/Orig, that is, `pic_optimized/pic_barsamian`, values in the range 0% to 20% can be expected.

The claim of the paper is the ability to produce code that is equivalent or better
to the manually optimized code. Technically, even small negative values, e.g. -3%,
would not compromise the conclusions of the paper.


# Part 2: Execution of the transformation scripts

## Introduction

In this part we explain how to execute our transformation script, to transform the
unoptimized C code into the optimized C code.

This part can be achieved either using the singularity package, or using the
VirtualBox VM which we have prepared for interactive usage with VScode, or using
system packages.


## Installation

### Option 1: installation using Singularity

Use the same image as in the previous section.

### Option 2: installation using the VM

Install VirtualBox:

```
  sudo apt-get install virtualbox
```

Download our image:

```
  wget http://www.chargueraud.org/research/2022/sc_optitrust/OptiTrust.ova
```

Then, open VirtualBox, navigate to file, using `Import Appliance ..`,
load the file `OptiTrust.ova`.
Specify the CPU-s and RAM that you want to be allocated for the VM,
and click the Import button. Finally, click the Start button.

Inside the VM, execute the script `install.sh` to download and install OptiTrust.

Optional: execute the script `watch.sh` and leave it open in a separate terminal.
The "watch" script is used to support keybindings in VScode.


### Option 3: installation using system packages

It takes about 30 minutes to install the required OCaml software.


Installation of system packages:

```
   sudo apt-get install clang-format meld libclang-dev llvm-dev libomp-dev pkg-config zlib1g-dev
```

Unfortunately, this takes a little while (typically 15-20 minutes), because the opam
package manager compiles the OCaml compiler and all its packages from sources.

```
   sudo apt-get install opam
   opam init
   opam switch create 4.12.0
   opam pin add menhirLib 20210419
   opam pin add pprint 20220103
   opam install dune clangml pprint menhir menhirLib base64 ocamlbuild
   eval $(opam env)
```

In case of trouble, the installation of opam is explained on this page:
https://opam.ocaml.org/doc/Install.html

Remark about package versions:

- OCaml compiler (tested with version 4.12.0)
- dune (tested with version 2.9.1)
- clangml (https://gitlab.inria.fr/tmartine/clangml) (tested with version
  4.5.0)
- [pprint](https://github.com/fpottier/pprint) (tested with version 20220103)
- menhir and menhirLib (must be version 20210419, otherwise it won't work)


## Compilation and installation of OptiTrust, and copy of the pic-demo source files

If you have not done it before, or if you use another shell,
you need to export the path to the OptiTrust folder.

```
   # From the root folder of OptiTrust
   export OPTITRUST=`pwd`
   eval $(opam env)
```

All the commands in the following steps are to be executed in the `/demo` folder.

```
   cd demo
```

In this folder, you can compile and install the OptiTrust library,
unless the container already comes with OptiTrust installed.

```
   # skip this step if you are using the singularity container
   make optitrust
```

Then copy the input source files over from the `../case_studies/pic` folder using the
command:

```
   make import 
```

## Execution of our transformation script

The command below generates `pic_demo_single_out.cpp`, which should be the same
as `pic_demo_single_exp.cpp`, which we used for benchmarking under the name
`pic_optimized_single.c` in the `pic/simulation/` folder.

```
   make optim_single
```

On a recent laptop, the command takes about 10 seconds to execute.

To check that the file produced matches the one used, you can use the command:

```
   diff --ignore-blank-lines --ignore-all-space pic_demo_single_out.cpp pic_demo_single_exp.cpp
```

## Generation of the HTML page to browse through the steps

To build the interactive trace, execute the command:

```
   make trace
```

Then open the output file:

```
   chromium-browser pic_demo_trace.html &
```

You can use the buttons to navigate in the big steps, on in the small steps that
constitute the big steps.


# Part 3: Interactive usage of OptiTrust

Feel free to skip this part, which is mostly about setting keybindings,
and has very little scientific interest.

It will probably take about 20 minutes to install and configure VScode.
The only point is to check that our keybindings allow for interactive
development of the tranformation script: F6 shows the diff for a step,
ALT+F6 shows the diff for a group of steps (a "big step"). The key bindings
F7 and ALT+F7 are similar but work with respect to a checkpoint previously
saved using CTRL+F7.

You can use either the VirtualBox VM described in the previous section,
or use system packages.

## Option 1: installation using the VM

See previous section.

## Option 2: installation using system packages

### Installation of chromium-browser

Our scripts assume the Chromium browser, which opens up pages very fast.

```
   sudo apt-get install chromium-browser
```


### Installation of VSCode

Download VScode from https://go.microsoft.com/fwlink/?LinkID=760868

Install the .deb package using a commande of the form:

```
   sudo dpkg -i code_xxx.deb
```

We used Visual Studio Code version 1.63.2, but any recent version should work.


### Configuration of VScode

We provide key bindings for Visual Studio Code (VSCode).

To execute a VSCode task, one may use a keyboard shortcut. This shortcut triggers a
task defined in `.vscode/tasks.json`, which is part of the repository, unlike
`keybindings.json`, where bindings are defined, and which is a user-specific
configuration file.

In the root folder of OptiTrust, open VSCode with:

```
   cd ${OPTITRUST}
   code . &
```

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
   # and let the process run in the terminal
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

To execute a given step, place the cursor on a line starting with '!!' among
the first few ones, type `F6` on this should open a browser with the diff.
Type `ALT+F6` to see the diff for the big step containing the cursor.

To work on a selected big-step from the script, it is useful to
save a local checkpoint. To that end, select a line starting with
"bigstep", type `CTRL+F7`, this saves a checkpoint for that line.
Then, select a line starting with '!!' among the lines that follow
the selected big-step, and type `F7`, this should show the diff,
faster than using F6 which executes everything from the start.


# Part 4: Correctness checker (bonus)

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

Simulation parameters are controlled by the file `template_parameters_3d_checker.txt`,
where you can increase the grid size, the number of particles, or
the number iterations as desired.

Typical values for deviations in large simulations are in the order of `10^(-13)`.

The source code for the checker may be found in the file:
```
   ${OPTITRUST}/case_studies/pic/scripts/checker.c
```


# Appendix: Reference of the machine used for the benchmark


## Compiler options

The compilation command is generated by `${OPTITRUST}/case_studies/pic/scripts/compile.sh`.

It includes, for programs compiled using ICC:

```
    source /opt/intel/oneapi/setvars.sh > /dev/null
    OPTIONS="-lfftw3 -lm -O3 -march=native -std=gnu11"
    mpiicc ${OPTIONS} ${FILES} -qopenmp -qopt-report-phase=vec -qopt-report=1 -o theprogram.out
```

## Execution options

The execution command is generated by `${OPTITRUST}/case_studies/pic/scripts/run.sh`.

It includes, for programs compiled using ICC:

```
   export LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libjemalloc.so
   source /opt/intel/oneapi/setvars.sh > /dev/null
   export LD_LIBRARY_PATH=$INTEL_OPENMP_DYNAMIC_LIBRARY_PATH:$LD_LIBRARY_PATH
   export KMP_AFFINITY=granularity=fine,compact,1,0,verbose
   taskset --cpu-list ${CPULIST} ./theprogram.out
```


## Machine #1, with a 18-core socket.

Machine #1 is a 36-core machine, with two sockets.
Each socket hosts a 18-core Intel Xeon Gold 6240 chip, running at 2.60GHz, with 96GB of RAM.

The system is CentOS Linux release 7.6.1810 (Core), 3.10.0-957.el7.x86_64.
The version of ICC is 19.0.4.243.


## Machine #2, with a 10-core socket.

Machine #2 is a 20-core machines, with two sockets.
Each sockets hosts a 10-core Intel(R) Xeon(R) CPU E5-2650 v3 chip, running at 2.30GHz, with 16GB of RAM per socket.

The system is Ubuntu 18.04.6 LTS (binonic), 4.15.0-143-generic.
The version of ICC is 2021.5.0.

## Machine #3, with a 4-core socket.

Machine #3 is a 4-core laptop, with a single socket.
The socket hosts a Intel(R) Core(TM) i7-8665U CPU, running at 1.90GHz, with 32GB of RAM.

The system is Ubuntu 20.04.4 LTS(focal), 5.4.0-80-generic.
The version of ICC is 2021.5.0.






