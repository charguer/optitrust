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







Now we are ready to open vscode and open pic_demo.ml file to generate the diffs similar to the ones on our paper.

<!--
UNFORTUNATELY: IT'S NOT CONSISTENT ENOUGH
Before you move on we would suggest to try running OptiTrust interactively from the container.
To do that you could skip the instructions for installing OptiTrust on your machine and just run the following inside the container shell:

```
  eval $(opam env)
  cd $OPTITRUST
  make install
```

this command will load the `opam` environment that contains all the libraries needed to run `OptiTrust`.
Navigate to the optitrust directory and install it inside the container. Finally VSCode should be installed and configured as documented on the next step. If that doesn't work then you will have to install OptiTrust with all its dependencies in your operating system(Ubuntu 18.04 and later). -->

**Note:** This part can be skipped if you used the VM to run and test OptiTrust.

In this second part, we explain how to install the tooling for generating,
using OptiTrust, our `pic_optimized` program, starting from the totally
unoptimized code, which corresponds to the file
`${OPTITRUST}/case_studies/pic/simulations/pic_demo.c`.

It takes about 30 minutes to install the required OCaml software.
Then, running the transformation script and checking its output should
take no more than 2 minutes.

**Note:** we pushed really hard to provide a container with OptiTrust and its dependencies already installed but things didn't work out well. Because containers are not designed to work GUI apps we experienced some un-consistent behaviour of the program.
Hence, to reproduce the same diffs as the ones in the paper installing OptiTrust is mandatory.

We first list the OCaml packages required, and then detail further on the instructions
for installing those packages using opam, the package manager for OCaml.


Besides, we also require:

- several system packages that are needed by the OCaml package (they are listed further on)
- gcc (tested with version 9.4.0, but any version should work)
- clang-format (tested with version 10.0.0, but any version should work)
