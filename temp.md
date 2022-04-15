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
