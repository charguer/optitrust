
# Checklist for testing performance on a given machine

# Obtain the files

Only once:
```
git clone git@gitlab.inria.fr:charguer/verified_transfo.git
git checkout -b encodings
```

Only once per git update:

```
git pull
```

# Stream benchmark

Only once:

```
cd src/case_studies/pic/Stream-test
./a_compile_Stream.sh
./b_run_Stream.sh
```

Read the maximal value of the first column.


# Achitecture information

Only once:

```
htop
cat /proc/cpuinfo
lscpu
hwloc -ls
```

# Configuration of parameters

Only once per git update:

```
cd src/case_studies/pic
cp configuration_`hostname`.sh your_configuration.sh
cd scripts
cp parameters_3d_`hostname`.txt parameters_3d.txt
```

# Checking machine availability

```
who
htop
```

# Checking flags


Check: `compile.sh`
```
# should have empty VALGRIND
VALGRIND=

# should have defined jemalloc (LATER: we might need to adapt to the hostname)
JEMALLOC="export LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libjemalloc.so.1"
```

Check: `run.sh`
```
# should have empty DEBUGFLAGS
# DEBUGFLAGS="-g"

# should have PRINTPERF, and optionally DPRINTSTEPS to see the progress
PERFFLAGS=" -DPRINTPERF -DPRINTSTEPS"
```
The value of these parameters are printed by `run.sh` and `compile.sh`.


# Compilation

```
cd src/case_studies/pic
./compile.sh pic_optimized.c
# to compile with the checker activated
./compile.sh pic_optimized.c 1
```

# Execution

```
cd src/case_studies/pic
./run.sh pic_optimized.c
# to overwrite the number of threads from your_configuration.sh
P=2 ./run.sh pic_optimized.c
```


# Combined compilation and execution

```
cd src/case_studies/pic
./test.sh pic_barsamian.c
P=2 ./test.sh pic_demo.c
```

# Vectorization information

```
./vectinfo.sh pic_barsamian.c
./vectinfo.sh pic_optimized.c

# for more detailed information, in a GUI view
./vectinfo.sh pic_optimized.c view
# for more detailed information, as an output file
./vectinfo.sh pic_optimized.c full
```

# Correctness checking

```
./check.sh pic_barsamian.c pic_demo.c
./check.sh pic_demo.c pic_optimized.c
```

# Performance comparison

```
# compare with one thread
P=1 ./perf.sh pic_demo.c pic_optimized.c

# compare with 2 threads
P=2 ./perf.sh pic_barsamian.c pic_demo.c

# compare with the number of threads specified in your_configuration.sh, or e.g. `export P=4`
./perf.sh pic_barsamian.c pic_demo.c

# Compare against the version of pic_barsamian without the custom memory allocator for chunks
./perf.sh pic_barsamian_malloc.c pic_demo.c

```


# Additional notes
```
export GOMP_CPU_AFFINITY="0 2 4 6 8 10 12 14 16 18"
```



