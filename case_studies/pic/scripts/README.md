
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


# Execution

```
cd src/case_studies/pic
./test.sh pic_barsamian.c
./test.sh pic_demo.c
```


# Additional notes
```
export GOMP_CPU_AFFINITY="0 2 4 6 8 10 12 14 16 18"
```



