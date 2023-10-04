
# Installation of additional useful tools for program optimization (OPTIONAL)

### Installation of hwloc

The package `hwloc` provides the command `lstopo` used for viewing your
machine topology, and gather the identifiers of the cores to use for
multicore executions.

```sh
   sudo apt install -y hwloc
```

### Installation of ICC via Intel oneAPI HPC

# Download the Intel key, add it to the system keyring, then install the relevant package

```sh
   wget -O- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB \
   | gpg --dearmor | sudo tee /usr/share/keyrings/oneapi-archive-keyring.gpg > /dev/null

   echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] \
     https://apt.repos.intel.com/oneapi all main" \
     | sudo tee /etc/apt/sources.list.d/oneAPI.list

   sudo apt update && sudo apt install -y intel-hpckit
```

### Installation of Valgrind

To install the Valgrind tool for detecting memory errors.

```sh
   sudo apt-get valgrind
```

### Installation of OpenMPI

To use OpenMPI with the GCC toolchain for distributed computations:

```sh
   sudo apt-get install libopenmpi-dev openmpi-bin
```


### Installation of jemalloc

Jemalloc is an alternative to standard malloc, for fast multi-thread allocation.
It may be activated in the PIC case study, in particular.

```sh
   sudo apt install -y libjemalloc-dev
```

To use it:

```sh
  export LD_PRELOAD=$LD_PRELOAD:/path/to/jemalloc-build/lib/jemalloc.so.1
  cc myprog.c -o myprog -L`jemalloc-config --libdir` -Wl,-rpath,`jemalloc-config --libdir` -ljemalloc `jemalloc-config --libs`
```


### Installation of FFTW

The FFTW (fast fourier transform) library is used by the Poison solver in the PIC case study.

```sh
   sudo apt-get install -y libfftw3-dev
```
