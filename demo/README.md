
# Set up


```
   # set up configuration.sh and parameters_3d.txt,
   # as described in ../case_studies/pic/README.md

   # then run to compile optitrust and import C files from ../case_studies/pic/*/*.{h,c}
   make init
```


Details of what this performs:
```
   # compile the optitrust library
   make optitrust

   # copy relevant files from ../case_studies/pic/*
   make import
```


# To build the trace

```
   make pic_demo_trace.html

   chromium-browser pic_demo_trace.html &
```

# Execution of the transformation script

```
   make pic_demo.out
```


# Compilation of the output of the transformation script

```
   # execute the transformation script and
   # copy output file to ../case_studies/pic/simulations/pic_optimized.c
   make optim

   # compile the output file
   cd ../case_studies/pic/scripts
   ./compile.sh pic_optimized.c

   # get information about vectorization (uses meld)
   ./vectinfo.sh pic_optimized.c
```

# To evaluate performance

```
   make perf

   # equivalent to
   make optim
   cd ../case_studies/pic/scripts
   ./perf.sh
```


# To run the checker

```
   make chk
```


./vectinfo.sh pic_barsamian.c


# To run the checker at a given point obtained by the F7 shortcut

```
# replace in pic_demo.ml
#   let usechecker = !usechecker
# with
#   let usechecker = !usechecker || true
   make export_fast
   cd ../case_studies/pic/scripts
   ./check.sh pic_barsamian.c pic_demo_checker.c
```

# To run the checker on all steps

# TODO: fix this...the checker crashes for some reason

```
   # set parameters_3d.txt to a small grid and nbparticles, else it takes a long time

   # build one cpp file per bigstep
   make chksteps

   # this exports the output C files into:
   # ../case_studies/pic/simulations/pic_demo_checker_$i_out.c
   # and executes:
   cd ../case_studies/pic/scripts
   ./check_steps.sh pic_demo_checker
```
