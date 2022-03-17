
# Set up

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


# Compilation of the demo

```

   # execute the transformation script
   make pic_demo.out

   # copy output file to ../case_studies/pic/simulations/pic_optimized.c
   make export

   # compile the output file
   cd ../case_studies/pic/scripts
   ./compile.sh pic_optimized.c

   # check correctness against non-optimized code
   ./check.sh pic_demo.c pic_optimized.c
```

# To run the checker on all steps

```
   # in pic_demo.ml, make sure to set:
   let use_checker = true

   # build one cpp file per bigstep
   make pic_demo.bigsteps

   # export those files as C files into  ../case_studies/pic/simulations/pic_demo_$i_out.c
   make export_bigsteps

   #
   cd ../case_studies/pic/scripts
   ./check_steps.sh pic_demo
```
