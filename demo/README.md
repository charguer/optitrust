

# Compilation of the demo

```
   # copy relevant files from ../case_studies/pic/*
   make import

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

# TODO: rename pic_demo.c to pic_naive.c
