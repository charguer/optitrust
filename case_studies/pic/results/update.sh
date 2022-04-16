#!/bin/bash

# ./update.sh
#   recompiles the pic_optimized.c file

cd ../../../demo
make import

make optim
make optim_single

