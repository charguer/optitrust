#!/bin/bash

make -C ../../../demo export_fast
./test.sh pic_optimized.c
