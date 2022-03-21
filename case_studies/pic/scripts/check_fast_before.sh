#!/bin/bash

make -C ../../../demo export_fast_before
./check.sh pic_demo.c pic_optimized.c
