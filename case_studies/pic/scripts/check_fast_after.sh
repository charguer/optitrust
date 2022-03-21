#!/bin/bash

make -C ../../../demo export_fast
./check.sh pic_demo.c pic_optimized.c
