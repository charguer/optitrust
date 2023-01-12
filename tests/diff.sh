#!/bin/bash

# Compare two source files by ignoring all space and line return differences
#
# Usage: diff.sh file1 file2

FILE1=$1
FILE2=$2
diff --ignore-all-space <(cat ${FILE1} | sed 's#//.*##g' | tr '\n' ' ') <(cat ${FILE2} | sed 's#//.*##g' | tr '\n' ' ')


