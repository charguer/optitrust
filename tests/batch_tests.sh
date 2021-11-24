#!/bin/bash

# Take as argument a list of .ml files containing tests.
# Concatenate these tests, with the following processing.
# For a file 'foo.ml', replace 'Run.script_cpp' with
# 'Run.script_cpp ~filename:"foo.ml"'

# To debug this script, run:
#   cd basic
#   make BATCH=1 TESTS="loop_fission.ml loop_color.ml" batch.ml && cat batch.ml

for file in "$@"
do
    sed "s/Run.script_cpp/Run.script_cpp ~filename:\"${file}\"/" < ${file}
done

