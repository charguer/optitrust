#!/bin/bash

# Take as argument a list of .ml files containing tests.
# Concatenate these tests, with the following processing.
# For a file 'foo.ml', replace 'script_cpp' with
# 'script_cpp ~filename:"foo.ml"'

# Add as header "let _ = Optitrust.Flags.batch_tests := true"

# To debug this script, run:
#   cd basic
#   make BATCH=1 TESTS="loop_fission.ml loop_color.ml" batch.ml && cat batch.ml

echo "let _ = Optitrust.Flags.batch_tests := true"
for file in "$@"
do
    sed "s/script_cpp/script_cpp ~filename:\"${file}\"/;" < ${file}
done

