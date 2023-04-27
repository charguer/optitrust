#!/bin/bash

# Take as argument a list of .ml files containing tests.
# Concatenate these tests, with the following processing.
# For a file 'foo.ml', replace 'script_cpp' with
# 'script_cpp ~batching:"foo.ml"'


# To debug this script, run:
#   cd basic
#   make BATCH=1 TESTS="loop_fission.ml loop_color.ml" batch.ml && cat batch.ml

for file in "$@"
do
    echo
    # Add a marker to show errors on their original file location
    echo "let _ = Batching.run_test ~script_name:\"${file}\" (fun () -> (module struct"
    echo \# 1 \"${file}\"
    cat ${file}
    echo "end))"
done

