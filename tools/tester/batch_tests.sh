#!/bin/bash

# Take as argument a list of .ml files containing tests.
# Concatenate these tests, with the following processing.
# For a file 'foo.ml', replace 'script_cpp' with
# 'script_cpp ~batching:"foo.ml"'

# Can use env variable DISABLE_LINESHIFT=1 to disable shifting of locations

for file in "$@"
do
    echo
    # Add a marker to show errors on their original file location
    echo "let _ = Batching.run_test ~script_name:\"${file}\" (fun () -> (module struct"
    if [ -z ${DISABLE_LINESHIFT} ]; then
      echo \# 1 \"${file}\"
    fi
    #if [ ! -f ${file} ]; then
    #  echo "Missing file ${file}"
    #  exit 1;
    #fi;
    cat ${file}
    echo "end))"
done

