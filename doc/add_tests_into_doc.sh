#!/bin/sh
#
# This script compiles and executes the program 'add_tests_into_doc.exe''
# which post-processes the documentation built by dune for the OptiTrust sources,
# so as to integrate example scripts---the "*_doc.ml" files---into the documentation.
#
# Usage: (from OptiTrust root folder)
#   ./doc/add_tests_into_doc.sh

OPTITRUST_PATH=$(dirname $0)/..
cd ${OPTITRUST_PATH}

dune exec doc/add_tests_into_doc/add_tests_into_doc.exe


