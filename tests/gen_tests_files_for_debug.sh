#!/bin/bash

# Usage: from the root of the OptiTrust repon, call:
#   tests/gen_tests_files_for_debug.sh
#
# The execution of this script creates dummy *.tests files,
# which may then be used for debugging `tester.ml`.

echo "
tests/basic/variable_rename.ml
tests/basic/variable_rename_doc.ml
tests/basic/variable_to_const.ml
" > ignored.tests

echo "
tests/basic/typedef_unfold.ml
tests/basic/variable_bind.ml
" > missing_exp.tests

echo "
tests/basic/variable_rename.ml
tests/basic/variable_rename_doc.ml
tests/basic/variable_subst.ml
" > failed.tests

echo "
tests/basic/variable_fold.ml
" > wrong.tests

cat failed.tests > errors.tests
cat wrong.tests >> errors.tests

