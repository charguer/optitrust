#!/usr/bin/env bash

# Open in VScode the tests files associated with a transformation.
# Using `exec_with_nohup_and_runaction.sh` to avoid VScode sandboxing issues.
# Needs the 'watch.sh' script to run in the background.

# Usage:
#    open_test.sh mytest.ml withdoc
#    open_test.sh mytest.ml
#    open_test.sh mytest.cpp
#    open_test.sh mytest_doc.ml
#    open_test.sh mytest
#
# Action:
#    code mytest.cpp mytest.ml
#
# Action if 'withdoc' is provided as second argument
#    code mytest.cpp mytest.ml mytest_doc.ml mytest_doc.cpp


DOTVSCODEFOLDER=$(dirname -- "$( readlink -f -- "$0"; )")
FILE=$1
WITHDOC=$2

# Remove the extension, and the _doc and _exp suffixes if any
BASE=${FILE%.*}
BASE=${BASE%_doc}
BASE=${BASE%_exp}

# Prepare the list of files to open
ARGS="${BASE}.ml ${BASE}.cpp"
if [ ! -z "${WITHDOC}" ]; then
  ARGS+=" ${BASE}_doc.ml ${BASE}_doc.cpp"
fi

# Open the files using 'code', after checking that the '.ml' file exists
if [ -f "${BASE}.ml" ]
then
  ${DOTVSCODEFOLDER}/exec_with_nohup_and_runaction.sh code ${ARGS}
else
  echo "open_test.sh: ${BASE}.ml is not a cpp or a ml file"
  exit 1
fi

