#!/bin/bash

# Open in vscode the tests files associated with a transformation

# Usage:
#    open_test.sh mytest.ml withdoc
#    open_test.sh mytest.ml
#    open_test.sh mytest.cpp
#    open_test.sh mytest_doc.ml
#    open_test.sh mytest

FILE=$1
WITHDOC=$2

# remove extension and _doc suffix if any
BASE=${FILE%.*}
BASE=${BASE%_doc}
BASE=${BASE%_exp}

ARGS="${BASE}.ml ${BASE}.cpp"

if [ ! -z "${WITHDOC}" ]; then
  ARGS+=" ${BASE}_doc.ml ${BASE}_doc.cpp"
fi

if [ -f "${BASE}.ml" ]
then
  code ${ARGS}
else
  echo "open_test.sh: ${BASE}.ml is not a cpp or a ml file"
  exit 1
fi

