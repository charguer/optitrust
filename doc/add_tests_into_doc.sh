#!/bin/sh

OPTITRUST_PATH=$(dirname $0)/..
cd ${OPTITRUST_PATH}

tryopen() {
  if [ "${USER}" = "charguer" ]; then
    ~/conf/vscode/show_window.sh || echo "tryopen failed"
  fi
}

dune exec doc/add_tests_into_doc/add_tests_into_doc.exe && tryopen


