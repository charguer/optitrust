#!/bin/sh

tryopen() {
  if [ "${USER}" = "charguer" ]; then
    ~/conf/vscode/show_window.sh
  fi
}

dune exec ./add_tests_into_doc.exe && tryopen
# && cat odoc_spec_after.html

