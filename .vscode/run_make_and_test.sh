#!/bin/bash
#Run make install and clean after installing




make install #&& cd DIRNAME  && make ${UNIT_TEST}.out && meld ${UNIT_TEST}.cpp ${UNIT_TEST}.out


DIRNAME=$1
FILEBASE=$2
LINE=$3

cd ${DIRNAME}

ocaml ../.vscode/add_exit.ml -file "${FILEBASE}.ml" -line ${LINE}

# second step: build and execute the script
ocamlbuild -pkgs clangml,refl,pprint,str,optiTrust.scriptTools "${FILEBASE}_with_exit.byte" || (echo "Cannot compile $1_with_exit.ml"; exit 1)
./${FILEBASE}_with_exit.byte ${OPTIONS}

# third step: clean up and show the diff of the two last states of the program
ocamlbuild -clean
rm "${FILEBASE}_with_exit.ml"
