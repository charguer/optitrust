#!/bin/bash
#Run make install and clean after installing




make install || exit 1 #&& cd DIRNAME  && make ${UNIT_TEST}.out && meld ${UNIT_TEST}.cpp ${UNIT_TEST}.out


DIRNAME=$1
FILEBASE=$2
LINE=$3

cd ${DIRNAME}

VSCODE=../.vscode

# TODO: find a more robust way to point to the .vscode folder

# TODO: share the contents with the add_exit script

ocaml ${VSCODE}/add_exit.ml -file "${FILEBASE}.ml" -line ${LINE}

# second step: build and execute the script
ocamlbuild -pkgs clangml,refl,pprint,str,optiTrust.scriptTools "${FILEBASE}_with_exit.byte" || (echo "Cannot compile $1_with_exit.ml"; exit 1)
./${FILEBASE}_with_exit.byte ${OPTIONS}
${VSCODE}/view_diff.sh ${DIRNAME} ${FILEBASE}

# third step: clean up and show the diff of the two last states of the program
ocamlbuild -clean
rm "${FILEBASE}_with_exit.ml"
