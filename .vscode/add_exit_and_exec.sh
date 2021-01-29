#!/bin/bash
# Arguments:
#   1. filename of the transformation script (without extension)
#   2. active line number (to add exit_script instruction)
#   3. option(s) for execution
#      currently: only -dump-trace

# first step: add exit_script instruction at the end of the active line
ocaml .vscode/add_exit.ml -file "$1.ml" -line $2

# second step: build and execute the script
ocamlbuild -pkgs clangml,refl,pprint,str,optiTrust.scriptTools "$1_with_exit.byte" || (echo "Cannot compile $1_with_exit.ml"; exit 1)
./$1_with_exit.byte $3

# third step: clean up and show the diff of the two last states of the program
ocamlbuild -clean
rm "$1_with_exit.ml"
