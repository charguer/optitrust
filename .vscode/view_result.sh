#!/bin/bash
# Arguments:
#   0. file dirname
#   1. filename of the transformation script (without extension)
#   2. active line number (to add exit_script instruction)
#   3. option(s) for execution
#      currently: only -dump-trace

DIRNAME=$1
FILEBASE=$2
LINE=$3
UPDATE=$4 # should be update or noupdated
OPTIONS=$5

#UPDATE=noupdate

# Path to .vscode folder and src folder and src/src folder
VSCODE=`pwd`
SRCFOLDER=`cd .. && pwd`
SRCSRCFOLDER=`cd ../src && pwd`

# Special treatment of src folder!
if [ "${DIRNAME}" = "${SRCSRCFOLDER}" ]; then
  make -C ${SRCFOLDER}
  echo "Recompiled the lib, done."
  exit 0
fi

# Run make update in working folder if requested
if [ "${UPDATE}" = "update" ]; then
  echo "recompile lib"
  make -C ${DIRNAME} update
  OUT=$?
  if [ ${OUT} -ne 0 ];then
    echo "Could not compile lib"
    exit 1
  fi
fi

# Make sure we work in the directory that contains the file
cd ${DIRNAME}

# First we create the source code for the transformation program
ocaml ${VSCODE}/add_exit.ml -file "${FILEBASE}.ml" -line ${LINE}

# Second, we compile that transformation program
ocamlbuild -quiet -r -pkgs clangml,refl,pprint,str,optiTrust.optitrust "${FILEBASE}_with_exit.byte"

OUT=$?
if [ ${OUT} -ne 0 ];then
  echo "Could not compile file"
  exit 1
fi

# Third, we execute the transformation program, obtain "${FILEBASE}_before.cpp" and "${FILEBASE}_after.cpp
./${FILEBASE}_with_exit.byte ${OPTIONS}
# DEPREACTED | tee stdoutput.txt

OUT=$?
if [ ${OUT} -ne 0 ];then
  echo "Could not execute script"
  exit 1
fi

# Fourth, we vizualize the diff between these two files

cd ${VSCODE}
${VSCODE}/view_diff.sh ${DIRNAME} ${FILEBASE} &


exit
