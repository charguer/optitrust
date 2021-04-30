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
VIEW=$4 # should be view_diff or view_result
RECOMPILE_OPTITRUST=$5 # should be recompile_optitrust_yes or recompile_optitrust_no
OPTIONS=$6

#UPDATE=noupdate

# Path to .vscode folder and src folder and src/src folder
VSCODE=`pwd`
SRCFOLDER=`cd .. && pwd`
SRCSRCFOLDER=`cd ../src && pwd`

# Special treatment of src folder!
# DEPRECATED TODO: remove
if [ "${DIRNAME}" = "${SRCSRCFOLDER}" ]; then
  make -C ${SRCFOLDER}
  echo "Recompiled the lib, done."
  exit 0
fi

# Make sure we work in the directory that contains the file
cd ${DIRNAME}

# Run make update in working folder if requested
if [ "${RECOMPILE_OPTITRUST}" = "recompile_optitrust_yes" ]; then
  echo "recompile lib"
  make optitrust
  OUT=$?
  if [ ${OUT} -ne 0 ];then
    echo "Could not compile lib"
    exit 1
  fi
fi

# First we create the source code for the transformation program
ocaml ${VSCODE}/add_exit.ml -file "${FILEBASE}.ml" -line ${LINE}
# LATER: add_exit should also introduce special commands for figuring out the line of the command that executes

# Second, we compile that transformation program
ocamlbuild -quiet -r -pkgs clangml,refl,pprint,str,optiTrust.optitrust "${FILEBASE}_with_exit.byte"
# LATER: capture the output error message
# so we can do the postprocessing on it

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

# Fourth, we vizualize a result or a diff
# We need to cd to ${VSCODE} folder because that's how the scripts know the path to .vscode

cd ${VSCODE}

if [ "${VIEW}" = "view_diff" ]; then

  ./open_diff.sh ${DIRNAME} ${FILEBASE} &

elif [ "${VIEW}" = "view_result" ]; then

  ./open_result.sh ${DIRNAME} ${FILEBASE} &

else

  echo "invalid VIEW argument"
  exit 1

fi

exit
