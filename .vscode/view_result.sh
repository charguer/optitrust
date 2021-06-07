#!/bin/bash
# Arguments:
#   0. file dirname
#   1. filename of the transformation script (without extension)
#   2. active line number (to add exit_script instruction)
#   3. option(s) for execution
#      currently: only -dump-trace

# TODO FOR ARTHUR: take as argument FILENAME, then deduce FILEBASE AND FILEEXT
# AND if we're calling a cpp file, then don't add_exit

DIRNAME=$1
FILEBASE=$2
LINE=$3
VIEW=$4 # should be view_diff or view_result
RECOMPILE_OPTITRUST=$5 # should be recompile_optitrust_yes or recompile_optitrust_no
OPTIONS=$6

# LATER: if a ${FILEBASE}_exp.cpp file is present, export it into the JS file,
# so that the browser can report on the differences between _out.cpp and _exp.cpp.


#UPDATE=noupdate

# Path to .vscode folder and src folder and src/src folder
VSCODE=`pwd`
SRCFOLDER=`cd .. && pwd`
SRCSRCFOLDER=`cd ../src && pwd`


# Make sure we work in the directory that contains the file
cd ${DIRNAME}

# NOTE: this could be removed if we use the VScode command to run "make optitrust" first.
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
ocamlbuild -quiet -r -pkgs clangml,refl,pprint,str,optitrust "${FILEBASE}_with_exit.byte"
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
  echo "Error executing the script:"
  echo "  cd ${DIRNAME}; ./${FILEBASE}_with_exit.byte ${OPTIONS}"
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
