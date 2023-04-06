#!/bin/bash


# TODO FOR ARTHUR: take as argument FILENAME, then deduce FILEBASE AND FILEEXT
# AND if we're calling a cpp file, then don't add_exit

DIRNAME=$1
FILEBASE=$2
LINE=$3
VIEW=$4 # should be view_diff or view_result or view_trace
RECOMPILE_OPTITRUST=$5 # should be recompile_optitrust_yes or recompile_optitrust_no
OPTIONS=$6
OPTIONS2=$7

# LATER: if a ${FILEBASE}_exp.cpp file is present, export it into the JS file,
# so that the browser can report on the differences between _out.cpp and _exp.cpp.

#UPDATE=noupdate

# Path to .vscode folder and src folder and src/src folder
VSCODE=`pwd`
SRCFOLDER=`cd .. && pwd`
SRCSRCFOLDER=`cd ../src && pwd`

# This can help with opam switches
eval $(opam env)

# Make sure we work in the directory that contains the file
cd ${DIRNAME}


# Read options from optitrust_flags.sh
# options: e.g., FLAGS="-dump-ast-details -analyse-stats-details -debug-reparse"
if [ -f "optitrust_flags.sh" ]; then
  source optitrust_flags.sh
fi


# NOTE: this could be removed if we use the VScode command to run "make optitrust" first.
# Run make update in working folder if requested
if [ "${RECOMPILE_OPTITRUST}" = "recompile_optitrust_yes" ]; then
  echo "recompile lib"
  # FIXME: This is very fragile since it requires that the Makefile in
  # the transformation script directory have an optitrust target properly configured
  make optitrust
  OUT=$?
  if [ ${OUT} -ne 0 ]; then
    echo "Could not compile lib"  >> /dev/stderr
    exit 1
  fi
fi


PROG="${FILEBASE}_with_lines.cmxs"
# TODO: Install optitrust_runner and use the installed version
RUNNER="${SRCFOLDER}/runner/optitrust_runner"

# First we create the source code for the transformation program
# ---DEPRECATED:
# ocaml ${VSCODE}/add_exit.ml -file "${FILEBASE}.ml" -line ${LINE}
#sed 's/show/myshow/' "${FILEBASE}.ml" > "${FILEBASE}_with_exit.byte"

# From "${FILEBASE}.ml", create ""{FILEBASE}_with_lines.ml" by inserting
# [~lines:__LINE__]   in the relevant places, and interpreting '!!' and '!!!'

${VSCODE}/add_lines.sh ${FILEBASE}.ml ${FILEBASE}_with_lines.ml
# DEBUG: cat "${FILEBASE}_with_lines.ml"; exit 0

# LATER: add_exit should also introduce special commands for figuring out the line of the command that executes

# Second, we compile that transformation program
# DEPRECATED
# ocamlbuild -quiet -r -pkgs clangml,refl,pprint,str,optitrust "${FILEBASE}_with_exit.byte"
# TODO(Anton): replace this line with a dune command that uses directly /src/src files instead
# of the installed package; only consider ${FILEBASE}.ml from local folder


# TODO: check that PROG is also more recent than the optitrust library before reactivating
#if [[ "${FILEBASE}.ml" -nt "${PROG}" ]] || [[ "${FILEBASE}.cpp" -nt "${PROG}" ]]; then
  # echo FILE1 is newer than FILE2
  PROGNEEDSREBUILD="needsrebuild"
#fi

if [ "${RECOMPILE_OPTITRUST}" = "recompile_optitrust_yes" ] || [ "${PROGNEEDSREBUILD}" = "needsrebuild" ]; then
  rm -Rf _build/${PROG} ${PROG}
  ocamlbuild -use-ocamlfind -r -tags "debug,package(clangml),package(refl),package(pprint),package(str),package(optitrust)" ${PROG}
  ln -sf _build/${PROG} ${PROG}
fi

# LATER: capture the output error message
# so we can do the postprocessing on it


OUT=$?
if [ ${OUT} -ne 0 ];then
  echo "Error: could not compile the program ${PROG}"  >> /dev/stderr
  exit 1
fi

if [ "${VIEW}" = "view_trace" ]; then

  # To build the trace, we invoke the appropriate makefile command
  TARGET="${FILEBASE}_trace.html"
  make OPTITRUST="${SRCFOLDER}" ${TARGET}
  OUT=$?
  if [ ${OUT} -ne 0 ];then
    exit 1
  fi

else



  if [ "${VIEW}" = "view_diff_enc" ]; then
    OPTIONS="${OPTIONS} -dump-ast-details"
  fi

  # Third, we execute the transformation program, obtain "${FILEBASE}_before.cpp" and "${FILEBASE}_after.cpp, unless mode is view_trace
  # For that run, we activate the backtrace
  echo "OCAMLRUNPARAM=b ${RUNNER} ${PROG} -exit-line ${LINE} ${OPTIONS} ${OPTIONS2} ${FLAGS}"
  OCAMLRUNPARAM=b ${RUNNER} ${PROG} -exit-line ${LINE} ${OPTIONS} ${OPTIONS2} ${FLAGS}

  # DEBUG: echo "cd ${DIRNAME}; ./${PROG} -exit-line ${LINE} ${OPTIONS}"
  # DEPREACTED | tee stdoutput.txt

  OUT=$?
  if [ ${OUT} -ne 0 ];then
    #echo "Error executing the script:"
    #echo "  cd ${DIRNAME}; ./${PROG} -exit-line ${LINE} ${OPTIONS} ${OPTIONS2}"
    exit 1
  fi

fi

# Fourth, we vizualize a result or a diff or a trace
# We need to cd to ${VSCODE} folder because that's how the scripts know the path to .vscode

cd ${VSCODE}


if [ "${VIEW}" = "view_diff" ] || [ "${VIEW}" = "view_diff_enc" ]; then

  if [ "${VIEW}" = "view_diff_enc" ]; then
    DIFFFOR="enc"
  fi

  ./open_diff.sh ${DIRNAME} ${FILEBASE} ${DIFFFOR} &

elif [ "${VIEW}" = "view_result" ]; then

  ./open_result.sh ${DIRNAME} ${FILEBASE} &

elif [ "${VIEW}" = "view_trace" ]; then

  ./open_in_browser.sh ${DIRNAME}/${TARGET} "Trace for ${DIRNAME}/${FILEBASE}"

else

  echo "invalid VIEW argument"  >> /dev/stderr
  exit 1

fi

exit
