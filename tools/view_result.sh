#!/bin/bash


# TODO FOR ARTHUR: take as argument FILENAME, then deduce FILEBASE AND FILEEXT
# AND if we're calling a cpp file, then don't add_exit

DIRNAME=$1
FILEBASE=$2
LINE=$3
VIEW=$4 # should be view_diff or view_result or view_trace
RECOMPILE_OPTITRUST=$5 # should be recompile_optitrust_yes or recompile_optitrust_no
OPTIONS="${@:6}"

# LATER: if a ${FILEBASE}_exp.cpp file is present, export it into the JS file,
# so that the browser can report on the differences between _out.cpp and _exp.cpp.

#UPDATE=noupdate

# Path to tools folder and src folder
TOOLS_FOLDER=$(dirname -- "$(readlink -f -- "$0";)")
SRCFOLDER=${TOOLS_FOLDER}/..

# This can help with opam switches
eval $(opam env)

# Make sure we work in the directory that contains the file
cd ${DIRNAME}


# Read options from optitrust_flags.sh
# options: e.g., FLAGS="-dump-ast-details -analyse-stats-details -debug-reparse"
if [ -f "optitrust_flags.sh" ]; then
  source optitrust_flags.sh
fi


# Recompile optitrust if requested
if [ "${RECOMPILE_OPTITRUST}" = "recompile_optitrust_yes" ]; then
  echo "recompile lib"
  pushd ${SRCFOLDER}
  make install
  OUT=$?
  if [ ${OUT} -ne 0 ]; then
    echo "Could not compile lib"  >> /dev/stderr
    exit 1
  fi
  popd
fi


PROG="${FILEBASE}_with_lines.cmxs"
RUNNER="optitrust_runner"

# From "${FILEBASE}.ml", create ""{FILEBASE}_with_lines.ml" by inserting
# [~lines:__LINE__]   in the relevant places, and interpreting '!!' and '!!!'
${TOOLS_FOLDER}/add_lines.sh ${FILEBASE}.ml ${FILEBASE}_with_lines.ml
# DEBUG: cat "${FILEBASE}_with_lines.ml"; exit 0

# LATER: add_exit should also introduce special commands for figuring out the line of the command that executes

# Second, we compile that transformation program

# TODO: For scripts in local optitrust folder (and only them!), consider using
# a dune build command instead of the installed package. This would remove the
# need for the recompile_optitrust option since users of the installed version
# don't care about library recompilation.


# TODO: check that PROG is also more recent than the optitrust library before reactivating
#if [[ "${FILEBASE}.ml" -nt "${PROG}" ]] || [[ "${FILEBASE}.cpp" -nt "${PROG}" ]]; then
  # echo FILE1 is newer than FILE2
  PROGNEEDSREBUILD="needsrebuild"
#fi

if [ "${RECOMPILE_OPTITRUST}" = "recompile_optitrust_yes" ] || [ "${PROGNEEDSREBUILD}" = "needsrebuild" ]; then
  rm -Rf _build/${PROG} ${PROG}
  ocamlbuild -use-ocamlfind -r -tags "package(optitrust)" ${PROG}
  ln -sf _build/${PROG} ${PROG}
fi

# LATER: capture the output error message
# so we can do the postprocessing on it

OUT=$?
if [ ${OUT} -ne 0 ];then
  echo "Error: could not compile the program ${PROG}"  >> /dev/stderr
  exit 1
fi


# Third, we execute the transformation program, obtain "${FILEBASE}_before.cpp" and "${FILEBASE}_after.cpp, unless mode is view_trace

if [ "${VIEW}" = "view_trace" ]; then
  OPTIONS="${OPTIONS} -dump-trace ${TRACEFLAGS}"
else
  OPTIONS="${OPTIONS} -exit-line ${LINE}"
  if [ "${VIEW}" = "view_diff_enc" ]; then
    OPTIONS="${OPTIONS} -dump-ast-details"
  fi
fi

# For that run, we activate the backtrace
echo "OCAMLRUNPARAM=b ${RUNNER} ${PROG} ${OPTIONS} ${FLAGS}"
OCAMLRUNPARAM=b ${RUNNER} ${PROG} ${OPTIONS} ${FLAGS}

OUT=$?
if [ ${OUT} -ne 0 ];then
    exit 1
fi

# Fourth, we vizualize a result or a diff or a trace

if [ "${VIEW}" = "view_diff" ] || [ "${VIEW}" = "view_diff_enc" ]; then

  if [ "${VIEW}" = "view_diff_enc" ]; then
    DIFFFOR="enc"
  fi

  ${TOOLS_FOLDER}/open_diff.sh ${FILEBASE} cpp ${DIFFFOR}

elif [ "${VIEW}" = "view_result" ]; then

  ${TOOLS_FOLDER}/open_result.sh ${FILEBASE}

elif [ "${VIEW}" = "view_trace" ]; then

  ${TOOLS_FOLDER}/open_trace.sh ${FILEBASE}

else

  echo "invalid VIEW argument" >> /dev/stderr
  exit 1

fi

exit
