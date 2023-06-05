#!/bin/bash
set -euo pipefail

# TODO FOR ARTHUR: take as argument FILENAME, then deduce FILEBASE AND FILEEXT
# AND if we're calling a cpp file, then don't add_exit

DIRNAME=$1
FILEBASE=$2
LINE=$3
VIEW=$4 # should be view_diff or view_result or view_trace
OPTIONS="${@:5}"
${FLAGS:=""}

# TODO: what are TRACEFLAGS for?
${TRACEFLAGS:=""}

# LATER: if a ${FILEBASE}_exp.cpp file is present, export it into the JS file,
# so that the browser can report on the differences between _out.cpp and _exp.cpp.

# Path to tools folder and src folder
TOOLS_FOLDER=$(dirname -- "$(readlink -f -- "$0";)")
SRC_FOLDER=$(readlink -f ${TOOLS_FOLDER}/../src)

# Disallow execution in the src folder
if [[ ${DIRNAME}/ == ${SRC_FOLDER}/* ]]
then
    echo "Cannot start view_result.sh in the src folder" >&2
    exit 2
fi

# Save arguments for redo command
echo "${TOOLS_FOLDER}/view_result.sh $*" > "${TOOLS_FOLDER}/_last_view_result.sh"
chmod +x "${TOOLS_FOLDER}/_last_view_result.sh"

# This can help with opam switches
eval $(opam env)

# Make sure we work in the directory that contains the file
cd ${DIRNAME}

# Read options from optitrust_flags.sh
# options: e.g., FLAGS="-dump-ast-details -analyse-stats-details -debug-reparse"
if [ -f "optitrust_flags.sh" ]; then
  source optitrust_flags.sh
fi

# From "${FILEBASE}.ml", create ""{FILEBASE}_with_lines.ml" by inserting
# [~lines:__LINE__]   in the relevant places, and interpreting '!!' and '!!!'
${TOOLS_FOLDER}/add_lines.sh ${FILEBASE}.ml ${FILEBASE}_with_lines.ml
# DEBUG: cat "${FILEBASE}_with_lines.ml"; exit 0

# LATER: add_exit should also introduce special commands for figuring out the line of the command that executes

${TOOLS_FOLDER}/build_cmxs.sh ${FILEBASE}_with_lines.ml
PROG="${FILEBASE}_with_lines.cmxs"

# Third, we execute the transformation program, obtain "${FILEBASE}_before.cpp" and "${FILEBASE}_after.cpp, unless mode is view_trace

if [ "${VIEW}" = "view_trace" ]; then
  OPTIONS="${OPTIONS} -dump-trace ${TRACEFLAGS}"
else
  OPTIONS="${OPTIONS} -exit-line ${LINE}"
  if [ "${VIEW}" = "view_diff_enc" ]; then
    OPTIONS="${OPTIONS} -dump-ast-details"
  fi
fi

# FIXME: Will not work if we are outside the optitrust buildtree either
OCAMLRUNPARAM=b dune exec --no-build optitrust_runner -- ${PROG} ${OPTIONS} ${FLAGS}

# Fourth, we vizualize a result or a diff or a trace

if [ "${VIEW}" = "view_diff" ] || [ "${VIEW}" = "view_diff_enc" ]; then

  DIFFFOR=""
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
