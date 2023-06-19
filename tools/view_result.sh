#!/bin/bash

# Usage:
#   ./tools/view_result.sh ${DIRNAME} ${FILENAME} ${LINENUMBER} ${MODE}
#
# where MODE is one of:
# - view_diff: to view the diff at the targeted line
# - view_diff_enc: likewise, but disable the encoding
# - view_trace: view the full execution trace
# - save_inter: save the AST at the specified line
# - view_diff_from_inter: execute the script from the line specified in
#     the last call to this script performed in mode save_inter
# - view_result: DEPRECATED, but could be revived in the future

# Additional arguments are passed to the execution of the script
#
# Example usage:
#    ./tools/view_result.sh ./tests/interact interact_traceview 21 view_diff
#
# This script is meant to be called from VScode via a keybinding,
# via run_action.sh to enable the loading of a browser window.

# FIXME: the "dune exec" command used by this script will not work if
# the script file is located outside of the optitrust buildtree


#==========================================================================
# Processing script arguments

# OPTION: for printing timing info, uncomment the line
#export PRINTTIME="1"
${PRINTTIME:=""}

# Throughout the script, we measure execution times
TIMER1=`date +%s%3N`

# Setting to ensure that the execution is interrupted in case of error
set -euo pipefail

# Parsing arguments
DIRNAME=$1
FILEBASE=$2
LINE=$3
MODE=$4
OPTIONS="${@:5}"

# Additional environment variables. TRACEFLAGS are used only in view_trace mode.
${FLAGS:=""}
${TRACEFLAGS:=""}

# Path to the tools and optitrust folder
TOOLS_FOLDER=$(dirname -- "$(readlink -f -- "$0";)")
OPTITRUST_FOLDER="${TOOLS_FOLDER}/.."
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

#==========================================================================
# Setting up the environment, and read additional settings

# Make sure we work in the directory that contains the file
cd ${DIRNAME}

# This command can help with opam switches --LATER: this is not zero cost, remove it?
eval $(opam env)

# Read options from the local optitrust_flags.sh or the one at the OptiTrust root
# Example options: FLAGS="-dump-ast-details"
if [ -f "optitrust_flags.sh" ]; then
  source optitrust_flags.sh
fi
if [ -f "${OPTITRUST_FOLDER}/optitrust_flags.sh" ]; then
  source ${OPTITRUST_FOLDER}/optitrust_flags.sh
fi

if [ "${MODE}" = "view_trace" ]; then
  OPTIONS="${OPTIONS} -dump-trace ${TRACEFLAGS}"
else
  OPTIONS="${OPTIONS} -exit-line ${LINE}"
  if [ "${MODE}" = "view_diff_enc" ]; then
    OPTIONS="${OPTIONS} -dump-ast-details"
  fi
fi


#==========================================================================
# Patch the script if needed to handle intermediate states

TIMER2=`date +%s%3N`

# TODO: extract the logic of these two modes into separate files

if [ "${MODE}" = "save_inter" ]; then

  # In mode "save_inter", we generate "${FILEBASE}_inter.ml" as a trimmed version
  # of the "${FILEBASE}.ml" script, by removing all the lines after line ${LINE}.

  SRCBASE="${FILEBASE}_inter"

  LINESTOKEEP=$((LINE-1))
  # echo "LINESTOKEEP=${LINESTOKEEP}"
  head -${LINESTOKEEP} ${FILEBASE}.ml > ${SRCBASE}.ml
  echo "  !!(); )" >> ${SRCBASE}.ml

  # Patch the call to "script_cpp" so that it loads the right CPP file
  sed -i "s/script_cpp/script_cpp ~filename:\"${FILEBASE}.cpp\" ~prefix:\"${FILEBASE}_inter\"/;" ${SRCBASE}.ml

  echo "Produced ${SRCBASE}.ml"

elif [ "${MODE}" = "view_diff_from_inter" ]; then

  # In mode "view_diff_from_inter", we generate a file called "${FILEBASE}_fast.ml"
  # by taking "${FILEBASE}_inter.ml" and adding to it the lines from "${FILEBASE}.ml"
  # at the position after the line specified for the call with mode "save_inter".

  SRCBASE="${FILEBASE}_fast"

  # Check that the intermediate file exists
  if [ ! -f "${FILEBASE}_inter.ml" ]; then
    echo "Error: intermediate state ${FILEBASE}_inter.ml does not exist. Try saving a state first."  >> /dev/stderr
    exit 1
  fi

  # Compute the number of lines in the intermediate script
  LINE_INTER=`wc -l ${FILEBASE}_inter.ml | awk '{ print $1 }'`

  # Error message if requested line is prior to the saved point
  if [ "${LINE}" -lt "${LINE_INTER}" ]; then
    echo "Error: line ${LINE} is before the saved intermediate state at line ${LINE_INTER}"  >> /dev/stderr
    exit 1
  fi

  # Compute the line containing the call to "script_cpp"
  LINE_RUNSCRIPT=`grep -n "Run.script_cpp" ${FILEBASE}.ml | cut -d : -f 1`

  # Error message if lines are not found
  if [ -z "${LINE_RUNSCRIPT}" ] || [ -z "${LINE_INTER}" ]; then
    echo "Error: cannot use intermediate state"  >> /dev/stderr
    exit 1
  fi

  # Make the lines blank in the script on the prefix that produces the intermediate state
  LINE_START=$((LINE_RUNSCRIPT+1))
  LINE_STOP=$((LINE_INTER-1))

  echo "Using cache up to line ${LINE_STOP}" # from ${LINE_START}
  sed "${LINE_START}","${LINE_STOP}"'{s/^.*$/  /;}' ${FILEBASE}.ml > ${SRCBASE}.ml

  # Patch the call to "script_cpp" so that it loads the saved intermediate CPP file
  sed -i "s/script_cpp/script_cpp ~filename:\"${FILEBASE}_inter_before.cpp\" ~prefix:\"${FILEBASE}_fast\"/;" ${SRCBASE}.ml

  # echo "Produced ${SRCBASE}.ml"

else

  # In other modes, we don't need to generate a patched source file
  SRCBASE="${FILEBASE}"

fi

#==========================================================================
# Generate the script instrumented with line numbers

TIMER3=`date +%s%3N`

# From "${SRCBASE}.ml", we create ""{SRCBASE}_with_lines.ml" by inserting
# the line numbers in the relevant places. See documentation in add_lines.sh.
${TOOLS_FOLDER}/add_lines.sh ${SRCBASE}.ml ${SRCBASE}_with_lines.ml

PROG="${SRCBASE}_with_lines.cmxs"

#==========================================================================
# Check the dependencies
# We need a rebuild of the cmx file if any src/ file is more recent
# than the .ml script

TIMER4=`date +%s%3N`

LAST_MODIF_LIB=`find ${OPTITRUST_FOLDER}/src -name "*.ml" -type f -printf '%T@ %p\n' | sort -n | tail -1 | cut -f2-2 -d" "`

NEEDS_REBUILD="0"

if [ ${LAST_MODIF_LIB} -nt ${PROG} ]; then
  echo "Compiled cmx is out of date compared with ${LAST_MODIF_LIB}"
  NEEDS_REBUILD="1"
elif [ ${FILEBASE}.ml -nt ${PROG} ]; then
  echo "Compiled cmx is out of date compared with ${FILEBASE}.ml"
  NEEDS_REBUILD="1"
fi


#==========================================================================
# Compile the script

TIMER5=`date +%s%3N`

if [ "${NEEDS_REBUILD}" = "1" ]; then
   echo "Building cmxs file"
  ${TOOLS_FOLDER}/build_cmxs.sh ${SRCBASE}_with_lines.ml
else
   echo "Skipping build of cmxs file"
fi


#==========================================================================
# Execute the script

TIMER6=`date +%s%3N`

# if [ ! -z ${OPTIONS} ]; then
echo "Execution options: ${OPTIONS}"

# TODO: --no-build
OCAMLRUNPARAM=b dune exec optitrust_runner -- ${PROG} ${OPTIONS} ${FLAGS}

#==========================================================================
# Open the output

TIMER7=`date +%s%3N`

if [ "${MODE}" = "view_diff" ] || [ "${MODE}" = "view_diff_from_inter" ] || [ "${MODE}" = "view_diff_enc" ]; then

  ${TOOLS_FOLDER}/open_diff.sh ${SRCBASE} cpp ${MODE}

elif [ "${MODE}" = "view_result" ]; then

  ${TOOLS_FOLDER}/open_result.sh ${SRCBASE}

elif [ "${MODE}" = "view_trace" ]; then

  ${TOOLS_FOLDER}/open_trace.sh ${SRCBASE}

elif [ "${MODE}" = "save_inter" ]; then

  echo "Produced ${SRCBASE}_before.cpp as checkpoint for line ${LINE}"

else

  echo "invalid MODE argument" >> /dev/stderr
  exit 1

fi

#==========================================================================
# Report on execution time

TIMER8=`date +%s%3N`

if [ ! -z "${PRINTTIME}" ]; then
  echo "Time process args: $((${TIMER2}-${TIMER1}))ms"
  echo "Time gen checkpoints: $((${TIMER3}-${TIMER2}))ms"
  echo "Time gen with-lines: $((${TIMER4}-${TIMER3}))ms"
  echo "Time dependencies:  $((${TIMER5}-${TIMER4}))ms"
  echo "Time build cmx: $((${TIMER6}-${TIMER5}))ms"
  echo "Time dune exec: $((${TIMER7}-${TIMER6}))ms"
  echo "Time open result: $((${TIMER8}-${TIMER7}))ms"
  echo "Time total: $((${TIMER8}-${TIMER1}))ms"
fi
