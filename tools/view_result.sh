#!/bin/bash

# Usage:
#   ./tools/view_result.sh ${MODE} ${FILEPATH} ${LINENUMBER} ${OPTIONS}
#
# where MODE is one of:
# - step_diff: view the diff at the targeted line
# - step_result: view the result after executing the targeted line
# - step_trace: view the trace at the targeted line
# - full_trace: view the full execution trace
# - standalone_full_trace: view the full execution trace as a standalone webpage
# - save_inter: save the AST at the specified line
# - step_diff_from_inter: execute the script from the line specified in
#     the last call to this script performed in mode save_inter
#  FUTURE USE:
# - full_trace_from_inter

# Additional arguments are passed to the execution of the script
#
# Example usage:
#    ./tools/view_result.sh step_diff ./tests/interact/interact_traceview.ml 21
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
MODE=$1
FILEPATH=$2
LINE=$3
OPTIONS="${@:4}"

DIRNAME=$(dirname $FILEPATH)
FILEBASE=$(basename $FILEPATH .ml)

# Additional environment variables.
${FLAGS:=""}
${CODE_VIEWER:="code -r"}

# Path to the tools and optitrust folder
TOOLS_FOLDER=$(dirname -- "$(readlink -f -- "$0";)")
export OPTITRUST_FOLDER=$(dirname "${TOOLS_FOLDER}")
SRC_FOLDER=$(readlink -f ${OPTITRUST_FOLDER}/src)

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

# Add EXECUTION MODE

if [ "${MODE}" = "step_diff" ] || [ "${MODE}" = "step_diff_from_inter" ] || [ "${MODE}" = "step_result" ]; then

  OPTIONS="${OPTIONS} -mode step-diff"

elif [ "${MODE}" = "step_trace" ]; then

  OPTIONS="${OPTIONS} -mode step-trace"

elif [ "${MODE}" = "standalone_full_trace" ]; then

  OPTIONS="${OPTIONS} -mode standalone-full-trace"

elif [ "${MODE}" = "full_trace" ] || [ "${MODE}" = "full_trace_from_inter" ]; then

  OPTIONS="${OPTIONS} -mode full-trace"

elif [ "${MODE}" = "save_inter" ]; then

  # no execution performed, do nothing
  :

else

  echo "view_result.sh invalid MODE argument: ${MODE}" >> /dev/stderr
  exit 1
fi

# Add TARGET LINE
OPTIONS="${OPTIONS} -line ${LINE}"


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

elif [ "${MODE}" = "step_diff_from_inter" ] || [ "${MODE}" = "full_trace_from_inter" ]; then

  # In mode "step_diff_from_inter", we generate a file called "${FILEBASE}_fast.ml"
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
# Compile the script

TIMER5=`date +%s%3N`

${TOOLS_FOLDER}/build_cmxs.sh ${SRCBASE}.ml

#==========================================================================
# Execute the script

TIMER6=`date +%s%3N`

# if [ ! -z ${OPTIONS} ]; then
echo "Execution options: ${OPTIONS}"

# TODO: --no-build
OCAMLRUNPARAM=b dune exec optitrust_runner -- ${SRCBASE}.cmxs ${OPTIONS} ${FLAGS} || [[ "${MODE}" == *"trace"* ]]

#==========================================================================
# Open the output

TIMER7=`date +%s%3N`

if [ "${MODE}" = "step_diff" ] || [ "${MODE}" = "step_diff_from_inter" ]; then

  ${TOOLS_FOLDER}/open_diff.sh ${SRCBASE} cpp

elif [ "${MODE}" = "step_trace" ] || [ "${MODE}" = "standalone_full_trace" ]; then

  ${TOOLS_FOLDER}/open_standalone_trace.sh ${SRCBASE}

elif [ "${MODE}" = "full_trace" ] || [ "${MODE}" = "full_trace_from_inter" ]; then

  ${TOOLS_FOLDER}/open_trace.sh ${SRCBASE}

elif [ "${MODE}" = "save_inter" ]; then

  echo "Produced ${SRCBASE}_before.cpp as checkpoint for line ${LINE}"

elif [ "${MODE}" = "step_result" ]; then

  ${CODE_VIEWER} ${SRCBASE}_after.cpp

else

  echo "view_result.sh invalid MODE argument: ${MODE}" >> /dev/stderr
  exit 1

fi

#==========================================================================
# Report on execution time

TIMER8=`date +%s%3N`

if false && [ ! -z "${PRINTTIME}" ]; then
  echo "Time process args: $((${TIMER2}-${TIMER1}))ms"
  echo "Time gen checkpoints: $((${TIMER3}-${TIMER2}))ms"
  echo "Time gen with-lines: $((${TIMER4}-${TIMER3}))ms"
  echo "Time dependencies:  $((${TIMER5}-${TIMER4}))ms"
  echo "Time build cmx: $((${TIMER6}-${TIMER5}))ms"
  echo "Time dune exec: $((${TIMER7}-${TIMER6}))ms"
  echo "Time open result: $((${TIMER8}-${TIMER7}))ms"
  echo "Time total: $((${TIMER8}-${TIMER1}))ms"
fi
