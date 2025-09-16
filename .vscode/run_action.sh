#!/bin/bash
#
# This script issues 'queries' to be executed by the 'server' script `watch.sh`.
# Each query consists of a command line to execute.
#
# Usage:
#    ./run_action.sh ${CMD}
#
# where ${CMD} denotes the command to run.
# The command must makes sense when executed in the root folder of OptiTrust.
# Indeed, the `watch.sh` script, which launches the actions, is executed in that folder.


# -- for debug:
# echo `pwd`
# echo "in run_action"
# echo "with args $*"


# Compute the paths to the files used for communiction
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
ACTION_OUT="${SCRIPT_DIR}/action_out.txt"
ACTION_FILE="${SCRIPT_DIR}/action.sh"
ACTION="$*"

# Check that the watcher is running.
# Else, issue a warning and attempt to execute the command directly.
if ! pgrep -f "watch.sh" > /dev/null; then
  echo "WARNING: the watcher is not running."
  echo "  Open a separate terminal and in the optitrust folder execute 'watcher.sh'."
  echo "  As a fallback, now trying to run the command directly."
  echo "  However, due to sandboxing, the command may not work fully as expected".
  echo "  Running command: ${ACTION}"
  ${ACTION}
  exit
fi

# Clear the output log file
rm -f ${ACTION_OUT}
touch ${ACTION_OUT}

# Request watch.sh to execute the command by writing the command in action.sh
echo "Run_action executes:"
echo "  ${ACTION}"
# echo "  and wait for the output in file ${ACTION_OUT}"
echo "${ACTION}" > ${ACTION_FILE}

# Watch for a change in the output file
inotifywait -q -e modify ${ACTION_OUT}

# When we get some change, display the contents of the log file
sleep 0.01
cat ${ACTION_OUT}
