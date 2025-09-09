#!/bin/bash
# Assumes the current path to be .vscode

# -- for debug:
# echo `pwd`
# echo "in run_action"
# echo "with args $*"

ACTION="$*"

# Check that the watcher is running.
# Else, issue a warning and attempt to execute the command directly.
if ! pgrep -f "watch.sh" > /dev/null; then
  echo "WARNING: the watcher is not running. Open a separate terminal and in the optitrust folder execute `watcher.sh`."
  echo "  As a fallback, trying to run the command directly."
  echo "  However, due to sandboxing, the command may not work fully as expected".
  echo "  Running command: ${ACTION}"
  ${ACTION}
  exit
fi

# Compute the paths to the files used for communiction
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
ACTION_OUT="${SCRIPT_DIR}/action_out.txt"
ACTION_FILE="${SCRIPT_DIR}/action.sh"

# Clear the output log file
rm -f ${ACTION_OUT}
touch ${ACTION_OUT}

# Request watch.sh to execute the command by writing the command in action.sh
echo "Request in ${ACTION_FILE} execution of the command:"
echo "  ${ACTION}"
echo "${ACTION}" > ${ACTION_FILE}

# Watch for a change in the output file
inotifywait -q -e modify ${ACTION_OUT}

# When we get some change, display the contents of the log file
sleep 0.01
cat ${ACTION_OUT}
