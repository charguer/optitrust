#!/bin/bash
# Assumes the current path to be .vscode

# echo `pwd`
# echo "in run_action"
# echo "with args $*"

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

ACTION_OUT="${SCRIPT_DIR}/action_out.txt"
ACTION_FILE="${SCRIPT_DIR}/action.sh"

# Clear the output log file
rm -f ${ACTION_OUT}
touch ${ACTION_OUT}

# Request watch.sh to execute the command by writing the command in action.sh
echo "Request in ${ACTION_FILE} execution of the command:"
echo "  $*"
echo "$*" > ${ACTION_FILE}

# Watch for a change in the output file
inotifywait -q -e modify ${ACTION_OUT}

# When we get some change, display the contents of the log file
sleep 0.01
cat ${ACTION_OUT}
