#!/bin/bash
# Assumes the current path to be .vscode

# echo `pwd`
# echo "in run_action"
# echo "with args $*"

ACTION_OUT="./action_out.txt"
ACTION_FILE="./action.sh"

# Clear the output log file
rm -f ${ACTION_OUT}
touch ${ACTION_OUT}

# Request watch.sh to execute the command by writing the command in action.sh
echo "$*" > ${ACTION_FILE}

# Watch for a change in the output file
inotifywait -q -e modify ${ACTION_OUT}

# When we get some change, display the contents of the log file
sleep 0.01
cat ${ACTION_OUT}



