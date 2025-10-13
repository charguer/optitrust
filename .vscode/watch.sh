#!/bin/bash
#
# This script plays the role of a 'server' for executing queries issued
# by the script `run_action.sh`. This script executes until interrupted.
# It must be launched in OptiTrust's root folder.
#
# Usage: (from OptiTrust's root folder)
#      .vscode/watch.sh
#
# Or simply use the shorthand:
#      ./watcher.sh
#
# Concretely, this script watches over for any modification of ACTION_FILE
# and executes the file when it gets modified.
# Using this file requires the `inotify-tools` package to be installed.

# Compute paths
OPTITRUST_DIR=`pwd`
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
ACTION_FILE="${SCRIPT_DIR}/action.sh"
ACTION_OUT_TEMP="${SCRIPT_DIR}/action_out_temp.txt"
ACTION_OUT="${SCRIPT_DIR}/action_out.txt"

echo "Will be watching for commands produced by run_action.sh into the file:"
echo "   ${ACTION_FILE}"
echo "Type CTRL+C or close current window to terminate the watcher."
echo "Now waiting for next command..."

while true; do
    # Create the file "action.sh" used to receive the commands to execute
    rm -f ${ACTION_FILE} # optional
    touch ${ACTION_FILE}
    chmod +x ${ACTION_FILE}
    # Watch for changes to this file
    inotifywait -q -e modify ${ACTION_FILE}
    sleep 0.01
    OUT=$?
    if [ $OUT -eq 0 ];then
       # Print the command to execute
       echo "Action to perform:"
       cat ${ACTION_FILE}
       # Clear the temporary file in which to capture the output of the command
       rm -f ${ACTION_OUT_TEMP}
       # Execute the action
       ${ACTION_FILE} > ${ACTION_OUT_TEMP} 2>&1
       # Copy the output of the command into the "action_out.txt" file.
       mv ${ACTION_OUT_TEMP} ${ACTION_OUT} -f
       echo "Action complete"
    else
       exit
    fi
done
