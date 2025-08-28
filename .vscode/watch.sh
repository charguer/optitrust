#!/bin/bash

# This script watches over for any modification of ACTION_FILE
# and executes the file when it gets modified.
# Requires the `inotify-tools` package.
# you must run it, to be able to triggers tasks like view diff
# Assumes it is called from the .vscode folder

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

ACTION_FILE="./action.sh"
ACTION_OUT_TEMP="./action_out_temp.txt"
ACTION_OUT="./action_out.txt"

cd ${SCRIPT_DIR}

echo "Will be watching for commands produced by run_action.sh into the file:"
echo "   ${SCRIPT_DIR}/${ACTION_FILE}"
echo "Type CTRL+C or close current window to terminate the watcher."
echo "Now waiting for next command..."

while true; do
    rm -f ${ACTION_FILE} # optional
    touch ${ACTION_FILE}
    chmod +x ${ACTION_FILE}
    inotifywait -q -e modify ${ACTION_FILE}
    sleep 0.01
    OUT=$?
    if [ $OUT -eq 0 ];then
       echo "Action to perform:"
       cat ${ACTION_FILE}
       rm -f ${ACTION_OUT_TEMP}
       # Execute the action, and save the output
       # TODO Arthur : remove this when fix run action
       cd ..
       ./.vscode/${ACTION_FILE} > ./.vscode/${ACTION_OUT_TEMP} 2>&1
       mv ./.vscode/${ACTION_OUT_TEMP} ./.vscode/${ACTION_OUT} -f
      cd .vscode
    else
       exit
    fi
done
