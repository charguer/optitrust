#!/bin/bash

# This script watches over for any modification of ACTION_FILE
# and executes the file when it gets modified.
# Requires the `inotify-tools` package.
# Assumes it is called from the .vscode folder



SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
echo "Start watching in folder:"
echo ${SCRIPT_DIR}


cd ${SCRIPT_DIR}
ACTION_FILE="./action.sh"
ACTION_OUT_TEMP="./action_out_temp.txt"
ACTION_OUT="./action_out.txt"

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
       ${ACTION_FILE} > ${ACTION_OUT_TEMP} 2>&1
       mv ${ACTION_OUT_TEMP} ${ACTION_OUT}
    else
       exit
    fi
done
