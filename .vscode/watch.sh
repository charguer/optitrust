
#!/bin/bash
# This script watches over for any modification of ACTION_FILE
# and executes the file when it gets modified.
# Requires the `inotify-tools` package.
# Assumes it is called from the .vscode folder

echo "start watching in folder"
echo $(pwd)

ACTION_FILE="./action.sh"
while true; do
    inotifywait -e modify ${ACTION_FILE}
    sleep 0.01
    OUT=$?
    if [ $OUT -eq 0 ];then
       echo "Action to perform:"
       cat ${ACTION_FILE}
       ${ACTION_FILE}
       # TODO: redirect stdout and stderr to a file called action_out_temp.txt
       # TODO: then at the end do mv action_out_temp.txt to action_out.txt
    else
       exit
    fi
done
