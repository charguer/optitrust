#!/bin/bash
# Assumes the current path to be .vscode

# echo `pwd`
# echo "in run_action"
# echo "with args $*"

echo "$*" > ./action.sh

# TODO: rm action_out.txt; have the process wait until action_out.txt is created; 
# then at this time, cat this file.
#  can use a while loop with a sleep to wait for the change to exist with if -f.
