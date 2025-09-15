#!/usr/bin/env bash
#
# This script is for executing a command that opens a GUI process.
# - it relies on `run_action.sh` to escape limitations from VScode sandbox
# - it relies on nohup to make the GUI process run persistently.
#
# Note that `run_action.sh` depends on `watch.sh` being running in a separate
# terminal to successfully escape the sandbox.
#
# Usage:
#   .vscode/exec_with_nohup_and_runaction.sh ${CMD}
#
# where ${CMD} is the command to be executed for opening the desired GUI

DOTVSCODEFOLDER=$(dirname -- "$( readlink -f -- "$0"; )")
CMD="$*"

${DOTVSCODEFOLDER}/run_action.sh ${DOTVSCODEFOLDER}/exec_with_nohup.sh ${CMD}


