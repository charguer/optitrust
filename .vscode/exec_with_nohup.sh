#!/usr/bin/env bash
#
# This script is for executing a command that opens a GUI process,
# by wrapping it in a call to [nohup].
#
# Usage:
#   .vscode/exec_with_nohup.sh ${CMD}
#
# where ${CMD} is the command to be executed for opening the desired GUI

DOTVSCODEFOLDER=$(dirname -- "$( readlink -f -- "$0"; )")
CMD="$*"

nohup ${CMD} >/dev/null 2>&1 &
