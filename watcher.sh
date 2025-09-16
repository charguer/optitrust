#!/bin/bash

# Launch the "watcher" script in the current terminal.
# The terminal should be left open, on the side.
# The purpose of the "watcher" is to execute tasks from VSCode
# outside of the VSCode sandbox, to avoid technical limitations.
# For that purpose VSCode tasks go through a call to "run_action.sh".

.vscode/watch.sh

