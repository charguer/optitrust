#!/usr/bin/env bash

# This script builds a '_trace.html' file for a given a '.ml' script using `build_trace.sh`.
# Unlike `open_trace.sh`, here the trace produced is a standalone file that does not
# depend on an interactive server.
#
# The script then opens the trace in a browser using `open_in_browser.sh`.
# If the script is called from VScode, you might need `watch.sh` to get the browser to show up.
#
# Usage: (This script must be executed with a `pwd` matching the folder that contains the script)
#   ${OPTITRUST_FOLDER}/tools/open_standalone_trace.sh ${FILEBASE}
# where ${FILEBASE} is the script name without extension, e.g., label_add.
# The output is named, e.g. `label_add_standalone_trace.html`.

TOOLS_FOLDER=$(dirname -- "$( readlink -f -- "$0"; )")
OPTITRUST_FOLDER=$(dirname "${TOOLS_FOLDER}")
SCRIPT_FOLDER=`pwd`

FILEBASE=$1

# Name of the trace output file
TRACE_FILENAME="${FILEBASE}_standalone_trace.html"
TARGET=$(realpath --relative-to=${OPTITRUST_FOLDER} ${TRACE_FILENAME})

# Build the standalone html file
${TOOLS_FOLDER}/build_trace.sh ${FILEBASE} ${TRACE_FILENAME}
echo "Generated ${TARGET}"

# Open the browser with the target file
URL="${OPTITRUST_FOLDER}/${TARGET}"
${TOOLS_FOLDER}/open_in_browser.sh ${URL} "${FILEBASE} - OptiTrust Standalone Trace"
