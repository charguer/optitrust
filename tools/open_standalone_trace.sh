#!/bin/bash

TOOLS_FOLDER=$(dirname -- "$( readlink -f -- "$0"; )")

FILEBASE=$1

TARGET="${FILEBASE}_trace.html"

# Build the standalone html file
${TOOLS_FOLDER}/build_trace.sh ${FILEBASE}
echo "Generated ${TARGET}"

# Open the browser with the target file
${TOOLS_FOLDER}/open_in_browser.sh ${TARGET} "${FILEBASE} - OptiTrust Trace"
