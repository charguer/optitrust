#!/usr/bin/env bash

# This script builds a html trace file for a given a '.ml' script, and a specified output filename.
# It is used by the scripts `open_trace.sh` and `open_standalone_trace.sh`.
#
# Usage: (This script must be executed with a `pwd` matching the folder that contains the script)
#   ${OPTITRUST_FOLDER}/tools/open_standalone_trace.sh ${FILEBASE} ${TRACE_FILENAME}
# where ${FILEBASE} is the script name without extension, e.g., label_add.


# TOOLS_FOLDER_RELATIVE denotes the path to the tools folder relative to the current script
TOOLS_FOLDER=$(dirname -- "$( readlink -f -- "$0"; )")
TOOLS_FOLDER_RELATIVE=$(realpath --relative-to=. ${TOOLS_FOLDER})

# WEB_VIEW_FOLDER denotes the path to the web_view folder relative to the script folder
WEB_VIEW_FOLDER="${TOOLS_FOLDER_RELATIVE}/web_view"

OPTITRUST_FOLDER=$(dirname "${TOOLS_FOLDER}")

SCRIPT_FOLDER=`pwd`
SCRIPT_FOLDER_RELATIVE=$(realpath --relative-to=${OPTITRUST_FOLDER} ${SCRIPT_FOLDER})

FILEBASE=$1
TARGET=$2

#echo "Generating ${TARGET}"

# Compute title
TITLESTR="OptiTrust: ${SCRIPT_FOLDER_RELATIVE}/${TARGET}"

# Compute trace js file name
BASENODIR=$(basename ${FILEBASE})
TRACEJSFILE="./${BASENODIR}_trace.js"

TEMPLATE="${WEB_VIEW_FOLDER}/trace_template.html"

cp ${TEMPLATE} ${TARGET}

sed -i "s#{INSERT_TITLE}#${TITLESTR}#g
s#{WEB_VIEW_FOLDER}#${WEB_VIEW_FOLDER}#g
s#{TRACEJSFILE}#${TRACEJSFILE}#g" ${TARGET}
# Note: there seems to be an issue if performing the sed one after the other, hence doing them all at once

echo "Generated ${TARGET}"
