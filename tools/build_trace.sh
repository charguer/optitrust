#!/bin/bash

TOOLS_FOLDER=$(dirname -- "$( readlink -f -- "$0"; )")
TOOLS_FOLDER=$(realpath --relative-to=. ${TOOLS_FOLDER})
WEB_VIEW_FOLDER="${TOOLS_FOLDER}/web_view"

FILEBASE=$1

TARGET="${FILEBASE}_trace.html"

#echo "Generating ${TARGET}"

# Compute title
TITLESTR="${FILEBASE} - OptiTrust Trace"

# Compute trace js file name
BASENODIR=$(basename ${FILEBASE})
TRACEJSFILE="./${BASENODIR}_trace.js"

TEMPLATE="${WEB_VIEW_FOLDER}/trace_template.html"

cp ${TEMPLATE} ${TARGET}

sed -i "s#{INSERT_TITLE}#${TITLESTR}#g
s#{WEB_VIEW_FOLDER}#${WEB_VIEW_FOLDER}#g
s#{TRACEJSFILE}#${TRACEJSFILE}#g" ${TARGET}
# Note: there seems to be an issue if performing the sed one after the other...
