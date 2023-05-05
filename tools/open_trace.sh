#!/bin/bash

TOOLS_FOLDER=$(dirname -- "$( readlink -f -- "$0"; )")
WEB_VIEW_FOLDER="${TOOLS_FOLDER}/web_view"

FILEBASE=$1

TARGET="${FILEBASE}_trace.html"

#echo "Generating ${TARGET}"

# Compute title
TITLESTR="${FILEBASE} - OptiTrust Trace"

# Compute trace js file name
TRACEJSFILE="./${FILEBASE}_trace.js"

TEMPLATE="${WEB_VIEW_FOLDER}/trace_template.html"

cp ${TEMPLATE} ${TARGET}

sed -i "s#{INSERT_TITLE}#${TITLESTR}#g
s#{WEB_VIEW_FOLDER}#${WEB_VIEW_FOLDER}#g
s#{TRACEJSFILE}#${TRACEJSFILE}#g" ${TARGET}
# Note: there seems to be an issue if performing the sed one after the other...

echo "Generated ${TARGET}"

# Open the browser with the target file
${TOOLS_FOLDER}/open_in_browser.sh ${TARGET} "Trace for ${FILEBASE}"
